

#' Création modèle prédiction des essences des couronnes
#'
#' @param nom_ref nom du jeu de calibration ou NULL pour choix interactif
#' @param path_cal NULL ou chemin des couronnes de calibration créés avec md_ref_crowns_params()
#' @param dest NULL
#'
#' @return
#' @export
#'

md_model_ess_crowns <- function(
    nom_ref = NULL,
    path_cal = NULL,
    dest = NULL

){

  if(is.null(path_cal)){
    if(is.null(nom_ref)){

      nms <- list.files(dc("dos_modeles_oiseau"), pattern = "crown_") %>%
        stringr::str_remove_all("crown_") %>% tools::file_path_sans_ext()

      nom_ref <- utils::select.list(nms, title = "Choisissez un jeu de couronnes de référence :")

    }
    path_cal = file.path(dc("dos_modeles_oiseau"), paste0("crown_", nom_ref, ".rds"))
  }



  cal0 <- readRDS(path_cal) %>% dplyr::mutate(id = rownames(.))

  # suppression des dominés

  domine <- cal0 %>% filter(hmoy10>mnh_h_q90)

  cal0 <- cal0 %>% filter(hmoy10 < mnh_h_q90)

  # bdforet IGN

  path_bdforet_loc <- file.path(dirname(path_cal), paste0("bdforet_", nom_ref, ".rds"))

  if(file.exists(path_bdforet_loc)){

    comp <- readRDS(path_bdforet_loc)

  }else{

    bdforet <- sf::read_sf("/var/partage/oiseau/ifn/bdforet/FORMATION_VEGETALE.shp")


    bdloc <- bdforet %>% sf::st_crop(cal0 %>% st_bbox() %>% sf::st_as_sfc()) %>% sf::st_buffer(50)


    bdloc <- bdloc %>% dplyr::mutate(bdforet = case_when(
      startsWith(ESSENCE, "Pin") ~ "pin",
      ESSENCE %in% c("Conifères", "Douglas", "Sapin, épicéa") ~ "résineux",
      ESSENCE == "Mixte" ~"mixte",
      ESSENCE == "NC" ~ "nc",
      TRUE ~ "feuillu"
    ))

    comp <- sf::st_intersection(bdloc %>% dplyr::select(bdforet),
                                cal0 %>% sf::st_centroid() %>% dplyr::select(id), tolerance = 0)

    comp <- comp %>% filter(!duplicated(id))

    saveRDS(comp, path_bdforet_loc)
  }

  cal0 <- cal0 %>% left_join(comp %>% as.data.frame() %>% select(-geometry))

  cal0 <- cal0 %>% as.data.frame() %>%
    select(-c(id, geometry)) %>%
    mutate(essence = as.factor(essence),
           bdforet = as.factor(bdforet))

  cal <- cal0 %>% select(-any_of(c("id", "X", "Y", "area", "n", "g", "hmoy10"))) %>%
    relocate(essence)


  (na_x <- purrr::map_int(names(cal), ~sum(is.na(cal[[.x]]))) / nrow(cal))
  (cl_x <- purrr::map_chr(names(cal), ~paste(class(cal[[.x]]), collapse = "+")))

  co <- names(cal)

  exclu <- c("mnh_aspect", "flowdir", "mnh_TPI", "spot_.*_q", "moy", "topo_.*q", "topo_.*sd", ".*_x_.*q", ".*_x_.*sd", "TRIrmsd", "TRIriley")

  coout <- purrr::map(exclu, ~ co[which(stringr::str_detect(co, .x))]) %>% unlist() %>% unique()
  co[which(! co %in% coout)]


  cal_ <- cal %>% na.omit() %>%
    select(-any_of(coout)) %>%
    as.data.frame() %>%
    select(-any_of(c("geometry", "azimut", "dist", "diam", "cycle", "G", "ess"))) %>%
    mutate(bdforet = as.factor(bdforet))


  ans_spot <- names(cal_)[which(startsWith(names(cal_), "spot"))] %>%
    str_remove("spot_") %>% str_split("_", simplify = TRUE) %>%
    as.numeric() %>% na.omit() %>% unique()

  nrm <- names(cal_)[grep(paste0("^((?!spot).)*_20.._"), names(cal_), perl = TRUE)]

  for(a in ans_spot){
  nrm <- nrm[which(!str_detect(nrm, as.character(a)))]
}

  # select

  message("suppression des variables avec années absentes de Spot:\n", paste(nrm, collapse = ", "))

  cal_ <- cal_ %>% select(-nrm)

  vrm <- md_rm_corr_var(cal_)

  message("suppression des variables hautement corrélées:\n", paste(vrm, collapse = ", "))

  cal_ <- cal_ %>% select(-vrm)

  cal_ <- cal_ %>% filter(mnh_h_q90 > dc("lim_h_perche"))

  cal_$bdforet <- NULL

#

  ess_res <- c("S.P", "EPC", "PIN", "A.R")

  cal_fam <- cal_ %>% mutate(
    essence = as.character(essence),
    essence = ifelse(essence %in% ess_res, "RES", "FEU"),
    essence = as.factor(essence))

  cal_res <- cal_ %>%
    filter(essence %in% ess_res) %>%
    mutate(essence = as.factor(essence))

  cal_het <- cal_ %>%
    filter(! essence %in% ess_res) %>%
    mutate(
      essence = as.character(essence),
      essence = ifelse(essence == "HET", "HET", "DIV"),
      essence = as.factor(essence))

  cal_div <- cal_ %>%
    filter(! essence %in% c("HET", ess_res)) %>%
    mutate(
      essence = as.factor(essence))

  # tests------------------------------------------

  cc <- cal_fam %>% select(- any_of("bdforet")) %>%
    mutate(essence = (essence == "RES"))

  # modelisation ----------------------------------

  ls_mod <- purrr::map(list(cal_fam, cal_res, cal_het, cal_div), function(xxx){

    message(crayon::green(paste(unique(xxx$essence), collapse = "+")))


    xxx$essence <- as.character(xxx$essence) %>% as.factor()  # suppr facteurs vides


    tab <- xxx
    col_y <- 'essence'
    md_new_model_rf(tab = xxx, col_y = "essence", pvalid = .2)


  })

  names(ls_mod) <- c("famille", "res", "het", "af")

  path_rf <- file.path(dirname(path_cal), stringr::str_replace(path_cal %>% basename(), "crown_", "rf_"))
  saveRDS(ls_mod, path_rf)



}
