

#' Création modèle prédiction des essences des couronnes
#'
#' @param path_cal chemin des couronnes de calibration créés avec md_ref_crowns_params()
#' @param dest NULL
#'
#' @return
#' @export
#'

md_model_ess_crowns <- function(path_cal, dest = NULL){

  # path_cal = "/var/partage/oiseau/modeles/crown_ess_ain_res1-spot2018.rds"

  bdforet <- sf::read_sf("/var/partage/oiseau/ifn/bdforet/FORMATION_VEGETALE.shp")

  cal0 <- readRDS(path_cal) %>% dplyr::mutate(id = rownames(.))


  bdloc <- bdforet %>% sf::st_crop(cal0 %>% st_bbox() %>% sf::st_as_sfc()) %>% sf::st_buffer(50)

  unique(bdloc$ESSENCE) %>% sort()

  bdloc <- bdloc %>% dplyr::mutate(bdforet = case_when(
    startsWith(ESSENCE, "Pin") ~ "pin",
    ESSENCE %in% c("Conifères", "Douglas", "Sapin, épicéa") ~ "résineux",
    ESSENCE == "Mixte" ~"mixte",
    ESSENCE == "NC" ~ "nc",
    TRUE ~ "feuillu"
  ))

  comp <- sf::st_intersection(bdloc %>% dplyr::select(ess),
                         cal0 %>% sf::st_centroid() %>% dplyr::select(id), tolerance = 0)

  comp <- comp %>% filter(!duplicated(id))

  cal0 <- cal0 %>% left_join(comp %>% as.data.frame() %>% select(-geometry))

  cal0 <- cal0 %>% as.data.frame() %>%
    select(-c(id, geometry)) %>%
    mutate(ess = as.factor(ess),
           bdforet = as.factor(bdforet))

  cal <- cal0[, c((which(names(cal0) == "topo_alti_moy")):(length(names(cal0))))]
  cal <- cal %>% select(-c(n, g.y)) %>%
    relocate(ess)


  (na_x <- purrr::map_int(names(cal), ~sum(is.na(cal[[.x]]))) / nrow(cal))
  (cl_x <- purrr::map_chr(names(cal), ~paste(class(cal[[.x]]), collapse = "+")))

  cal_ <- cal %>% na.omit() %>%
    select(-ends_with('moy')) %>%
    select(-(starts_with('topo_') & ! ends_with("med")))
  # select(-area)
  # select(c("ess", ends_with("med")))

  cal_gr <- cal_ %>% mutate(
    ess = as.character(ess),
    ess = ifelse(! ess %in% c("S.P", "EPC", "HET"), "DIV", ess),
    ess = as.factor(ess))

  cal_div <- cal_gr %>% mutate(div = cal_$ess) %>%
    filter(ess == "DIV") %>%
    mutate(ess = as.factor(as.character(div))) %>%
    select(-div)

  ls_mod <- purrr::map(list(cal_gr, cal_div), function(cal_x){

    sel <- varSelRF::varSelRF(xdata = cal_x %>% select(-ess), Class = cal_x$ess,
                              whole.range = TRUE)
    sv <- sel$selec.history
    sv$num <- 1:nrow(sv)
    plot(sv %>% select(Number.Variables, OOB), type = "n")
    text(sv$Number.Variables, sv$OOB, sv$num, offset = 1, col = "red")

    message(
      paste(sv$Number.Variables, paste0(round(sv$OOB*100), "%"), sep = ":", collapse = " ")
    )

    choix <- utils::select.list(sv$Number.Variables, title = "Sélectionner le nombre de variables à utiliser:")


    (vars <- sv$Vars.in.Forest[which(sv$Number.Variables == choix)] %>% as.character() %>% strsplit(" \\+ ") %>% unlist)

    model <- randomForest::randomForest(cal_x$ess ~ ., data = cal_x %>% select(vars),
                                        importance = TRUE)
    model
  })

  path_rf <- file.path(dirname(path_cal), stringr::str_replace(path_cal %>% basename(), "crown_", "rf_"))
  saveRDS(ls_mod[[1]],path_rf)


  path_rf_div <- stringr::str_replace(path_rf, "rf_ess_", "rf_essDIV_")
  saveRDS(ls_mod[[2]], path_rf_div)

}
