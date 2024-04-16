#' Métriques des houppiers des arbres des placettes permanentes
#'
#' @param nom_ref nom du jeu de données
#' @param placettes table des placettes permanentes, avec champ id = identifiant de la placette
#' @param arbres_ttl table des arbres avec champ de coordonnées polaire azimut (en grad) et dist (en m) et champs placette (id)
#' @param spec_date date des images spot à utiliser
#' @param dest_dir dossier d'écriture
#' @param user nom utilisateur
#' @param champ_essence de la table arbres
#' @param champ_num (vecteur) des champs de la table arbre à conserver
#' @param topo TRUE pour utiliser
#' @param dendro TRUE pour utiliser
#' @param spot TRUE pour utiliser
#' @param insol TRUE pour utiliser
#' @param mnh TRUE pour utiliser
#' @param sentinel TRUE pour utiliser les données sentinel
#' @param copernicus TRUE pour utiliser les données copernicus
#' @param test TRUE pour test sur 5 placettes
#' @param add NULL ou nom_ref du même jeu de crowns auquel ajouter les données extraites
#'
#' @return chemin du fichier d'écriture, écrit le fichier .rds des données de calibration
#' @export
#'

md_ref_crowns_params <- function(
    nom_ref = "ess_ain",
    placettes = dbOiseau::bdloc_read_table("placettes_permanentes_placettes") %>% filter(!is.na(date) & date > as.Date("2015-01-01")),
    arbres_ttl = dbOiseau::bdloc_read_table("placettes_permanentes_arbres") %>% # sans les morts
      filter(is.na(type)),
    champ_essence = "groupe",
    champ_diam = "diam1",
    spec_date = 2018,
    topo = TRUE, dendro = TRUE, spot = TRUE,
    insol = TRUE, mnh = TRUE,
    sentinel = FALSE,
    copernicus = TRUE,
    rsp_coper = FALSE,
    dest_dir = dc("dos_modeles_oiseau"),
    user = basename(dirname(dc("dos_projet"))),
    test = FALSE,
    add = NULL
){

  # Vérifications -----------------------------------------

  if(spot){
    message(crayon::red("ATTENTION: ------------------------------------------"))
    message(crayon::red("Les images SPOT de", spec_date, "seront utilisées"))
    message(crayon::red("-----------------------------------------------------"))
  }

  if((! "id" %in% names(arbres_ttl)) | (! "id" %in% names(placettes))){
    message(crayon::red("Erreur: le champ  'id' est obligatoire pour les tables placettes et arbres_ttl"))
    return("ko")
  }

  if(champ_essence %in% names(arbres_ttl)){

    if(champ_essence != "essence")
      arbres_ttl$essence <- arbres_ttl[[champ_essence]]

  }else{

    message(crayon::red("Erreur: le champ  'champ_essence' ", champ_essence, " n'existe pas dans la table arbres_ttl"))
    return("ko")
  }

  if(champ_diam %in% names(arbres_ttl)){

    if(champ_diam != "diam")
      arbres_ttl$diam <- arbres_ttl[[champ_diam]]

  }else{

    message(crayon::red("Erreur: le champ  'champ_diam' ", champ_diam, " n'existe pas dans la table arbres_ttl"))
    return("ko")
  }

  arbres_ttl <- arbres_ttl %>% select(all_of(c("id", "essence", "diam", "cycle", "azimut", "dist")))

  placettes <- placettes %>% select(id)

  grille_lidarhd <- read_sf(file.path(dc("dos_lidarhd"), "grille"))

  # Filtre des placettes disposant du LiDARHD ---------------------------------------

  # blocs dispo

  blc <- list.files(dc("dos_lidarhd"), pattern = "mnh.*tif") %>% strtrim(6) %>% stringr::str_remove(("mnh_"))

  aci <- sf::st_intersection(placettes, grille_lidarhd) %>%
    dplyr::filter(bloc %in% blc) %>% sf::st_make_valid()


  # Extraction par placette ---------------------------------------------------

  Sys.umask(0)

  fake_proj <- paste0("tmp_ref_", nom_ref)
  tmp_ref <- file.path(dest_dir, fake_proj)
  dir.create(tmp_ref)

  pb <- progress::progress_bar$new(total = length(1:(ifelse(test, 2, nrow(aci)))),
                                   format = "  downloading [:bar] :percent eta: :eta",
                                   clear = FALSE, width= 100)

  ls_cal <- purrr::map(1:(ifelse(test, 2, nrow(aci))),
                       function(.x){
                         pb$tick()

                         md.ref_crowns_params_placette(.x,
                         spec_date = spec_date, aci = aci, arbres_ttl = arbres_ttl, fake_proj = fake_proj,
                         grille_lidarhd = grille_lidarhd,
                         topo = topo, dendro = dendro, spot = spot, insol = insol, mnh = mnh,
                         sentinel = sentinel, copernicus = copernicus,
                         dest_pp = tmp_ref, rsp_coper = rsp_coper)
                       })



  # furrr::furrr_options(packages = c("oiseauAcces"))
  # future::plan("multisession")
  #
  # ls_cal <- furrr::future_map(1:(ifelse(test, 2, nrow(aci))),
  #                              md.ref_crowns_params_placette,
  #                    spec_date = spec_date, aci = aci, arbres_ttl = arbres_ttl, fake_proj = fake_proj,
  #                    grille_lidarhd = grille_lidarhd, sentinel = sentinel, copernicus = copernicus,
  #                    .progress = TRUE)
  #
  # future::plan("sequential")


  # ls_cal <- lapply(1:(ifelse(test, 5, nrow(aci))),
  #                              params_arbres_pp,
  #                              spec_date = spec_date)
  #
  # ls_cal <- purrr::map(ifelse(test, 1:5,1:nrow(aci)), ~tryCatch(
  #   {
  #     message("Placette ", .x, "/", ifelse(test, 1:5,1:nrow(aci)))
  #     params_arbres_pp(.x, spec_date = spec_date)
  #   },
  #   error = function(e){NULL}))

  # Regroupement des arbres toutes placettes ----------------------------------------

  ls_cal <- purrr::map(list.files(tmp_ref, full.names = TRUE), readRDS)

  cn <- purrr::map(ls_cal, names) %>% unlist() %>% unique()

  ls00 <- ls_cal[! purrr::map_lgl(ls_cal, is.null)]
  ls00 <- ls_cal[! purrr::map_lgl(ls_cal, function(x){nrow(x) == 0})]

  ls00 <- purrr::map(ls00, function(x){
    for(n in cn[which(! cn %in% names(x))]){
      x[[n]] <- NA
    }
    x
  })

  # les dates sentinel peuvent varier: regroupements

  # dates_sen <- purrr::map(ls00, ~ unique(
  #                           stringr::str_split(
  #                             names(.x), "_x_", simplify = TRUE)[,2]
  # ))
  #
  #
  # dates <- unique(unlist(dates_sen)) %>% sort()


  # ls000 <- purrr::map(ls00, ~ .x[, which(endsWith(names(.x), "01") | !stringr::str_detect(names(.x), "_x_"))])
  ls000 <- ls00

  cal <- do.call(rbind, ls000) #%>% select(any_of(c("groupe", names(cr))))


  # cn_ref <- c(names(placettes), names(arbres_ttl))
  # cal <- cal %>% select(-any_of(cn_ref[! cn_ref %in% c("id")]))

  # Enregistrement--------------------------------

  dest_name <- paste0("crown_", nom_ref,
                      "-res1-spot", paste(spec_date, collapse = "x"),
                      ifelse(rsp_coper, "rsp", ""))


  path <- file.path(dest_dir, paste0(dest_name, ".rds"))

  if(file.exists(path)){
    path <- file.path(dest_dir, paste0(dest_name, "-", stringr::str_replace_all(Sys.time(), "-", "_"), ".rds"))
  }

  saveRDS(cal, path)
  message("données de calibration sauvegardées sous ", path)

  # Fusion avec d'autres valeurs ------------------------------------

  if(!is.null(add)){

    path0 <- file.path(dest_dir, paste0("crown_", add, ".rds"))

    cal0 <- readRDS(path0)

    cal12 <- cal0 %>% select(-any_of(names(cal1))) %>% st_join(cal1)

    saveRDS(cal0, file.path(dest_dir, paste0("crown_", add, "_0.rds")))
    saveRDS(cal12, path0)

  }

  unlink(
    list.files(dc("dos_projet") %>% dirname(),
               recursive = FALSE, full.names = TRUE, include.dirs = TRUE,
               pattern = paste0(fake_proj, "[0-9]{6}")),
    recursive = TRUE
  )

  if(!is.null(add)){
    return(path0)
  }else{
    return(path)
  }
}
