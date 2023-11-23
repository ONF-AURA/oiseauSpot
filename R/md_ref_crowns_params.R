#' Métriques des houppiers des arbres des placettes permanentes
#'
#' @param nom_ref nom du jeu de données
#' @param placettes table des placettes permanentes, avec champ numdisp (identifiant réseau) et numplac (identifiant placette au sein du réseau)
#' @param arbres_ttl table des arbres avec champ de coordonnées polaire azimut (en grad) et dist (en m) et champs placette (numdisp et numpla)
#' @param spec_date date des images spot à utiliser
#' @param dest_dir dossier d'écriture
#' @param dest_name nom de fichier d'écriture
#' @param user nom utilisateur
#' @param fake_proj nom du projet fictif de travail à créer
#'
#' @return rien, écrit le fichier .rds des données de calibration
#' @export
#'

md_ref_crowns_params <- function(
    nom_ref = "ess_ain",
    placettes = dbOiseau::bdloc_read_table("placettes_permanentes_placettes") %>% filter(!is.na(date) & date > as.Date("2015-01-01")),
    arbres_ttl = dbOiseau::bdloc_read_table("placettes_permanentes_arbres") %>% # sans les morts
      filter(is.na(type)),
    spec_date = 2018,
    sentinel = FALSE,
    dest_dir = dc("dos_modeles_oiseau"),
    dest_name = paste0("crown_", nom_ref, "_res1-spot", spec_date),
    user = basename(dirname(dc("dos_projet"))),
    fake_proj = "tmp_ref_crowns"
){

  message(crayon::red("ATTENTION: ------------------------------------------"))
  message(crayon::red("Les images SPOT de", spec_date, "seront utilisées"))
  message(crayon::red("-----------------------------------------------------"))

  grille_lidarhd <- read_sf(file.path(dc("dos_lidarhd"), "grille"))

  # blocs dispo

  blc <- list.files(dc("dos_lidarhd"), pattern = "mnh.*tif") %>% strtrim(6) %>% stringr::str_remove(("mnh_"))

  aci <- sf::st_intersection(placettes, grille_lidarhd) %>%
    dplyr::mutate(bloc = url_telech %>% dirname() %>% basename()) %>%
    dplyr::filter(bloc %in% blc)



  # ttt <- data.frame(famille = as.character((1:4)),
  #                   X = 0, Y = 0,
  #                   azimut = c(0, 100, 200, 300),
  #                   dist = 1:4) %>%
  #   mutate( x = sin(azimut / 200 * pi) * dist + X,
  #           y = cos(azimut / 200 * pi) * dist + Y) %>%
  #   sf::st_as_sf(coords = c("x", "y"))

  # par placette ---------------------------------------------------


  params_arbres_pp <- function(i, spec_date){


    plac <- aci %>% slice(i)

    message("placette ", i, "...")

    coo_pla <- sf::st_centroid(plac) %>% sf::st_coordinates()



    # sf des arbres (points)
    arb <- arbres_ttl %>% dplyr::filter(numdisp == plac$numdisp & numplac== plac$numplac) %>%
      dplyr::filter(cycle == max(cycle) & !is.na(azimut) & !is.na(dist) & !is.na(diam)) %>%
      dplyr::mutate(X = coo_pla[1], Y = coo_pla[2],
                    x = sin(azimut / 200 * pi) * dist + X,
                    y = cos(azimut / 200 * pi) * dist + Y) %>%
      sf::st_as_sf(coords = c("x", "y"))
    sf::st_crs(arb) <- 2154

    # visu_leaflet_base() %>%
    #   addCircleMarkers(data = arb %>% sf::st_transform(4326),weight = 1,
    #                    color = rainbow(2)[as.numeric(as.factor(arb$famille))],
    #                    radius = ~diam/2
    #   )


    e <- sf::st_bbox(plac) %>% sf::st_as_sfc() %>% sf::st_buffer(30) %>%
      sf::st_as_sf() %>%
      mutate(id = "pla")

    data_new_projet(user, fake_proj, shp = e, replace = TRUE)


    # houppiers -------------------------

    data_mnt()
    data_mnh(grille_lidarhd = grille_lidarhd)
    if(! "lidarhd" %in% uDates()$origine) return(NULL)

    data_spot(spec_date = spec_date)

    data_crowns()
    md_crowns_metrics(sentinel = sentinel)

    metr <- readRDS(list.files(file.path(dc(dos_modeles)), pattern = ".rds", full.names = TRUE))

    cr <- uRast("crowns") %>% terra::as.polygons() %>% st_as_sf() %>%
      left_join(
        metr %>%
          filter(!duplicated(id) & !is.na(id)),
        by = "id") %>%
      filter(area > 10)

    sf::st_crs(cr) <- 2154

    # visu_leaflet_base() %>%
    #   addCircleMarkers(data = arb %>% sf::st_transform(4326),weight = 1,
    #                    color = rainbow(2)[as.numeric(as.factor(arb$famille))])

    # appar --------------------------

    i <- sf::st_intersection(arb %>% select(-id), cr)

    igc <- i %>% as.data.frame() %>% group_by(id) %>%
      summarise(
        n = n(),
        g = sum(pi * diam1^2 / 40000)
      )

    igc$ess <- purrr::map_chr(i$id %>% unique() %>% sort(), function(iid){

      ii <- i %>% filter(id == iid) %>%
        mutate(pp = g/sum(g))


      if(length(ii) == 0){

        NA_character_

      }else if(length(ii) == 1){

        ii$groupe[1]

      }else{# plusieurs arbres dans une couronne

        iie <- ii %>% group_by(groupe) %>%
          summarise(n = n(),
                    pp = sum(pp))

        if(max(iie$pp) > .8){
          iie$groupe[which(iie$pp == max(iie$pp))]
        }else{
          NA_character_
        }
      }
    })

    i %>% filter(!duplicated(as.character(id))) %>%
      arrange(id) %>%
      left_join(igc, by = "id") %>%
      filter(!is.na(ess))

    # visu_leaflet_base() %>%
    #   addCircleMarkers(data = cal %>% sf::st_transform(4326),weight = 1,
    #                    color = rainbow(2)[as.numeric(as.factor(arb$famille))]
    #   )
  }


  ls_cal <- purrr::map(1:nrow(aci), ~tryCatch(
    {
      message("Placette ", .x, "/", nrow(aci))
      suppressMessages(suppressWarnings(params_arbres_pp(.x, spec_date = spec_date)))
    },
    error = function(e){NULL}))


  ls00 <- ls_cal[! purrr::map_lgl(ls_cal, is.null)]

  # les dates sentinel peuvent varier: regroupements

  # dates_sen <- purrr::map(ls00, ~ unique(
  #                           stringr::str_split(
  #                             names(.x), "_x_", simplify = TRUE)[,2]
  # ))
  #
  #
  # dates <- unique(unlist(dates_sen)) %>% sort()


  ls000 <- purrr::map(ls00, ~ .x[, which(endsWith(names(.x), "01") | !stringr::str_detect(names(.x), "_x_"))])

  cal <- do.call(rbind, ls000) #%>% select(any_of(c("groupe", names(cr))))


  path <- file.path(dest_dir, paste0(dest_name, ".rds"))
# message("!!!!!!!! enregistré sous le dossier courant !!!!!!!!!!!!!!!!!")
  saveRDS(cal, "crown_ess_ain-res1-spot2018.rds") # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  saveRDS(cal, path)
  message("données de calibration sauvegardées sous ", path)
}
