#' Fonction interne de md_ref_crowns_params
#'
#' @param i ligne de aci
#' @param spec_date date
#' @param aci table des placettes
#' @param arbres_ttl table des arbres
#' @param fake_proj nom projet
#'
#' @return sf
#' @export
#'

md.ref_crowns_params_placette <- function(i, spec_date, aci, arbres_ttl, fake_proj, grille_lidarhd,
                                          topo, dendro, spot,
                                          insol, mnh,
                                          sentinel, copernicus, dest_pp, rsp_coper = FALSE){

  tryCatch({

    plac <- aci %>% slice(i)

    num_pla <- i
    # message("placette ", i, "...")

    coo_pla <- sf::st_centroid(plac) %>% sf::st_coordinates()



    # sf des arbres (points)
    arb <- arbres_ttl %>% dplyr::filter(id == plac$id) %>%
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

    fake_proj_x <- paste0(fake_proj, sample(1e15, 1))

    suppressMessages({
      data_new_projet(fake_proj_x, shp = e, replace = TRUE)


      # houppiers -------------------------

      data_mnt()
      data_mnh(grille_lidarhd = grille_lidarhd)
      if(! "lidarhd" %in% uDates()$origine) return(NULL)

      data_crowns()

      if(spot){
        data_spot(spec_date = spec_date)
      }
    })

    suppressMessages({
      metr <- md_crowns_metrics(topo = topo, dendro = dendro, spot = spot, insol = insol, mnh = mnh,
                                sentinel = sentinel, copernicus = copernicus, rsp_coper = rsp_coper)
    })


    cr <- uRast("crowns") %>% terra::as.polygons() %>% st_as_sf() %>% st_make_valid() %>%
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

    i <- sf::st_intersection(arb %>% select(-any_of(c("id"))), cr) %>%
      mutate(g = pi*diam^2/40000)

    igc <- i %>% as.data.frame() %>% group_by(id) %>%
      summarise(
        n = n(),
        G = sum(g)
      )

    igc$ess <- purrr::map_chr(i$id %>% unique() %>% sort(), function(iid){

      ii <- i %>% filter(id == iid) %>%
        mutate(pp = g/sum(g))


      if(length(ii) == 0){

        NA_character_

      }else if(length(ii) == 1){

        ii$essence[1]

      }else{# plusieurs arbres dans une couronne

        iie <- ii %>% group_by(essence) %>%
          summarise(n = n(),
                    pp = sum(pp))

        if(max(iie$pp) > .8){
          iie$essence[which(iie$pp == max(iie$pp))]
        }else{
          NA_character_
        }
      }
    })

    pp <- i %>% filter(!duplicated(as.character(id))) %>%
      arrange(id) %>%
      select(-any_of(c("g"))) %>%
      left_join(igc, by = "id") %>%
      filter(!is.na(ess))

    # suppression des dominés (l'ombrage rend leur prédiction difficile)
    rcr <- uRast("crowns")
    names(rcr) <- "id"
    mnh <- uRast("mnh", "last") %>% aggregate(10, na.rm = TRUE) %>%
      project(rcr)
    names(mnh) <- "h"
    hhh <- c(mnh, rcr) %>%
      as.data.frame() %>%
      group_by(id) %>%
        summarise(hmoy10 = mean(h, na.rm = TRUE)) %>%
      filter(id %in% pp$id)

    pp <- pp %>% left_join(hhh, by = "id")

    saveRDS(pp, file.path(dest_pp, paste0("pla_", num_pla, ".rds")))

    pp
    # visu_leaflet_base() %>%
    #   addCircleMarkers(data = cal %>% sf::st_transform(4326),weight = 1,
    #                    color = rainbow(2)[as.numeric(as.factor(arb$famille))]
    #   )
  },
  error = function(e){
    message(crayon::inverse(crayon::red("placette", num_pla, ": ECHEC")))
    NULL
  })
}
