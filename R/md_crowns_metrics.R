#' Rasters des métriques des couronnes
#'
#' @param date_mnh date du MNH à utiliser
#' @param crowns raster des couronnes issu de data_crowns
#' @param mnh
#'
#' @return spatRaster
#' @export
#'
md_crowns_metrics <- function(date_mnh = util_get_date("last", "mnh"),
                                path_mnh_ts = data_conf("path_mnh_ts"),
                                path_crowns_ts = data_conf("path_crowns_ts"),
                                path_meta = data_conf("tab_crowns"),
                                path_mnt = data_conf("path_mnt"),
                                buffer = data_conf("buffer"),
                                mask = data_conf("shp"),
                                lim_h_rege = data_conf("lim_h_rege"),
                                best_day = data_conf("spot_best_day"),
                                dest_dir = dc("dos_modeles"),
                                spot_date = "last",
                                sentinel = TRUE
                                ){


  if(date_mnh == "ko"){
    return("ko")
  }

  if(!file.exists(path_mnh_ts)){
    util_log("md_crowns_metrics", paste("Le MNH n'existe pas."))
  }

  # raster des couronnes -------------------

  crowns <- uRast("crowns", path = path_crowns_ts, path_meta = path_meta)

  # names(crowns) <- uMet <- a(path_meta)$origine

  if(! date_mnh %in% as.character(terra::time(crowns))){
    util_log("md_crowns_metrics", paste0("La date ", date_mnh, "n'est pas disponible dans la série temporelle des couronnes"))
    return("ko")
  }
  crowns <- crowns[[terra::time(crowns) %>% as.character() == date_mnh]]


  # # données dérivés du MNH ------------------------
  #
  # mnh <- uRast("mnh", date = date_mnh)
  #
  # names(mnh) <- "h"
  #
  # pte <- terra::terrain(mnh, "slope") %>% terra::resample(mnh)
  # names(pte) <- "pte"
  #
  # pte2 <- terra::terrain(pte, "slope") %>% terra::resample(mnh)
  # names(pte2) <- "pte2"
  #
  # tpi <- terra::terrain(mnh, "TPI") %>% terra::resample(mnh)
  # names(tpi) <- "tpi"

  #
  # # spot
  #
  # date_spot <- uDates() %>% dplyr::mutate(diff = abs(date %>% as.Date() - as.Date(util_get_date("last", "mnh")))) %>%
  #   filter(var == "spot") %>%
  #   filter(diff == min(diff)) %>%
  #   dplyr::pull(date) %>% unique()
  #
  # sp <- uRast("spot", date = date_spot)
  #
  # sp$ndvi <- (sp$ir - sp$red) / (sp$ir + sp$red)
  # # names(sp) <- paste(names(sp), terra::time(sp), sep = "_")
  # sp <- sp %>% terra::resample(mnh)

  # synthèse par couronne ----------------------

  # pile <- c(mnh, pte, pte2, tpi, crowns, sp) %>%
  #   terra::mask(mask %>% sf::st_buffer(buffer) %>% as("SpatVector"))


  # rasters de prediction

  message("Constitution des rasters de prédiction")

  pile0 <- md_predicteurs(res = 1, spot_date = spot_date)

  pile <- c(crowns$id, pile0)

  #

  if(sentinel){

    message("Images Sentinel...")

    ansp <- util_get_date(spot_date, "spot") %>% as.Date() %>% format("%Y")

    sen <- oiseauSentinel::sen_data(tmin = paste0(ansp, "-01-01"), tmax = paste0(ansp, "-12-31"))
    sen <- uRast("sentinel")
    sen1 <- sen %>% terra::resample(pile[[1]])

    cr1 <- crowns$id %>% terra::as.polygons() %>% st_as_sf() %>%  sf::st_centroid()
     data_sen <- terra::extract(sen1, cr1)

     names(data_sen) <- c("id", paste(names(sen), terra::time(sen), sep = "_x_"))

  }


  message("Calcul des prédicteurs par couronne...")

  data0 <- terra::as.data.frame(pile, xy = TRUE) %>%
    dplyr::filter(!is.nan(id))
    # dplyr::filter(mnh_h >= lim_h_rege)



  data1 <- data0 %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(area = dplyr::n())

  if(sentinel){
    data1 <- data1 %>% left_join(data_sen, by = "id")
  }

  data2 <- data0 %>%

    dplyr::group_by(id) %>%
    dplyr::summarise(dplyr::across(-c(x,y), list(
      moy = function(x){mean(x, na.rm = TRUE)},
      med = function(x){median(x, na.rm = TRUE)},
      q10 = function(x){quantile(x, .1, na.rm = TRUE) %>% as.numeric()},
      q90 = function(x){quantile(x, .9, na.rm = TRUE) %>% as.numeric()},
      sd = function(x){sd(x, na.rm = TRUE)}
                                           )))

  data <- data2 %>%
    dplyr::left_join(data1, by="id") %>%
    dplyr::filter(area > 1) # & mnh_h_q90 >= lim_h_rege)

  # data_rcr <- terra::as.data.frame(crowns$id, na.rm = FALSE) %>%
  #   dplyr::left_join(data, by = "id")
  #
  # new_fact <- names(data_rcr %>% dplyr::select(-id))
  #
  # ls_r <- purrr::map(new_fact,
  #                    function(x){
  #                      r <- crowns[[1]]
  #                      terra::values(r) <- data_rcr[[x]]
  #                      names(r) <- x
  #                      r
  #                    })
  #
  # names(ls_r) <- new_fact
  #
  # rcr2 <- do.call(c, ls_r)
  #
  # new <- terra::rast(rcr2)
  #
  #
  #
  # data.ras_merge(new,
  #                            var = "crowns",
  #                            dest = path_crowns_ts,
  #                            path_meta = path_meta,
  #                            path_mnt = path_mnt)

  dest <- file.path(dest_dir, paste0("crowns_metrics_", date_mnh, ".rds"))

  saveRDS(data, dest)

  util_msg(paste("Métriques des couronnes suvegardées sous", dest), notification = TRUE)


}
