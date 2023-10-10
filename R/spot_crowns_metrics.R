#' Rasters des métriques des couronnes
#'
#' @param date_mnh date du MNH à utiliser
#' @param crowns raster des couronnes issu de spot_crowns
#' @param mnh
#'
#' @return spatRaster
#' @export
#'
spot_crowns_metrics <- function(date_mnh,
                                path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
                                path_crowns_ts = oiseauData::data_conf("path_crowns_ts"),
                                path_meta = oiseauData::data_conf("tab_crowns"),
                                path_mnt = oiseauData::data_conf("path_mnt"),
                                buffer = oiseauData::data_conf("buffer"),
                                mask = oiseauData::data_conf("shp"),
                                lim_h_rege = oiseauData::data_conf("lim_h_rege"),
                                best_day = oiseauData::data_conf("spot_best_day")
){

  date_mnh <- oiseauUtil::util_is_date(date_mnh, "mnh")

  if(date_mnh == "ko"){
    return("ko")
  }

  if(!file.exists(path_mnh_ts)){
    oiseauUtil::util_log("spot_crowns_metrics", paste("Le MNH n'existe pas."))
  }

  # raster des couronnes -------------------

  crowns <- uRast("crowns", path = path_crowns_ts, path_meta = path_meta)

  names(crowns) <- uMeta(path_meta)$origine

  if(! date_mnh %in% as.character(terra::time(crowns))){
    oiseauUtil::util_log("spot_crowns_metrics", paste0("La date ", date_mnh, "n'est pas disponible dans la série temporelle des couronnes"))
    return("ko")
  }
  crowns <- crowns[[terra::time(crowns) %>% as.character() == date_mnh]]

  # raster du MNH ---------------------------

  mnh <- uMnh(path_mnh_ts, date = date_mnh)

  names(mnh) <- "h"

  # rasters dérivés du MNH ------------------------

  pte <- terra::terrain(mnh, "slope") %>% terra::resample(mnh)
  names(pte) <- "pte"

  pte2 <- terra::terrain(pte, "slope") %>% terra::resample(mnh)
  names(pte2) <- "pte2"

  tpi <- terra::terrain(mnh, "TPI") %>% terra::resample(mnh)
  names(tpi) <- "tpi"

  # synthèse par couronne ----------------------

  pile <- c(mnh, pte, pte2, tpi, crowns) %>%
    terra::mask(mask %>% sf::st_buffer(buffer) %>% as("SpatVector"))


  data0 <- terra::as.data.frame(pile, xy = TRUE) %>%
    dplyr::filter(!is.nan(id)) %>%
    dplyr::filter(h >= lim_h_rege)

  data <- data0 %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(area = dplyr::n(),
                     hmax = max(h, na.rm=TRUE),
                     h5 = median(h, na.rm=TRUE),
                     h1 = quantile(h, .9, na.rm=TRUE),
                     ptm = mean(pte, na.rm=TRUE),
                     pt5 = median(pte, na.rm=TRUE),
                     pt9 = quantile(pte, .9, na.rm=TRUE),
                     dpt = median(pte2, na.rm=TRUE),
                     tpi5 = quantile(tpi, .5, na.rm=TRUE),
                     tpi9 = quantile(tpi, .9, na.rm=TRUE)
    ) %>%
    dplyr::filter(area > 1)

  data_rcr <- terra::as.data.frame(crowns$id, na.rm = FALSE) %>%
    dplyr::left_join(data, by = "id")

  new_fact <- names(data_rcr %>% dplyr::select(-id))

  ls_r <- purrr::map(new_fact,
                     function(x){
                       r <- crowns[[1]]
                       terra::values(r) <- data_rcr[[x]]
                       names(r) <- x
                       r
                     })

  names(ls_r) <- new_fact

  rcr2 <- do.call(c, ls_r)

  new <- terra::rast(rcr2)

  oiseauData::data.ras_merge(new,
                             var = "crowns",
                             dest = path_crowns_ts,
                             path_meta = path_meta,
                             path_mnt = path_mnt)

  oiseauUtil::util_msg("Métriques des couronnes ajoutées.", notification = TRUE)


}
