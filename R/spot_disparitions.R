



#' Raster des couronnes disparues entre deux images spot
#'
#'
#' @param date0 date de la première image spot
#' @param date1 date de la seconde image spot
#' @param date_mnh date du MNH utilisé pour la détection des couronnes
#' @param ext sf de la zone
#' @param path_spot_ts chemin du fichier .nc d'écriture de la série temporelle
#' @param path_mnh_ts chemin du MNH
#' @param buffer buffer shp
#' @param seuil_diff_spot seuil de différence à partir duquel la couronne est éliminée
#'
#' @return spatraster 0 = disparu, 1 = toujours présent
#' @export
#'
spot_disparitions <- function(
    date0, date1, date_mnh,
    ext = oiseauData::data_conf("shp"),
    path_spot_ts = oiseauData::data_conf("path_spot_ts"),
    path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
    tab_mnh = oiseauData::data_conf("tab_mnh"),
    buffer  = oiseauData::data_conf("buffer"),
    seuil_diff_spot = oiseauData::data_conf("seuil_diff_spot")
){



  mnhs <- oiseauData::data.load_mnh(path_mnh_ts, tab_mnh)

  date_mnh <- oiseauUtil::util_an2date(date_mnh, mnhs)

  mnh <- mnhs[[as.Date(terra::time(mnhs)) == as.Date(date_mnh)]]

  ind0 <- spot_indice(date_spot = date0, date_mnh = date_mnh) %>% terra::resample(mnh)
  ind1 <- spot_indice(date_spot = date1, date_mnh = date_mnh) %>% terra::resample(mnh)


  ras_dif <- terra::scale(ind1)-terra::scale(ind0)
  ras_dif[ras_dif < seuil_diff_spot] <- 1
  ras_dif[ras_dif >= seuil_diff_spot] <- 0

  ras_dif
}

