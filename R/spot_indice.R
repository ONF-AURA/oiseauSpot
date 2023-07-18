#' Indice de végétation des couronnes
#'
#' @param date_mnh date du MNH utilisé
#' @param date_spot date de l'image SPOT utilisée
#' @param shp_roi sf de découpage
#' @param buffer buffer à appliquer à shp_roi
#' @param path_spot_ts chemin vers le raster de série temporelle spot
#' @param path_mnh_ts chemin vers le raster de série temporelle du MNH (résolution 1m)
#' @param indice nom de l'indice utilisé, par défaut ndvi. voir spot_formula
#'
#' @return spatRaster
#' @export
#'
spot_indice <- function(
    date_mnh,
    date_spot,
    shp_roi = oiseauData::data_conf("shp"),
    path_spot_ts = oiseauData::data_conf("path_spot_ts"),
    path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
    buffer = oiseauData::data_conf("buffer"),
    # spot_best_day = oiseauData::data_conf("spot_best_day"), #"07-01",
    indice = "ndvi"
){

  spots <- terra::rast(path_spot_ts)
  spot <- spots[[which(as.Date(terra::time(spots)) == as.Date(date_spot))]]
  names(spot) <- terra::varnames(spot)

  ind <- eval(parse(text = stringr::str_replace_all(spot_formula(indice), "spot", "spot")))

  # ind_diff <- terra::scale(ind)-terra::scale(ind_prec)

  if(is.null(path_mnh_ts)){
    message("MNH non fourni: si vous disposez d'un MNH, la prédiction sera améliorée.")
    return(ind)
  }else{

    mnhs <- terra::rast(path_mnh_ts)
    mnh <- mnhs[[as.Date(terra::time(mnhs)) == as.Date(date_mnh)]]

    cr <- suppressWarnings(spot_crowns(date_mnh = date_mnh))

    e <- terra::extract(ind, cr, fun = median, na.rm = TRUE)

    shp <- cr %>% sf::st_as_sf() %>%
      dplyr::mutate(i = e$ir) %>%
      dplyr::select(i)


    ind <- as(shp, "SpatVector") %>% terra::rasterize(ind, "i")

    terra::time(ind) <- as.Date(date_spot)

ind

  }
}
