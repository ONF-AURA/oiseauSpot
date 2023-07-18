#' Détection des couronnes sur MNH
#'
#' @param date_mnh  du MNH utilisé
#' @param ext polygone sf de l'étendue à détecter
#' @param path_mnh_ts chemin du MNH
#' @param buffer tampon à appliquer à exr
#' @param lim_h_rege hauteur minimale des apex à détecter
#' @param pente voir lidR::locate_trees
#' @param intercept voir lidR::locate_trees
#'
#' @return spatvector des couronnes
#' @export
#'
spot_crowns <- function(
    date_mnh,
    ext = oiseauData::data_conf("shp"),
    path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
    buffer = oiseauData::data_conf("buffer"),
    lim_h_rege = oiseauData::data_conf("lim_h_rege"),
    pente = .07,
    intercept = 2){

  mnhs <- terra::rast(path_mnh_ts)
  mnh0 <- mnhs[[as.Date(terra::time(mnhs)) == as.Date(date_mnh)]]

  h0 <- mnh0 %>% terra::crop(as(ext %>% st_buffer(buffer), "SpatVector")) %>%
    terra::mask(as(ext %>% st_buffer(buffer), "SpatVector"))

  # if(!is.null(path_mnh_dead_ts)){
  #   if(file.exists(path_mnh_dead_ts)){
  #     h0_msk <- terra::rast(path_mnh_dead_ts)
  #     dates_msk <- terra::time(h0_msk)
  #     dates_msk_util <- dates_msk[which(dates_msk < )]
  #   }
  #
  #   if(an %in% names(h0_msk)){
  #     terra::subset(as.character(an)) %>%
  #       terra::crop(ext %>% st_buffer(buffer)) %>%
  #       terra::mask(as(ext %>% st_buffer(buffer), "SpatVector"))
  #
  #     h0 <- h0 * h0_msk
  #   }else{
  #     message("Couche de masque indisponible.")
  #   }
  # }

  a <- spot_apex(h0, lim_h_rege)

  h0r <- h0
  suppressWarnings(terra::crs(h0r) <- NA)
  h0r <- as(h0r , "Raster")
  suppressWarnings(raster::crs(h0r) <- raster::crs(a))

  algo <- lidR::dalponte2016(h0r, a)
  cr <- rast(algo())
  names(cr) <- "id"

  terra::as.polygons(cr) %>% st_as_sf %>%
    mutate(area = sf::st_area(.) %>% as.numeric()) %>%
    filter(area > 1) %>%
    dplyr::select(-area) %>%
    as("SpatVector")
}
