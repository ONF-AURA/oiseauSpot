#' Détection des couronnes sur MNH
#'
#' @param path_mnh_ts chemin du MNH
#' @param an année du MNH utilisé
#' @param ext polygone sf de l'étendue à détecter
#' @param buffer tampon à appliquer à exr
#' @param lim_h_rege hauteur minimale des apex à détecter
#' @param pente voir lidR::locate_trees
#' @param intercept voir lidR::locate_trees
#' @param path_mnh_mask_ts chemin spatraster 0/1, où 0 correspond aux couronnes disparues les années précédentes
#'
#' @return spatvector des couronnes
#' @export
#'
spot_crowns <- function(ext = oiseauData::data_conf("shp"),
                        path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
                        buffer = oiseauData::data_conf("buffer"),
                        lim_h_rege = oiseauData::data_conf("lim_h_rege"),
                        path_mnh_mask_ts = oiseauData::data_conf("path_mnh_mask_ts"),
                        an = oiseauData::data_conf("an1"),
                        pente = .07,
                        intercept = 2){

  h0 <- mnh_select_year(an)
    terra::crop(ext %>% st_buffer(buffer)) %>%
    terra::mask(as(ext %>% st_buffer(buffer), "SpatVector"))

  if(!is.null(path_mnh_mask_ts)){

    h0_msk <- terra::rast(path_mnh_mask_ts)

    if(an %in% names(h0_msk)){
      terra::subset(as.character(an)) %>%
        terra::crop(ext %>% st_buffer(buffer)) %>%
        terra::mask(as(ext %>% st_buffer(buffer), "SpatVector"))

      h0 <- h0 * h0_msk
    }else{
      message("Couche de masque indisponible.")
    }
  }

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
