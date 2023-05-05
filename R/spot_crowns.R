#' Détection des couronnes sur MNH
#'
#' @param path_mnh chemin du MNH
#' @param ext polygone sf de l'étendue à détecter
#' @param buffer tampon à appliquer à exr
#' @param lim_h_rege hauteur minimale des apex à détecter
#' @param pente voir lidR::locate_trees
#' @param intercept voir lidR::locate_trees
#' @param mask_mnh spatraster 0/1, où 0 correspond aux couronnes disparues les années précédentes
#'
#' @return spatvector des couronnes
#' @export
#'
spot_crowns <- function(path_mnh, ext, buffer, lim_h_rege = 3,
                        pente = .07, intercept = 2, mask_mnh = NULL){

  h0 <- rast(path_mnh) %>%
    terra::crop(ext %>% st_buffer(buffer)) %>%
    terra::mask(as(ext %>% st_buffer(buffer), "SpatVector"))

  if(!is.null(mask_mnh)){
    h0 <- h0 * mask_mnh
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
