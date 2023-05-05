#' Détection des apex sur MNH
#'
#' @param h mnh projeté en Lambert93 (epsg 2154)
#' @param lim_h_rege hauteur minimale des apex à détecter
#' @param pente voir lidR::locate_trees
#' @param intercept voir lidR::locate_trees
#'
#' @return spatialpoints
#' @export
#'
spot_apex <- function(h, lim_h_rege = 3, pente = .07, intercept = 2){

  hs <- terra::focal(h, w = matrix(1,3,3), fun = median, na.rm = TRUE)

  ttops <- lidR::locate_trees(
    hs,
    lidR::lmf(function(x) {x * pente + intercept},
              hmin = lim_h_rege, shape = "circular")
  ) %>%
    sf::as_Spatial()

  suppressWarnings(raster::crs(ttops) <- "+init=epsg:2154")

  ttops
}

