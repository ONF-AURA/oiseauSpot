#' Conversion SpatRaster en Raster
#'
#' Palie un bug de conversion de la projection
#'
#' @param spat spatrasetr
#'
#' @return raster
#' @export
#'

spot_spat2rast <- function(spat){

  terra::crs(spat) <- ""
  rast <- as(spat, "Raster")
  raster::crs(rast) <- "+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs"

  rast
}
