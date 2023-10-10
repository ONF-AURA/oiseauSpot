#' Indice de végétation des couronnes
#'
#' @param date_crowns date du MNH utilisé, ou 'last' pour utiliser le plus récent
#' @param date_spot date de l'image SPOT utilisée
#' @param crowns TRUE pour l'indice par couronne, sinon par pixel
#' @param shp_roi sf de découpage
#' @param buffer buffer à appliquer à shp_roi
#' @param path_spot_ts chemin vers le raster de série temporelle spot
#' @param path_crowns_ts chemin vers le raster de série temporelle des couronnes (résolution 1m)
#' @param path_tab_crowns chemin du fichier meta de la série temporelle des couronnes
#' @param indice nom de l'indice utilisé, par défaut ndvi. voir spot_formula
#'
#' @return spatRaster
#' @export
#'
spot_indice <- function(
    date_crowns = "last",
    date_spot,
    crowns = TRUE,
    shp_roi = oiseauData::data_conf("shp"),
    path_spot_ts = oiseauData::data_conf("path_spot_ts"),
    path_crowns_ts = oiseauData::data_conf("path_crowns_ts"),
    path_tab_crowns = oiseauData::data_conf("tab_crowns"),
    path_mnt = oiseauData::data_conf("path_mnt"),
    buffer = oiseauData::data_conf("buffer"),
    indice = "ndvi"
){

# raster des indices par pixel ----------------------------------

  message("Calcul des indices", paste(indice, collapse = " + "), "de l'image Spot", date_spot, "...")

  spot <- uRast("spot", date = date_spot)

  if(length(spot) == 0){
    oiseauUtil::util_log("spot_indice", paste0("L'image SPOT du ", date_spot, " n'existe pas"))
    return("ko")
  }



  ind <- eval(parse(text = stringr::str_replace_all(spot_formula(indice), "spot", "spot")))


  if(!crowns) return(ind)

  # raster des indices par couronne --------------------------


    cr0 <- uRast("crowns", origine = "id", date = date_crowns, path = path_crowns_ts, path_meta = path_tab_crowns)


  if(length(cr0) == 0){
    oiseauUtil::util_log("spot_indice", paste0("Le raster des couronnes (crowns) n'existe pas."))
    return("ko")
  }

    cr <- terra::as.polygons(cr0)

    e <- terra::extract(ind, cr, fun = median, na.rm = TRUE)

    shp <- cr %>% sf::st_as_sf() %>%
      dplyr::mutate(i = e$ir) %>%
      dplyr::select(i)


    ind <- as(shp, "SpatVector") %>% terra::rasterize(ind, "i")

    terra::time(ind) <- terra::time(cr0)
    names(ind) <- paste(indice, date_spot, collapse = "::")

    oiseauData::data.ras_merge(ind,
                               var = "crowns",
                               dest = path_crowns_ts,
                               path_meta = path_tab_crowns,
                               path_mnt = path_mnt)



    ind

  }

