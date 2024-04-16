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
#' @param indice noms des indices utilisés, par défaut ndvi. voir spot_formula
#' @param path_mnt chemin du MNT
#' @param res NULL ou nouvelle résolution (m)
#'
#' @return spatRaster
#' @export
#'
spot_indice <- function(
    date_crowns = "last",
    date_spot,
    crowns = TRUE,
    shp_roi = data_conf("shp"),
    path_spot_ts = data_conf("path_spot_ts"),
    path_crowns_ts = data_conf("path_crowns_ts"),
    path_tab_crowns = data_conf("tab_crowns"),
    path_mnt = data_conf("path_mnt"),
    buffer = data_conf("buffer"),
    indice = c("ndvi", "bai", "savi"),
    res = NULL
){

  # raster des indices par pixel ----------------------------------

  message("Calcul des indices ", paste(indice, collapse = " + "), "de l'image Spot ", date_spot, "...")

  spot <- uRast("spot", date = date_spot)

  if(!is.null(res)){
    if(crowns){
      message("Argument res ignoré car crowns = TRUE")
    }
    spot <- spot %>% uResolution(res)
  }

  if(length(spot) == 0){
    util_log("spot_indice", paste0("L'image SPOT du ", date_spot, " n'existe pas"))
    return("ko")
  }


  if(length(indice) == 1){
    indice <- list(i = indice)
  }else if(!inherits(indice, "list")){
    indice <- as.list(indice)
  }

  names(indice) <- unlist(indice)

  ind <- purrr::map(indice, ~eval(parse(text = stringr::str_replace_all(spot_formula(.x), "spot", "spot"))))

  for(n in names(ind)){
    names(ind[[n]]) <- n
  }

  ind <- terra::rast(ind)

  if(!crowns) return(ind)

  # raster des indices par couronne --------------------------


  cr0 <- uRast("crowns", origine = "id", date = date_crowns, path = path_crowns_ts, path_meta = path_tab_crowns)


  if(length(cr0) == 0){
    util_log("spot_indice", paste0("Le raster des couronnes (crowns) n'existe pas."))
    return("ko")
  }

  names(ind) <- paste0(names(ind), "_med")
  e <- util_extract(c(cr0, ind), names(cr0))

  v <- data.frame(id = values(cr0)) %>%
    left_join(e, by = c("id"="cr"))




  indcr <- list()

  for(n in names(ind)){

    n0 <- stringr::str_remove(n, " _med")

    indcr[[n0]] <- cr0

    values(indcr[[n0]]) <- v[[n]]
    terra::time(indcr[[n0]]) <- terra::time(cr0)
    names(indcr[[n0]]) <- paste(indice, date_spot, collapse = "::")

    data.ras_merge(indcr[[n0]],
                               var = "crowns",
                               dest = path_crowns_ts,
                               path_meta = path_tab_crowns,
                               path_mnt = path_mnt)

  }

  terra::rast(indcr)

}

