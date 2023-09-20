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
    choix_crowns = "last",
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

  spots <- terra::rast(path_spot_ts)

  sel <- which(as.Date(terra::time(spots)) == as.Date(date_spot))

  if(length(sel) == 0){
    oiseauUtil::util_log("spot_indice", paste0("L'image SPOT du ", date_spot, " n'existe pas"))
    return("ko")
  }

  spot <- spots[[sel]]
  names(spot) <- terra::varnames(spot)

  ind <- eval(parse(text = stringr::str_replace_all(spot_formula(indice), "spot", "spot")))

  if(crowns & !file.exists(path_tab_crowns)){

    oiseauUtil::util_log("spot_indice", paste0("Aucun raster de couronnes n'est disponible. Le raster des indices par pixel est renvoyé."))
    crowns <- FALSE

  }else{

    meta <- read.csv(path_tab_crowns, stringsAsFactors = FALSE)
    dates_cr <- meta %>% dplyr::filter(origine == "id")


    if(choix_crowns == "last"){

      date_crowns <- dates_cr %>% dplyr::arrange(desc(date)) %>% dplyr::slice(1) %>% dplyr::pull(date)

    }else{

      check <- oiseauUtil::util_is_date(choix_crowns, "spot")

      if(check == "ko") crowns <- FALSE


      if(! choix_crowns %in% dates_cr$date){
        oiseauUtil::util_log("spot_indice", paste0("Le raster des couronnes du ", choix_crowns, " n'existe pas."))

        crowns <- FALSE
      }

    }

  }

  if(!crowns) return(ind)

  # raster des indices par couronne --------------------------


  if(is.null(path_crowns_ts)){
    message("MNH non fourni: si vous disposez d'un MNH, la prédiction sera améliorée.")
    return(ind)
  }else{

    mnhs <- terra::rast(path_crowns_ts)
    mnh <- mnhs[[as.Date(terra::time(mnhs)) == as.Date(date_crowns)]]

    cr0 <- terra::rast(path_crowns_ts)
    cr0 <- cr0[[as.Date(terra::time(cr0)) == as.Date(date_crowns)]]

    cr <- terra::as.polygons(cr0)

    e <- terra::extract(ind, cr, fun = median, na.rm = TRUE)

    shp <- cr %>% sf::st_as_sf() %>%
      dplyr::mutate(i = e$ir) %>%
      dplyr::select(i)


    ind <- as(shp, "SpatVector") %>% terra::rasterize(ind, "i")

    terra::time(ind) <- as.Date(date_crowns)
    names(ind) <- paste(indice, date_spot, collapse = "::")

    oiseauData::data.ras_merge(ind,
                               var = "crowns",
                               dest = path_crowns_ts,
                               path_meta = path_tab_crowns,
                               path_mnt = path_mnt)



    ind

  }
}
