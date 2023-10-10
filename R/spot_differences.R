#' Détection des houppiers disparus entre deux dates
#'
#' @param paths liste des chemins d'images spot d'une même tuile, nommée par l'année de prise de vue
#' @param ext sf de découpage
#' @param an année à laquelle détecter les changements
#' @param buffer buffer à appliquer à ext
#' @param path_mnh chemin vers le MNH (résolution 1m)
#' @param an année du MNH
#' @param formula formule de l'indice utilisé, par défaut log(spot$ir/spot$g) où ir=infra r=rouge g=vert b=bleu. Voir https://www.indexdatabase.de/db/s-single.php?id=173
#' @param mask_mnh  pour les mises à jour annuelles de MHN: spatraster 0/1, où 0 correspond aux couronnes disparues les années précédentes
#'
#' @return spatRaster   nommé dif + indice + date1 + date0 (ex: "Dif ndvi 2023-05-06 2022-06-07")

#' @export
#'
spot_differences <- function(an = 0,
                             ext = oiseauData::data_conf("shp"),
                             path_spot_ts = oiseauData::data_conf("path_spot_ts"),
                             path_crowns_ts = oiseauData::data_conf("path_crowns_ts"),
                             path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
                             path_deads_ts = oiseauData::data_conf("path_deads_ts"),
                             buffer = oiseauData::data_conf("buffer"),
                             spot_best_day = oiseauData::data_conf("spot_best_day"), #"07-01",
                             indice = "ndvi"
){

  if(as.numeric(an) == 0){
    util_log("spot_differences", "Spécifiez l'année de l'image à comparer avec l'année précedante.")
    return("ko")
  }

  if(!file.exists(dc("path_crowns_ts"))){
    oiseauUtil::util_log("spot_difference", "Le raster des couronnes est requis: essayez spot_crowns()")
    return("ko")
  }

  # vérification des dates ---------------------------------

  date1 <- oiseauUtil::util_get_date(an, "spot")


  if(as.character(date1) == "ko"){
    message("Calcul des différences entre l'image SPOT de ", an, " et l'image de l'année précédente est impossible: dates non disponibles: pas d'image pour ", an)
    return(NULL)
  }

  an1 <- as.Date(date1) %>% format("%Y") %>% as.numeric()


  dates_dispo <- util_get_date("all", "spot") %>% as.Date()

  date0 <- max(dates_dispo[which(dates_dispo %>% format("%Y") %>% as.numeric() < an1)])

  if(is.na(date0)){
    message("Calcul des différences entre l'image SPOT de ", an, " et l'image de l'année précédente est impossible: dates non disponibles: année précédente absente.")
    return(NULL)
  }

  message("Calcul des différences de ", indice, " entre ", date0, " et ", date1)

  # rasters des indices par date et différence -------------------------

  rcr <- uRast("crowns", path = path_crowns_ts)

  name0 <- paste(indice, date0)
  name1 <- paste(indice, date1)

  if(! name0 %in% names(rcr)){
    ind <- spot_indice(date_spot = date0, indice = indice)
  }
  if(! name1 %in% names(rcr)){
    ind <- spot_indice(date_spot = date1, indice = indice)
  }

  rcr <- uRast("crowns", path = path_crowns_ts)


  ind_diff <- terra::scale(rcr[[name1]])-terra::scale(rcr[[name0]])

  # date du raster des couronnes à utiliser --------------------------------

  rcr_id <- tryCatch(rcr[[which(names(rcr) == "id")]],
                     error = function(e){NULL})


  if(is.null(rcr_id)){
    message("Pas de raster CROWNS: les différences sont renvoyées par pixel.")
    return(ind_diff)
  }


  diff_date <- abs(date0 - terra::time(rcr_id))

  cr_id <- rcr_id[[which(diff_date == min(diff_date))]]


  cr <- cr_id %>% terra::as.polygons()

  e <- terra::extract(ind_diff, cr, fun = median, na.rm = TRUE)

  shp <- cr %>% sf::st_as_sf() %>%
    dplyr::mutate(diff = e[[2]])


  diff <- as(shp, "SpatVector") %>% terra::rasterize(ind_diff, "diff")

  names(diff) <- paste("Dif", indice, date1, date0)

  # name(diff) = dif + indice + date1 + date0 (ex: "Dif ndvi 2023-05-06 2022-06-07)

  message("Pour afficher, utilisez la fonction spot_plotDiff")

  diff

}
