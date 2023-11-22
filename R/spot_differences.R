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
                             indice = c("ndvi")
){

  # if(as.numeric(an) == 0){
  #   util_log("spot_differences", "Spécifiez l'année de l'image à comparer avec l'année précedante.")
  #   return("ko")
  # }

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

  date0 <- max(dates_dispo[which(dates_dispo %>% format("%Y") %>% as.numeric() < an1)]) %>% as.character()

  if(is.na(date0)){
    message("Calcul des différences entre l'image SPOT de ", an, " et l'image de l'année précédente est impossible: dates non disponibles: année précédente absente.")
    return(NULL)
  }

  message("Calcul des différences de ", paste(indice, collapse = " + "), " entre ", date0, " et ", date1)

  # rasters des indices par date et différence -------------------------

  rcr <- uRast("crowns", path = path_crowns_ts)

  new <- list()

  for(date_n in c(date0, date1)){
    names_n <- paste(indice, date_n)

    ind_abs <- indice[which(!names_n %in% names(rcr))]

    for(n in ind_abs){
      new[[paste(n, date_n)]] <- spot_indice(date_spot = date_n, indice = n)
    }
  }

  # recharge les indices par couronne

  rcr <- uRast("crowns", path = path_crowns_ts)

  rcr_id <- tryCatch(rcr[[which(names(rcr) == "id")]],
                     error = function(e){NULL})


  if(is.null(rcr_id)){
    message("Pas de raster CROWNS: les différences sont renvoyées par pixel.")
    return(ind_diff)
  }

  # date du raster des couronnes à utiliser --------------------------------

  diff_date <- abs(date0 %>% as.Date() - terra::time(rcr_id))

  cr_id <- rcr_id[[which(diff_date == min(diff_date))]]

  cr <- cr_id %>% terra::as.polygons()

  #

  fun_diff <- function(i, date0, date1, cr){

    name0 <- paste(i, date0)
    name1 <- paste(i, date1)

    ind_diff <- terra::scale(rcr[[name1]])-terra::scale(rcr[[name0]])


    # e <- terra::extract(ind_diff[[name1]], cr, fun = median, na.rm = TRUE)
    #
    # shp <- cr %>% sf::st_as_sf() %>%
    #   dplyr::mutate(diff = e[[2]])
    #
    #
    # diff <- as(shp, "SpatVector") %>% terra::rasterize(ind_diff, "diff")

    names(ind_diff) <- paste("Dif", i, date1, date0)

    ind_diff
  }

  if(inherits(indice, "character")) indice <- list(indice)

  diffs <- purrr::map(indice, ~ fun_diff(.x, date0, date1, cr))

  # name(diff) = dif + indice + date1 + date0 (ex: "Dif ndvi 2023-05-06 2022-06-07)

  message("Pour afficher, utilisez la fonction spot_plotDiff")

  terra::rast(diffs)

}
