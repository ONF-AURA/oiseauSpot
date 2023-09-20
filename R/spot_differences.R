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
#' @return spatRaster
#' @export
#'
spot_differences <- function(ext = oiseauData::data_conf("shp"),
                             path_spot_ts = oiseauData::data_conf("path_spot_ts"),
                             path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
                             path_mnh_dead_ts = oiseauData::data_conf("path_mnh_dead_ts"),
                             an = oiseauData::data_conf("an1"),
                             buffer = oiseauData::data_conf("buffer"),
                             spot_best_day = oiseauData::data_conf("spot_best_day"), #"07-01",
                             formula = oiseauSpot::spot_indice("NDVI")
                             ){



  spot_an <- spot_select_year(an, path_spot_ts, spot_best_day, ifnull = "nothing")

  if(is.null(spot_an)){
    message("Calcul des différences entre l'image SPOT de ", an, " et l'image de l'année précédente est impossible: dates non disponibles: pas d'image pour ", an)
    return(NULL)
  }

  spot_an_prev <- spot_select_year(an - 1, path_spot_ts, spot_best_day, ifnull = "prev")

  if(is.null(spot_an_prev)){
    message("Calcul des différences entre l'image SPOT de ", an, " et l'image de l'année précédente est impossible: dates non disponibles: année précédente absente.")
    return(NULL)
  }

  an_prev <- terra::time(spot_an_prev) %>% unique() %>% format("%Y") %>% as.numeric()

  ind <- eval(parse(text = stringr::str_replace_all(formula, "spot", "spot_an")))
  ind_prec <- eval(parse(text = stringr::str_replace_all(formula, "spot", "spot_an_prev")))

  terra::crs(ind) <- terra::crs(ind_prec)

  ind_diff <- terra::scale(ind)-terra::scale(ind_prec)

  if(is.null(path_mnh_ts)){
    message("MNH non fourni: si vous disposez d'un MNH, la prédiction sera améliorée.")
    return(c(ind_prec, ind))
  }else{
    cr <- suppressWarnings(spot_crowns(path_mnh_ts = path_mnh_ts, ext = ext, buffer = buffer,
                                       date_mnh = an))

    e <- terra::extract(c(ind_prec, ind), cr, fun = median, na.rm = TRUE)
    shp <- cr %>% sf::st_as_sf() %>%
      dplyr::mutate(i0 = e$ir, i1 = e$ir.1) %>%
      dplyr::select(c(i0, i1))


    ind_prec <- as(shp, "SpatVector") %>% terra::rasterize(ind, "i0")
    ind2 <- as(shp, "SpatVector") %>% terra::rasterize(ind, "i1")

    names(ind_prec) <- an_prev
    names(ind2) <- an

    c(ind_prec, ind2)


  }
}
