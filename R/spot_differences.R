#' Détection des houppiers disparus entre deux dates
#'
#' @param paths liste des chemins d'images spot d'une même tuile, nommée par l'année de prise de vue
#' @param ext sf de découpage
#' @param an année à laquelle détecter les changements
#' @param buffer buffer à appliquer à ext
#' @param path_mnh chemin vers le MNH (résolution 1m)
#' @param an_mnh année du MNH
#' @param formula formule de l'indice utilisé, par défaut log(spot$ir/spot$g) où ir=infra r=rouge g=vert b=bleu. Voir https://www.indexdatabase.de/db/s-single.php?id=173
#' @param mask_mnh spatraster 0/1, où 0 correspond aux couronnes disparues les années précédentes
#'
#' @return spatRaster
#' @export
#'
spot_differences <- function(paths, ext, an, buffer = 10,
                             path_mnh = NULL, an_mnh = NULL,
                             formula = "log(spot$ir/spot$g)",
                             mask_mnh = NULL){

  spots <- spot_get(paths, ext = ext, template = terra::rast(path_mnh))

  spot_an <- spots[startsWith(names(spots), paste0("spot", an))][[1]]

  an_prec <- spot_an_prec(names(spots), an)
  spot_an_prec <- spots[startsWith(names(spots), paste0("spot", an_prec))][[1]]

  ind <- eval(parse(text = stringr::str_replace_all(formula, "spot", "spot_an")))
  ind_prec <- eval(parse(text = stringr::str_replace_all(formula, "spot", "spot_an_prec")))

  terra::crs(ind) <- terra::crs(ind_prec)

  ind_diff <- terra::scale(ind)-terra::scale(ind_prec)

  if(is.null(path_mnh)){
    message("MNH non fourni: si vous disposez d'un MNH, la prédiction sera améliorée.")
    return(c(ind_prec, ind))
  }else{
    cr <- suppressWarnings(spot_crowns(path_mnh = path_mnh, ext = ext, buffer = buffer,
                                       mask_mnh = mask_mnh))

    e <- terra::extract(c(ind_prec, ind), cr, fun = median, na.rm = TRUE)
    shp <- cr %>% sf::st_as_sf() %>%
      dplyr::mutate(i0 = e$ir, i1 = e$ir.1) %>%
      dplyr::select(c(i0, i1))


    ind_prec <- as(shp, "SpatVector") %>% terra::rasterize(ind, "i0")
    ind2 <- as(shp, "SpatVector") %>% terra::rasterize(ind, "i1")

    names(ind_prec) <- an_prec
    names(ind2) <- an

    c(ind_prec, ind2)


  }
}
