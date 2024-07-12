#' Cartographie par apprentissage
#'
#' @param fac2pred champs de la table tab_invrel_data à cartographier
#' @param use_topo si TRUE, utilise comme prédicteurs les rasters topographiques dérivés du MNT
#' @param use_dendro si TRUE, utilise comme prédicteurs les rasters dendrométriques issus des modèles lidar
#' @param use_spot si TRUE, utilise comme prédicteurs les indices de végétation spot
#' @param use_insol si TRUE, utilise comme prédicteurs les rasters d'insolation calculés par spot_insolation()
#' @param use_couvert si TRUE, utilise comme prédicteurs le MNH
#' @param suppress_rasters_with_na retire des prédicteurs les rasters dont le % de valeurs NA est supérieur au nombre donné (100: garde tout, 0: supprime si une valeur absente)
#' @param buff_pla rayon d'extraction
#' @param list_fac TRUE pour lister les facteurs disponibles
#'
#' @return spatRaster
#' @export
#'
#'
md_spatialise <- function(
    fac2pred,
    tab_inv = util_read_csv(dc("tab_invrel_data")),
    coords = c("Coord.X", "Coord.Y"),
    epsg = 4326,
    buff_pla = 20,
    use_topo = TRUE, use_dendro = TRUE, use_spot = TRUE, use_insol = TRUE, use_couvert = TRUE,
    suppress_rasters_with_na = 0,
    rsp_coper = FALSE,
    list_fac = FALSE
){


  if(list_fac){

    print(head(tab_inv))
    return("ok")
  }

  if(inherits(tab_inv, "data.frame")){
    if(any(! coords %in% names(tab_inv))){
      message("tab_inv ne comporte pas les champs de géométrie ", paste(coords, collapse = " et "))
      return("ko")
    }
  }


  # placettes ------------------------------------

  inv <- st_as_sf(tab_inv, coords=coords)
  sf::st_crs(inv) <- epsg
  inv <- sf::st_transform(inv, 2154) %>% st_buffer(buff_pla)

  if(!fac2pred %in% names(inv)){
    message(fac2pred, " n'est pas un champ de la table des placettes./nLes champs existants sont ",
            paste(names(inv), collapse = "\t"))

    return("ko")
  }

  y <- inv[[fac2pred]]

  if(inherits(y, "character")){
    y <- as.factor(y)
  }

  # prédicteurs --------------------------------------------

  pile <- data_predicteurs(topo = use_topo, dendro = use_dendro, spot = use_spot, insol = use_insol, couvert = use_couvert,
                           suppress_rasters_with_na,
                         rsp_coper = rsp_coper)



  x <- extract(pile, inv %>% as("SpatVector"), fun="mean", na.rm=TRUE) %>%
    select(-ID)

  x <- x[purrr::map_lgl(x, ~length(unique(.x)) > 1)]

 # suppressions données NA -------------------------------------

  x$y <- y
  x <- na.omit(x)

  y <- x$y
  x$y <- NULL

  # apprentissage et prédictionn ------------------------------------------------

  if(inherits(y, "factor")){
    sel <- varSelRF::varSelRF(xdata = x, Class = y)

    message("Variables sélectionnées: ", paste(sel$selected.vars, collapse = ", "))

    model <- randomForest::randomForest(y ~ ., data = x %>% select(sel$selected.vars))
  }

  print(model)

  rpred <- predict(pile, model)


  rpred %>% terra::mask(dc("shp") %>% sf::st_buffer(dc("buffer")))


}
