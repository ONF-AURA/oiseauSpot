#' Détection des apex sur MNH
#'
#' @param h mnh projeté en Lambert93 (epsg 2154)
#' @param lim_h_rege hauteur minimale des apex à détecter
#' @param pente voir lidR::locate_trees
#' @param intercept voir lidR::locate_trees
#' @param smooth fenêtre de lissage du mnh
#' @param uniqueness incremental ou bitmerge
#'
#' @return spatialpoints
#' @export
#'
spot_apex <- function(h, lim_h_rege = 3, pente = .07, intercept = 2, smooth = 5, uniqueness = "incremental"){

  if(terra::res(h) > 1.1){
    util_log("spot_apex", paste("La résolution du MNH est insuffisante."))
    return("ko")
  }

  hs <- terra::focal(h, w = matrix(1, smooth, smooth), fun = median, na.rm = TRUE)

  h_pix <- terra::values(hs)
  n_h <- h_pix[h_pix >= lim_h_rege] %>% length()

  # locate_tree ne supporte pas les raster on-disk
  hss <- terra::values(hs)
  terra::values(hs) <- hss

  fun <- function(intercept){
    lidR::locate_trees(
      hs,
      lidR::lmf(function(x) {x * pente + intercept},
                hmin = lim_h_rege, shape = "circular"),
      uniqueness = uniqueness
    ) %>%
      sf::as_Spatial()
  }

  if(is.null(intercept)){

    # recherche de l'intercept "raisonnable"

    ls_ttops <- purrr::map(2:12, fun)

    ls_pp <- purrr::map_dbl(ls_ttops, ~ max(.x@data$treeID, na.rm = TRUE))

    ls_pp <- ls_pp / ls_pp[1]

    pp <- max(ls_pp[which(ls_pp < .5)])
    print(pp)

    ttops <- ls_ttops[[which(ls_pp == pp)[1]]]

  }else{
    ttops <- fun(intercept)
  }

suppressWarnings(raster::crs(ttops) <- "+init=epsg:2154")

ttops
}

