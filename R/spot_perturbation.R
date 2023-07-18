#' Test de délimitation des zones ouvertes entre 2 dates
#'
#' @param lim valeur limite de l'indicateur utilisé par ras_diff
#' @param diff liste des rasters sortis de spot_differences (ignoré si ras_dif non nul)
#' @param ras_dif si diff NULL, spatraster des différences entre deux images spot
#' @param liste_spots liste des rasters RGB Spot, nommée "spotXXXX" où XXXX est l'année de prise de vue
#' @param an année à laquelle constater les évolutions
#' @param sub sous-titre
#' @param horizontal agencement des images multiples: TRUE ou FALSE
#' @param overlay sf polygones à supperposer sur l'image
#' @param mask TRUE pour utiliser overlay comme masque
#' @param col couleur de délimitation du seuil
#'
#'
#' @return plot
#' @export
#'
spot_perturbation <- function(lim, diff = NULL, ras_dif = NULL, liste_spots = NULL, an, sub = NULL, horizontal = TRUE,
                              overlay = NULL, mask = FALSE, col = "blue"){

  if(is.null(ras_dif)){
    ras_dif <- terra::scale(diff[[2]])-terra::scale(diff[[1]])
  }

  test <- ras_dif
  test[ras_dif < lim] <- 1
  test[ras_dif >= lim] <- 0



  terra::plot(ras_dif, col = colorRampPalette(c("green", "green", "yellow", "red"))(20),
              main = paste("indice", an))

  terra::contour(test, col=col, add = TRUE, lty = 1)

  # année précédente

  an_prec <- spot_an_prec(terra::time(liste_spots), an)

  if(!is.null(liste_spots)){

    # images spot utilisées

    ans_spots <- liste_spots %>% terra::time() %>% format("%Y")

    ls_sp <- list(
      an0 = liste_spots[[which(ans_spots == an_prec)]],
      an1 = liste_spots[[which(ans_spots == an)]]
    )
    spots <- purrr::map(ls_sp, ~ terra::crop(.x, ras_dif))

    # graph

    layout(matrix(c(1,2), nrow = 1))

    spot_plotRGB(spots, evo = test, sub = paste(sub, "\nseuil indic = ", lim),
                 horizontal = horizontal, overlay = overlay, mask = mask, col = col)

    layout(1)
  }

  # pas de couronne = pas de perturbation
  test0 <- test
  test0[is.na(test)] <- 1
  test0[is.na(spots[[1]][[1]])] <- NA


  return(test0)

}

