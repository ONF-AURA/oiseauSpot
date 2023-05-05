#' Image couleur SPOT
#'
#' @param s spatraster 4 bandes nommées: r (rouge) ir (infrarouge) b (bleu) et g (vert), OU liste de spatraster 4 bandes nommées "spotXXXX" où XXXX est l'année de prise de vue
#' @param ir TRUE pour une image en fausses couleurs infrarouge
#' @param ans 0 pour toutes les images, ou vecteur des années à afficher
#' @param horizontal agencement des images multiples: TRUE ou FALSE
#' @param overlay sf polygones à supperposer sur l'image
#' @param mask TRUE pour utiliser overlay comme masque
#' @param sub sous-titre
#' @param evo raster 0/1 des évolutions (facultatif)
#' @param col couleur de délimitation du seuil
#'
#' @return plot
#' @export
#'

spot_plotRGB <- function(s, ir = FALSE, ans = 0, horizontal = TRUE, overlay = NULL, mask = FALSE,
                         sub = NULL, evo = NULL, col = "blue"){

  if(!inherits(s, "list")) s <- list(s)
  if(ans[1] != 0){
    s <- s[paste("spot", ans, sep="")]
  }

  fun <- function(i){

    si <- s[[i]]

    if(mask & !is.null(overlay)){
      si <- terra::mask(si, as(overlay, "SpatVector"))
    }

    nx <- terra::minmax(si)
    rn <- (si - nx[1,]) / (nx[2,] - nx[1,])
    if(ir){
      terra::plotRGB(rn,4,1,2, scale=1, stretch="hist", smooth=TRUE)
    }else{
      terra::plotRGB(rn,1,2,3, scale=1, stretch="hist", smooth=TRUE)
    }
    title(main = names(s)[i], sub = sub, line = -1, outer = FALSE)

    if(!is.null(overlay)){
      plot(overlay %>% dplyr::select(1), add = TRUE, pal = "#00000000")
    }

    if(!is.null(evo)){

      terra::contour(evo, add=TRUE, col = col, lty = 1)
    }
  }

  layout(matrix(c(1:length(s)), nrow = ifelse(horizontal, 1, length(s))))

  purrr::map(1:length(s), fun)

  layout(matrix(1, nrow = 1))

}
