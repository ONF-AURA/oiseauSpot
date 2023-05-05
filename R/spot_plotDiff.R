#' Image de l'indice et de son évolution
#'
#' @param diff liste issue de spot_differences
#' @param pal1 vecteur de couleurs pour carte d'indice
#' @param pal2 vcteur de couleurs pour carte des différence
#'
#' @return plot
#' @export
#'

spot_plotDiff <- function(diff,
                          pal1 =c("green", "yellow", "red"),
                          pal2 = c("green", "green", "yellow", "red")){

  ecart <- terra::scale(diff[[2]])-terra::scale(diff[[1]])

  layout(matrix(1:2, nrow = 1))
  terra::plot(diff[[2]], col = colorRampPalette(pal1)(20),
              main = names(diff)[1])
  terra::plot(ecart, col = colorRampPalette(pal2)(20),
              main = paste("Différence", names(diff)[1], "/",names(diff)[2]))

  layout(1)

}
