#' Image de l'indice et de son évolution
#'
#' @param diff liste issue de spot_differences
#' @param pal1 vecteur de couleurs pour carte d'indice
#' @param pal2 vcteur de couleurs pour carte des différence
#' @param ID zoom sur un ID foncier ou NULL pour tout
#' @param overlay couche vecteur à superposer (à défaut le shp foncier)
#' @param rev TRUE si la disparition correspond à une baisse de l'indice, FALSE sinon
#'
#' @return plot
#' @export
#'

spot_plotDiff <- function(diff, ID = NULL, overlay = oiseauData::data_conf("shp"),
                          pal1 =c("green", "yellow", "red"),
                          pal2 = c("green", "green", "yellow", "red"),
                          rev = TRUE){

  # name(diff) = dif + indice + date1 + date0 (ex: "Dif ndvi 2023-05-06 2022-06-07)

  if(rev) pal2 <- rev(pal2)

  nm <- stringr::str_split(names(diff), " ")[[1]][-1]

  crs <-  uRast("crowns")

 d0 <- crs[[paste(nm[1], nm[3])]] %>% terra::resample(diff)
 d1 <- crs[[paste(nm[1], nm[2])]] %>% terra::resample(diff)

 dd <- c(d0,d1, diff)

 over <- overlay

 if(!is.null(ID)){

   if(!all(ID %in% over$id)){
     message(crayon::red("ID inconnu"))

     return("ko")
   }
   over <- over %>% dplyr::filter(id %in% ID)
   dd <- uCropId(dd, ID)
 }


 plot <- function(x, pal, main = names(x)){

   terra::plot(x, col = colorRampPalette(pal)(20),
               axes = FALSE, box = FALSE, plg = list(size = c(.5, .5), horiz = TRUE),
               grid = FALSE,
               main = main)


   terra::plot(over %>% as("SpatVector"), box = FALSE, axes = FALSE, add = TRUE)

 }


 if(terra::ncol(dd[[1]]) > terra::nrow(dd[[1]])){
   nrow <- 3
 }else{
   nrow <- 1
 }

 layout(matrix(1:3, nrow = nrow))

  plot(dd[[1]], pal1)
  plot(dd[[2]], pal1)
  plot(dd[[3]], pal2)

  layout(1)

}
