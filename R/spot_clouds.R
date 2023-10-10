#' Masque de nuage d'une image SPOT
#'
#' @param path Chemin du tif spot
#'
#' @return sf avec champ maskType (CLOUD ou SNOW) ou NULL (pas de masque dispo) ou "ko" (erreur)
#' @export
#'

spot.clouds <- function(path){

  dir <- (strsplit(path, "/")[[1]] %>% rev())[-(1:3)] %>%
    rev() %>%
    paste(collapse = "/")

  vls <- purrr::map(list(nuages = "CLD_", neige = "SNW_"),

                     function(x){

                       file <- list.files(dir, pattern = x, recursive = TRUE, full.names = TRUE)
                       file <- file[which(endsWith(file, "GML"))]

                       if(length(file) > 1){
                         message("Erreur: plusieurs fichiers de masque ,", x," pour ", basename(path))

                         return(NULL)
                       }


                       if(length(file) == 0){
                         message("Pas de masque", x ," pour ", basename(path))

                         return(NULL)

                       }
                       v <- tryCatch(sf::read_sf(file),
                                     error = function(e){NULL})
                       if(is.null(v)){
                         # message("Erreur à la lecture du masque ", x, " de ", basename(path))

                         return(NULL)

                       }else{
                         v
                       }
                     }
  )

  vls0 <- vls[which(!sapply(vls,is.null))]

  if(length(vls0) == 0) return(NULL)

  if(length(vls0) == 1) return(vls0[[1]])

  do.call(rbind, vls0)

}


