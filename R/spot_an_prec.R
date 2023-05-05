#' Année précédente disponible la plus proche d'une année donnée
#'
#' @param n liste des noms des rasters spot
#' @param an année de référence
#'
#' @return integer
#' @export
#'
spot_an_prec <- function(n, an){
  annees_dispo <- as.numeric(stringr::str_sub(n, 5, 8)) %>% sort
  an_pos <- which(annees_dispo == an)

  if(an_pos == 1){
    stop(paste("La liste d'image spot doit comporter une image plus ancienne que l'année étudiée, ici", an, "."))
  }

  annees_dispo[an_pos - 1]
}
