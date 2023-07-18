#' Formules d'indices de végétation SPOT
#'
#' @param indice nom de l'indice. Voir https://www.indexdatabase.de/db/s-single.php?id=173
#'
#' @return texte de la formule
#' @export
#'
spot_formula <- function(indice){
  ndvi <- "(spot$ir - spot$red) / (spot$ir + spot$red)"
  gli <- "(2*spot$green−spot$red−spot$red) / (2*spot$green+spot$red+spot$blue)"
  logr <- "log(spot$ir/spot$green)"

  get(indice)
}
spot_formula("ndvi")
