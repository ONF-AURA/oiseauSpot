#' Formules d'indices de végétation SPOT
#'
#' @param indice nom de l'indice. Voir https://www.indexdatabase.de/db/s-single.php?id=173
#' @param list TRUE pour renvoyer la liste des indices disponibles
#'
#' @return texte de la formule
#' @export
#'
spot_formula <- function(indice = c("ndvi", "gli", "logr", "bai", "savi", "lai"), list = FALSE){
  ndvi <- "(spot$ir - spot$red) / (spot$ir + spot$red)"
  vigreen <- "(spot$green - spot$red) / (spot$green + spot$red)"
  ndvigreen <- "(spot$green - spot$ir) / (spot$green + spot$ir)"

  vari <- "(spot$green - spot$red) / (spot$green + spot$red - spot$blue)"
  gli <- "(2 * spot$green - spot$red - spot$blue) / (2 * spot$green + spot$red + spot$blue)"
  logr <- "log(spot$ir/spot$green)"
  bai <- "1/((0.1 - spot$red)^2 + (0.06 - spot$ir)^2)"
  savi <- "(spot$ir - spot$red) / (spot$ir + spot$red + 0.5) + 1.5"

  lai <- "10^(3.4838 * (spot$ir - spot$red) / (spot$ir + spot$red) - 0.148)"

  if(list) return(c("lai", "ndvi", "ndvigreen", "vigreen", "vari", "gli",  "logr", "bai", "savi"))
  get(indice)
}

