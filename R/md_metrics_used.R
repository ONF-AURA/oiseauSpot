#' Premier filtre des namess de prédicteurs
#'
#' @param names vecteur de nom de variables
#' @param exclu expressions exclues
#'
#' @return vecteur filtré
#' @export
#'

md_metrics_used <- function(names, exclu = c("mnh_aspect", "flowdir", "mnh_TPI", "spot_.*_q", "moy", "topo_.*q", "topo_.*sd", ".*_x_.*q", ".*_x_.*sd", "TRIrmsd", "TRIriley")){

  coout <- purrr::map(exclu, ~ names[which(stringr::str_detect(names, .x))]) %>% unlist() %>% unique()
  names[which(! names %in% coout)]

}
