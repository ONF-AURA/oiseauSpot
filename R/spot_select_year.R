#' Image SPOT d'une année donnée
#'
#' @param an muneric
#' @param path_spot_ts chemin de la série temporelle spot de l'emprise du projet
#' @param spot_best_day MM-DD jour préférentiel
#' @param ifnull si non trouvée, chercher l'année précédente ("prev") ou suivante ("next") ou rien ("nothing")
#'
#' @return spatraster
#' @export
#'
spot_select_year <- function(an,
                             path_spot_ts = oiseauData::data_conf("path_spot_ts"),
                             spot_best_day = dataOiseau::data_conf("spot_best_day"),
                             ifnull = c("prev", "next", "nothing")){

  spots <- terra::rast(path_spot_ts)

  dates <- terra::time(spots) %>% as.Date()
  ans <- as.numeric(format(dates, "%Y"))

  # Choix de l'année --------------------------------------------------------

  if(! an %in% ans){
    if(isnull == "prev"){
      add <- (-1)
    }else if(isnull == "next"){
      add <- 1
    }else{
      add <- 0
    }

    if(! an %in% ans){
      an <- an + add

      if(! an %in% ans){
        message("Pas d'image trouvée en ", paste(c(an, an-add) %>% unique(), collapse = " ou en "))
        return(NULL)
      }else{
        message("Pas d'image en ", an - add, ". L'image de ", an, " est utilisée.")
      }
    }

  }

  # choix de la date -----------------------------------------

  best_date <- paste0(an, "-", spot_best_day) %>% as.Date()


  diff_days <- abs(dates - best_date) %>% as.numeric()

  spot_an <- spots[[which(diff_days == min(diff_days))]]
  names(spot_an) <- terra::varnames(spot_an)

  spot_an
}
