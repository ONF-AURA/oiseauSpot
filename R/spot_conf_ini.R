#' Configuration par défaut d'un projet pour le calcul du couvert
#'
#' @param .dir dossier du projet
#' @param replace TRUE pour réinitialiser les paramètres
#'
#' @return
#' @export
#'
spot_conf_ini <- function(
    .dir = mget(".dir", envir = as.environment(1), ifnotfound = list(NULL))[[1]],
    replace = FALSE
){

  ls <- list(
    ls_ok = c(),
    ls_ko = c()
  )

  dcs <- function(p, set = NULL, descr, ls0 = ls, process = process, replace = TRUE){

    if(is.null(set)){
      return(oiseauData::data_conf(p, silent = TRUE))
    }

    if(!is.null(oiseauData::data_conf(p, silent = TRUE))){
      ls0$ls_ko <- c(ls0$ls_ko, p)
    }else{
      oiseauData::data_conf(p, set = set, process = process, replace = replace, descr = descr, silent = TRUE)
      ls0$ls_ok <- c(ls0$ls_ok, p)
    }
    return(ls0)
  }



  ls <- dc("dos_spot", set = "/var/partage2/spot6",
     process = "spot", replace = TRUE,
     descr = "chemin du dossier contenant les archives .tar.gz des images SPOT6/7")
  ls <-  dc("dos_data_spot", set = file.path(dc("dos_projet"), "data_spot"),
     process = "spot", replace = TRUE,
     descr = "chemin du dossier de travail de traitement des images spot")
  ls <- dc("dos_mnh_ts", set = file.path(dc("dos_projet"), "mnh_ts"),
     process = "spot",  replace = TRUE,
     descr = "chemin du dossier des séries temporelles du MNH")
  ls <- dc("path_mnh_ts", set = file.path(dc("dos_mnh_ts"), "mnh_ts.tif"),
     process = "spot",  replace = TRUE,
     descr = "chemin du raster des séries temporelles du MNH depuis la dernière mesure physique")
  ls <- dc("path_mnh_mask_ts", set = file.path(dc("dos_mnh_ts"), "mnh_mask_ts.tif"),
     process = "spot",  replace = TRUE,
     descr = "chemin du raster des séries temporelles du masque appliqué au MNH an1")
  ls <- dc("seuil_diff_spot", set = 2,
     process = "spot",  replace = TRUE,
     descr = "seuil de différence des indices d'images Spot à partir duquel la couronne est éliminée")

  # if(length(ls$ls_ok) > 0){
  #   message(length(ls$ls_ok), " paramètres ont été configurés par défaut: ", paste(ls$ls_ok, collapse = ", "))
  # }
  #
  # if(length(ls$ls_ko) > 0){
  #   message(length(ls$ls_ko), " paramètres étaient déjà configurés et n'ont  pas été modifié: ", paste(ls$ls_ko, collapse = ", "))
  # }

  message("Paramètres Spot initialisés avec succès.")
}
