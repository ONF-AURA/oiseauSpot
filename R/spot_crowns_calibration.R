#' Création d'un modèle calibré par des points d'observation
#'
#' @param path_points chemin vers le shapefile de points de calibration
#' @param y_name nom du champ de la variable à prédire
#' @param title Nom donné au modèle
#' @param select_vars TRUE pour Utiliser une sélection automatique des variable
#' @param path_crowns chemin du raster des couronnes
#' @param path_spot chemin du rster des images spot
#' @param dest_dos chemin du dossier des modèles
#'
#' @return modèle RF
#' @export
#'
#' @examples
spot_crowns_calibration <- function(path_points,
                                    y_name,
                                    title = "calibration",
                                    select_vars = TRUE,
                                    path_crowns = oiseauData::data_conf("path_crowns_ts"),
                                    path_spot = oiseauData::data_conf("path_spot_ts"),
                                    dest_dos = dc("dos_modeles")
){


  points <- sf::read_sf(path_points)

  if(! y_name %in% names(points)){
    oiseauUtil::util_log("spot_crowns_calibration",
                         paste0("Le champ ", y_name, " n'existe pas dans la couche ", path_points))
    return("ko")
  }

  if(! "hmax" %in% names(cr)){
    oiseauUtil::util_log("spot_crowns_calibration", "Les métriques des couronnes ne sont pas disponibles. Exécutez préalablement spot_crowns_metrics.")
    return("ko")
  }

  cr <- uRast("crowns", path = path_crowns_ts)
  sp <- uRast("spot", path = path_spot)

  if(! "hmax" %in% names(cr)){
    oiseauUtil::util_log("spot_crowns_calibration", "Les métriques des couronnes ne sont pas disponibles. Exécutez préalablement spot_crowns_metrics.")
    return("ko")
  }

  ecr <- terra::extract(cr, points %>% as("SpatVector")) %>%
    dplyr::select(-ID) %>%
    dplyr::mutate(type = pt[[y_name]]) %>%
    dplyr::relocate(type)

  esp <- terra::extract(sp, points %>% as("SpatVector")) %>%
    dplyr::select(-ID)

  e <- cbind(ecr, esp) %>% dplyr::select(-id)

  if(select_vars){
    sel <- spa_select_vars(e %>% dplyr::select(-type), e$type, family = "category")

    oiseauUtil::util_msg("spot_crowns_calibration", paste0("Données sélectionnées : ", paste(names(sel), collapse = " + ")))

  }else{
    sel <- e
  }


  rf <- randomForest::randomForest(y=e$type, x=sel)

  print(rf$confusion)

  saveRDS(rf, file.path(dest_dos, paste0(title, ".rds")))

}
