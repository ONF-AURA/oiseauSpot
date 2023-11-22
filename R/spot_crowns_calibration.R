#' Création d'un modèle calibré par des points d'observation
#'
#' @param path_points chemin vers le shapefile de points de calibration
#' @param y_name nom du champ de la variable à prédire
#' @param title Nom donné au modèle
#' @param select_vars TRUE pour Utiliser une sélection automatique des variable
#' @param path_crowns chemin du raster des couronnes
#' @param path_spot chemin du rster des images spot
#' @param dest_dos chemin du dossier des modèles
#' @param date date du MNH à utiliser
#' @param exclude exclure des prédicteurs les familles dend(rométriques) topo(graphiques) spot insolation mnh area (surface)
#' @param exclude_seuil taux de perte max de placettes tolérée en raison de données absentes avant exclusion de la variable
#' @param dest_name nom du raster en sortie
#'
#' @return modèle RF
#' @export
#'
#' @examples
spot_crowns_calibration <- function(path_points,
                                    y_name,
                                    dest_name = y_name,
                                    title = "calibration",
                                    date = "last",
                                    select_vars = TRUE,
                                    exclude = c("dend"),
                                    exclude_seuil = .05,
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


  cr <- uRast("crowns", path = path_crowns_ts, date = date)

  date <- terra::time(cr) %>% as.character() %>% unique()

  # sp <- uRast("spot")
  #
  # tdif <- terra::time(sp) - terra::time(cr) %>% unique()

  # date_sp <- unique(terra::time(sp)[which(abs(tdif) == min(abs(tdif)))])
  # sp <- sp[[which(terra::time(sp) == date_sp)]]

  # if(! "hmax" %in% names(cr)){
  #   oiseauUtil::util_log("spot_crowns_calibration", "Les métriques des couronnes ne sont pas disponibles. Exécutez préalablement spot_crowns_metrics.")
  #   return("ko")
  # }

  dates_metrics <- list.files(dest_dos, pattern = "crowns_metrics_") %>%
    stringr::str_remove_all(".rds") %>%
    stringr::str_remove_all("crowns_metrics_")

  if(!date %in% dates_metrics){
    message("Les métriques des couronnes ne sont pas disponibles pour le ", date, ".\nVoir spot_crowns_metrics()")
    return("ko")
  }

  preds <- readRDS(file.path(dest_dos, paste0("crowns_metrics_", date, ".rds")))

  e <- terra::extract(cr, points %>% as("SpatVector")) %>%
    dplyr::select(id) %>%
    dplyr::mutate(type = pt[[y_name]]) %>%
    dplyr::left_join(preds, by = "id") %>%
    dplyr::select(-id)

  for(exc in exclude){
    e <- e %>% dplyr::select(!tidyr::starts_with(paste0(exc, "_")))

  }

  nas <- purrr::map_int(e, ~sum(is.na(.x)))
  rms <- names(nas)[which(nas > (nrow(e)*exclude_seuil))]

  if(length(rms) > 0){
    e <- e %>% dplyr::select(-dplyr::all_of(rms))
  }


  # esp <- terra::extract(sp, points %>% as("SpatVector")) %>%
  #   dplyr::select(-ID)

  # e <- cbind(ecr, esp) %>% dplyr::select(-id)

  if(select_vars){

    # stringr::str_split(names(e), "_", simplify = TRUE) %>% as.data.frame()
    nsel <- varSelRF::varSelRF(xdata = e %>% dplyr::select(-type), Class = e$type %>% as.factor())

    oiseauUtil::util_msg("spot_crowns_calibration",
                         paste0("Données sélectionnées : ", paste(nsel$selected.vars, collapse = " + ")))

    sel <- e %>% dplyr::select(nsel$selected.vars)

    nsel <- varSelRF::varSelRF(xdata = sel, Class = e$type %>% as.factor())

  }else{
    sel <- e %>% dplyr::select(-type)
  }


  rf <- randomForest::randomForest(y=e$type %>% as.factor(), x=sel)

  print(rf$confusion)

  path <- file.path(dest_dos, paste0(title, ".rds"))
  saveRDS(rf, path)

  message("modèle écrit sous ", path)

  preds_rf <- preds %>% dplyr::select(c(id, dplyr::all_of(nsel$selected.vars)))

  preds$new <- NA

  preds0 <- na.omit(preds_rf)

  nbNA <- (nrow(preds) - nrow(preds0))

  message(nbNA, " couronne(s), soit ", round(nbNA / nrow(preds) * 100, 4), "% des couronnes n'ont pas de prédiction en raison de données manquantes")

  new <- predict(rf, preds0)

  preds$new[preds$id %in% preds0$id] <- new

  tab <- data.frame(id = terra::values(crowns$id)) %>%
    dplyr::left_join(preds %>% dplyr::select(id, new))

  nw <- crowns$id
  terra::values(nw) <- NA

  terra::values(nw) <- tab$new


  fp <- file.path(dest_dos, paste0("crowns_", dest_name, ".tif"))
  writeRaster(nw, fp, overwrite = TRUE)

}
