#' Classification automatique des couronnes
#'
#' @param ncat nb de types à créer
#' @param rm_size TRUE pour retirer les dimensions (h et surface) des facteurs explicatifs
#' @param dest_file nom du fichier à créer (avec extention .tif)
#' @param path_crowns chemin du raster des couronnes
#' @param path_spot chemin du rster des images spot
#' @param dest_dos dossier d'écriture
#'
#' @return spatRaster de catégories
#' @export
#'

spot_crowns_classify <- function(ncat = 8,
                          rm_size = TRUE,
                          dest_file = "class_crowns.tif",
                          path_crowns = oiseauData::data_conf("path_crowns_ts"),
                          path_spot = oiseauData::data_conf("path_spot_ts"),
                          dest_dos = dc("dos_spot")
                          ){


  cr <- uCrowns(path_crowns)
  sp <- uSpot(path_spot)

  if(! "hmax" %in% names(cr)){
    oiseauUtil::util_log("spot_crowns_classify", "Les métriques des couronnes ne sont pas disponibles. Exécutez préalablement spot_crowns_metrics.")
    return("ko")
  }

  data <- terra::as.data.frame(c(cr, sp %>% terra::resample(cr)))
  data_cr <- data %>%
    dplyr::group_by(id) %>%
    dplyr::summarise_all(mean)


  new_data <- data_cr %>% dplyr::select(-id)

  if(rm_size){
    new_data <- new_data %>%
      dplyr::mutate(hpp = h1/h5) %>%
      dplyr::select(-dplyr::any_of(c("area", "hmax", "h5", "h1")))
  }


  km <- kmeans(new_data, ncat)$cluster

  cat <- data.frame(id = data_cr$id,
                    cat = km,
                    ir = new_data %>% dplyr::select(dplyr::starts_with("ir_")) %>%
                      dplyr::mutate(ir = rowMeans(., na.rm = TRUE)) %>%
                      dplyr::pull(ir))

  # classement des types selon la bande IR spot

  cat_ir <- cat %>% dplyr::group_by(cat) %>%
    dplyr::summarise(ir = mean(ir, na.rm = TRUE)) %>%
    dplyr::arrange(ir) %>%
    dplyr::mutate(cat2 = paste0("T", row.names(.)))


  temp <- terra::as.data.frame(cr$id, na.rm = FALSE)

  val <- temp %>% dplyr::left_join(cat) %>%
    dplyr::left_join(cat_ir, by = "cat")

  rcat <- cr$id
  terra::values(rcat) <- val$cat2

  terra::writeRaster(rcat, file.path(dest_dos, dest_file))

  oiseauUtil::util_msg(
    paste("Raster écrit sous ",
          file.path(dest_dos, dest_file)),
    notification = TRUE)

  rcat
}
