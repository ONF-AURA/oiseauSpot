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

md_crowns_classify <- function(ncat = NULL,
                               rm_size = FALSE,
                               dest_file = "class_crowns.tif",
                               path_crowns = data_conf("path_crowns_ts"),
                               dos_metrics = data_conf("dos_modeles"),
                               file_metrics = "crowns_metrics_.*.rds",
                               dest_dos = dc("dos_spot"),
                               plot = FALSE,
                               methode = "kmeans"
){

  if(is.null(ncat)){
    util_log("md_crowns_classify", "ncat obligatoire pour methode kmeans")
    return("ko")
  }


  cr <- uRast("crowns")

  metrics <- list.files(dos_metrics, pattern = file_metrics, full.names = TRUE)

  if(length(metrics) == 0){
    util_log("md_crowns_classify", "Les métriques des couronnes ne sont pas disponibles. Exécutez préalablement md_crowns_metrics.")
    return("ko")
  }

  if(length(metrics) > 1){
    metrics = utils::select.list(metrics, title = "Plusieurs fichiers de métriques disponibles:")
  }

  tryCatch({
    data <- readRDS(metrics)
  },
  error = function(e){
    util_log("md_cronws_classify", "impossible d'ouvrir", metrics, ":\n", e)
    return("ko")
  })


  new_data <- data %>% na.omit()

  if(rm_size){
    new_data <- new_data %>%
      # dplyr::mutate(hpp = h1/h5) %>%
      dplyr::select(-dplyr::any_of(c("area", "hmax", "h5", "h1")))
  }

  rm <- md_rm_corr_var(new_data %>% select(-c(id)))

  data_sel <- new_data %>% select(-rm)

  ls_cats <- md_new_classif(data_sel %>% dplyr::select(-id), methode = methode,
                            nb_cat = ncat)


  data_end <- data_sel %>% mutate(cat = ls_cats$cats)
  new_data$cat <- ls_cats$cats

  # Rasterisation ----------------+

  val <- data %>% select(id) %>%
    left_join(data_end %>% select(id, cat), by = "id")

  v <- as.data.frame(cr, na.rm = FALSE) %>% select(id) %>%
    left_join(val, by = "id")

  r <- cr$id
  values(r) <- v$cat %>% as.factor()

  if(plot){
    library(ggplot2)

    gg <- new_data %>%
      select(any_of(c("cat", ls_cats$vars,
                      names(new_data)[
                        which(
                          startsWith(names(new_data), "TPROD") &
                            endsWith(names(new_data), "med")
                        )
                      ]))) %>%
      mutate(cat = as.factor(cat)) %>%
      tidyr::pivot_longer(-cat, names_to = "var", values_to = "val") %>%
      ggplot(aes(x = cat, y = val)) +
      geom_boxplot() +
      facet_wrap(vars(var), scales = "free")
  }else{gg <- NULL}

  # écriture ---------------

  terra::writeRaster(r, file.path(dest_dos, dest_file))

  util_msg(
    paste("Raster écrit sous ",
          file.path(dest_dos, dest_file)),
    notification = TRUE)

  list(r, gg)
}
