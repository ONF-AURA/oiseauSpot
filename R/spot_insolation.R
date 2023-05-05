#' Raster d'insolation
#'
#' @param mnh liste de SpatRaster de hauteur (séries temporelles)
#' @param mnt MNT
#' @param ext sf polygone de la zone à cartographier
#' @param h_min hauteur à partir du sol où évaluer le couvert
#'
#' @return liste de SpatRasetr d'insolation globale en J/m2
#' @export
#'
spot_insolation <- function(mnh, mnt, ext, h_min = 3){

  mnt <- mnt %>% terra::crop(mnh)

  ls <- purrr::map(1:length(mnh), function(n){

    message("Insolation AN ", n, "...")

    insol <- spot_sun(spot_spat2rast(mnt + mnh[[n]]))

    insol_rege <- terra::rast(insol) %>% terra::crop(mnh[[1]])
    insol_rege[mnh[[n]] > h_min] <- NA
    insol_rege.10 <- terra::aggregate(insol_rege, 10, na.rm = TRUE)

    # synthèse à l'ug

    ug_r <- terra::rasterize(ext %>% as("SpatVector"), insol_rege.10, field = "ug")

    tab_enso_ug <- data.frame(
      ug = terra::values(ug_r) %>% as.numeric,
      inso = terra::values(insol_rege.10) %>% as.numeric,
      stringsAsFactors = FALSE) %>%
      na.omit %>%
      dplyr::group_by(ug) %>%
      dplyr::summarise(insol.mean = mean(inso, na.rm = TRUE),
                insol.sd = sd(inso, na.rm = TRUE),
                area = dplyr::n()) %>%
      dplyr::mutate(ymin = insol.mean - insol.sd,
             ymax = insol.mean + insol.sd,
             ug = terra::levels(ug_r)[[1]]) %>%
      dplyr::arrange(insol.mean) %>%
      dplyr::left_join(ext %>%
                  mutate(area_ug = st_area(.) %>% as.numeric) %>%
                  as.data.frame %>% dplyr::select(ug, area_ug),
                by = "ug") %>%
      dplyr::mutate(pc = area / area_ug)

    scale <- max(tab_enso_ug$ymax) / max(tab_enso_ug$pc)

    plot_ug <- ggplot(tab_enso_ug %>% mutate(UG = factor(ug, levels = tab_enso_ug$ug)),
                      aes(x = UG, y = insol.mean, ymin = ymin, ymax = ymax))+
      geom_col(mapping = aes(y = pc * scale), fill = "#999999")+
      geom_boxplot()+
      geom_errorbar()+
      geom_text(aes(y = 5, label = UG), angle = 90, hjust = 0, color="black") +
      scale_y_continuous(sec.axis = ggplot2::sec_axis(~./scale*100, name="surface ouverte (%)"))+
      theme(axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank())

    return(list(
      insol_rege = insol_rege,
      tab_enso_ug = tab_enso_ug,
      plot_ug = plot_ug
    ))
  })

  names(ls) <- names(mnh)

  return(ls)
}

