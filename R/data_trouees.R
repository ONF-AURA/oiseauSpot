#' Cartographie des trouées
#'
#' @param mnh spatraster du MNH
#' @param ext sf polygone de la zone à cartographier
#' @param h_min hauteur à partir du sol où évaluer le couvert
#' @param min_size taille minimale des trouées à cartographier en m2
#' @param cvt_max couvert maximum (%) à l'échelle de l'are des peuplements ouverts
#'
#' @return spatraster
#' @export
#'
data_trouees <- function(mnh, ext, h_min = 3, min_size = 300, cvt_max = 10){


  bin <- mnh
  bin[mnh > h_min] <- 0
  bin[mnh <= h_min] <- 1

  are <- terra::focal(bin, 9, fun = mean, na.rm = TRUE)
  terra::plot(are, main = "Taux d'ouverture sur un rayon de 10m")

  are[are < (100 - cvt_max)/100] <- NA

  trouees_id_all <- terra::patches(are) %>%
    terra::mask(ext %>% as("SpatVector"))

  layout(matrix(c(1:3), nrow = 1))

  terra::plot(trouees_id_all,
              main = paste("Trouées où le couvert n'excède pas", cvt_max, "%"),
              sub = "Toutes surfaces")

  size <- terra::as.data.frame(trouees_id_all) %>%
    dplyr::mutate(id = dplyr::last(.)) %>%
    dplyr::count(id) %>%
    dplyr::filter(n >= min_size)


  trouees_id <- trouees_id_all
  trouees_id[!  terra::values(trouees_id) %in% size$id] <- NA
  trouees_id <- terra::patches(trouees_id)


  cnt <- trouees_id
  cnt[!is.na(cnt)] <- 1
  cnt[is.na(cnt)] <- 0

  terra::plot(mnh, col = colorRampPalette(c("white", "red"))(20),
              main = paste("Trouées de surface >", min_size, "m2"))
  terra::contour(cnt, add = TRUE)

  layout(1)

  trouees_id
}
