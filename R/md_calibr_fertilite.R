#' Récolte de données de calibration pour référence de fertilité
#'
#' A executer au sein de projets utilisés comme référence
#'
#' @param path_calibr chemin du fichier de calibration
#'
#' @return test des modèles d'accroissement de référence
#' @export
#'
md_calibr_fertilite <- function(path_calibr = "/var/partage/oiseau/modeles/calib_fertilite.rds"){


  path <- list.files(dc("dos_modeles"), pattern = "ess_detail", full.names = TRUE)
  path <- path[which(endsWith(path, ".tif"))]

  if(length(path) == 0){
    data_crowns_essences()
  }

  path <- list.files(dc("dos_modeles"), pattern = "ess_detail", full.names = TRUE)
  path <- path[which(endsWith(path, ".tif"))]

  ess <- terra::rast(path)




  mnh <- uRast("mnh", origine = "-spot")

  dates <- uDates() %>% filter(var == "mnh" & origine != "spot") %>% pull(date) %>% as.Date()

  diftime <- (dates[2] - dates[1]) %>% as.numeric() / 365

  cr <- uRast("crowns", "last")

  # pile ------------

  r <- c(cr$id, ess, mnh)

  names(r) <- c("id", "ess", "h0", "h1")

  data <- as.data.frame(r, na.rm = FALSE) %>%
    group_by(id) %>%
    dplyr::summarise(h0 = quantile(h0, .95, na.rm = TRUE),
                     h1 = quantile(h1, .95, na.rm = TRUE),
                     ess = unique(ess)) %>%
    dplyr::mutate(acc = (h1 - h0) / diftime)

  data$frt <- dc("shp")$CCOD_FRT %>% unique()


  old <- readRDS(path_calibr)

  data <- rbind(old, data)

  saveRDS(data, path_calibr)



  data_pre <- data %>% dplyr::filter(acc>.05 & acc<1)


  plot_lm <- function(data_pre0){

    ess_liste <- data_pre0$ess %>% unique()

    ls_lm <- purrr::map(ess_liste, function(e){

      data_e <- data_pre0 %>% dplyr::filter(ess == e)
      glm(acc ~ h1 + I(h1^2) -1, data = data_e)
    })

    names(ls_lm) <- ess_liste


  purrr::map_dfc(ls_lm, ~predict(.x, data.frame(h1 = 1:40))) %>%
      mutate(h = 1:40) %>%
      tidyr::pivot_longer(-h, names_to = "ess", values_to = "dh") %>%
    mutate(frt = paste(unique(data_pre0$frt), collapse = " "))


  }

  gg <- purrr::map_dfr(unique(data_pre$frt), function(f){
    data_pre0 <- data_pre %>% filter(frt == f)
    plot_lm(data_pre0)
  })


  ggplot2::ggplot(gg %>% filter(!ess %in% c("S.P", "EPC", "HET")), ggplot2::aes(x=h, y=dh, col=ess)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(vars(frt))

}
