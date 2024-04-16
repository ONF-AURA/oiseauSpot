#' Récolte de données de calibration pour référence de fertilité
#'
#' A executer au sein de projets utilisés comme référence
#'
#' @param path_calibr chemin du fichier de calibration
#'
#' @return test des modèles d'accroissement de référence
#' @export
#'
md_calibr_fertilite <- function(
    path_calibr = "/var/partage/oiseau/modeles/calib_fertilite.rds",
    add = TRUE,
    spat_filter = "CCOD_FRT",
    by = "all",
    plot = FALSE){



  ess <- data_crowns_essences()

  mnh <- uRast("mnh", origine = "-spot")

  dates <- uDates() %>% filter(var == "mnh" & origine != "spot") %>% pull(date) %>% as.Date()

  diftime <- (dates[2] - dates[1]) %>% as.numeric() / 365

  cr <- uRast("crowns", "last")

  # filtre des arbres selon leur environnement

  mnhh <- mnh[[1]]
  mnhh[is.na(mnhh)] <- 0

  # élimination des arbres isolés: au moins 80% de couvert
  cv <- mnhh
  cv[cv<dc("lim_h_rege")] <- 0
  cv[cv > 0] <- 1
  cv20 <- cv %>% aggregate(20) %>% project(mnh[[1]])

  # élimination des arbres dominés (les petits sont statistiquement plus dominé que les grands)
  dom <- mnhh %>% aggregate(5, na.rm = TRUE) %>%
    focal(5, fun = quantile, probs = .95, na.rm = TRUE) %>%
    project(mnh[[1]])


  # pile ------------

  r <- c(cr$id, ess, cv20, dom, mnh)

  names(r) <- c("id", "ess", "cv", "dom", "h0", "h1")

  # filtre_ess <- c("EPC", "S.P", "HET")

  filtre_ess <- by
  if(by == "all") filtre_ess <- cats(r$ess)[[1]][,2]

  data <- as.data.frame(r, na.rm = FALSE) %>%
    group_by(id) %>%
    dplyr::summarise(h0 = quantile(h0, .95, na.rm = TRUE),
                     h1 = quantile(h1, .95, na.rm = TRUE),
                     ess = unique(ess),
                     cv = mean(cv, na.rm = TRUE),
                     dom = quantile(dom, .95, na.rm = TRUE)) %>%
    dplyr::filter(h1 >= dc("lim_h_rege")) %>%
    dplyr::filter(ess %in% filtre_ess) %>%
    dplyr::mutate(acc = (h1 - h0) / diftime)

  data$frt <- dc("shp")[[spat_filter]] %>% unique() %>% paste(collapse = "_")

  # if(add){
  #   old <- readRDS(path_calibr)
  #
  #   data <- rbind(old, data)
  #
  #   saveRDS(data, path_calibr)
  # }

  lc <- 5

  data00 <- data %>% dplyr::filter(acc>.05 & acc<1) %>% # & cv > .8 & h1 > dom) %>%
    mutate(hc = cut(h1, c(0:(60/lc)) * lc))


  # ggplot(data00, aes(x = hc, y = acc)) + geom_boxplot()

  # Les petits sont statistiquement en fert moyenne et les grands en fert fort
  # La référence est prise sur les plus forts accroissmenet de chaque classe de hauteur

  data0 <- data00 %>%
    group_by(hc) %>%
    summarise(n = n(),
              dhm = mean(acc),
              dh = quantile(acc, .5)) %>%
    mutate(h = as.numeric(hc) * lc + lc/2) %>%
    filter(n > 100)


  # plot(data0$dh~data0$h, ylim = c(0, 1), type = "l", col="red")
  # lines(data0$dhm~data0$h, col = "gray")
  # lines(data0$n/sum(data0$n)~data0$h, col = "gray")


  ess_liste0 <- data$ess %>% unique() %>% as.character()

  if(by=="all"){

    ess_liste <- list(all = ess_liste0)

  }else{

    ess_liste <- ess_liste0 %>% as.list()

  }
  # courbe d'accroissement référence -----------------------------

  ls_lm <- purrr::map(ess_liste, function(e){
    data_e <- data00 %>% filter(ess %in% e)
    glm(acc ~ h1 + I(h1^2), data = data_e)
  })


  gg <- purrr::map_df(1:length(ls_lm), function(e){

    lm <- ls_lm[[e]]
    coef <- lm$coefficients

    hpot_ref <-   (-coef[2] - (coef[2]^2 - 4 * coef[1] * coef[3])^0.5)/(2 * coef[3])

    data.frame(dh = predict(lm, data.frame(h1 = 1:80))) %>%
      mutate(h = 1:80) %>% mutate(ess = e)

  })

  ggplot2::ggplot(gg, ggplot2::aes(x=h, y=dh, col=ess)) +
    ggplot2::geom_line()

  # df <- purrr::map_dfc(ls_lm, ~predict(.x, data.frame(h1 = 1:40))) %>%
  #   mutate(h = 1:40) %>%
  #   tidyr::pivot_longer(-h, names_to = "ess", values_to = "dh")
  #
  # ggplot2::ggplot(df %>% filter(ess %in% c("S.P", "EPC", "HET")), ggplot2::aes(x=h, y=dh, col=ess)) +
  #   ggplot2::geom_line()

  # validation


  # ls_val <- purrr::map(ess_liste, function(e){
  #
  #   data_val <- data0 %>% slice(-e)
  #
  #   data_e <- data_val %>% dplyr::filter(ess == e)
  #   data_e$pr <- predict(ls_lm[[e]], data_e)
  #
  #   rmse <- mean(abs((data_e$pr^2 - data_e$acc^2))^0.5) / mean(data_e$acc)
  #   plot(data_e$pr ~ data_e$acc)
  #
  # })

  if(plot){

    plot_lm <- function(data_pre0){



    }

    gg <- purrr::map_dfr(unique(data_pre$frt), function(f){
      data_pre0 <- data_pre %>% filter(frt == f)
      plot_lm(data_pre0)
    })


    ggplot2::ggplot(df %>% filter(ess %in% c("S.P", "EPC", "HET")), ggplot2::aes(x=h, y=dh, col=ess)) +
      ggplot2::geom_line()
      ggplot2::facet_wrap(vars(frt))


      df <- purrr::map_dfc(ls_lm, ~predict(.x, data.frame(h1 = 1:40))) %>%
        mutate(h = 1:40) %>%
        tidyr::pivot_longer(-h, names_to = "ess", values_to = "dh")

      ggplot2::ggplot(df %>% filter(ess %in% c("S.P", "EPC", "HET")), ggplot2::aes(x=h, y=dh, col=ess)) +
        ggplot2::geom_line()
      ggplot2::facet_wrap(vars(frt))
  }

  if(lm_only){
    return(ls_lm)
  }

  # Prédiction ----------------------------------

  data$pred <- NULL

  for(e in names(ls_lm)){
    if(e == "all"){
      e2 <- unique(data$ess)
    }else{
      e2 <- e
    }
    data[which(data$ess %in% e2), "pred"] <- predict(ls_lm[[e]], data[which(data$ess %in% e2),])
  }

  data$fert <- data$acc / data$pred

  data_pix <- as.data.frame(r, na.rm = FALSE) %>%
    left_join(data %>% select(id, fert), by = "id")

  data_pix$fert[which(data_pix$fert < 0)] <- NA

  rfert <- r$id
  values(rfert) <- data_pix$fert
  names(rfert) <- "fertilite"

  writeRaster(rfert, file.path(dc("dos_modeles"), "fertilite.tif"), overwrite = TRUE)
  visu_add_raster(c(rfert %>% aggregate(10, na.rm=TRUE)), compare = TRUE)

  rfert
}
