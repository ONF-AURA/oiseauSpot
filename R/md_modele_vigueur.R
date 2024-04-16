#' Création modèle croissance / hauteur
#'
#'
#' @param path_calibr chemin du fichier de calibration
#'
#' @return écrit modele_vigueur.rds
#' @export
#'
md_modele_vigueur <- function(
    path_calibr = "/var/partage/oiseau/modeles/calib_fertilite.rds"
){

  data <- readRDS(path_calibr)

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

  by <- "all"

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



  saveRDS(ls_lm,
          file.path(path_calibr %>% dirname(),
                    "modele_vigueur.rds"
          ))


  ggplot2::ggplot(gg, ggplot2::aes(x=h, y=dh, col=ess)) +
    ggplot2::geom_line()
}
