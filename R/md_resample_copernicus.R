

#' Modèle de rééchantillonnage des rasters Copernicus
#'
#' @param dalle numéro de la dalle
#'
#' @return
#' @export
#'

md_resample_copernicus <- function(dalle){

  df_cop <- data_table_copernicus()

  dalle_ <- dalle
  rs <- rast(df_cop %>% filter(dalle == dalle_) %>% pull(path))

  lst <- md_large_echantillon(rs, an = as.numeric(unique(df_cop$an)), nspl=40, larg_grappe = 200)


  library(randomForest)


  ls10 <- purrr::map(lst, function(.x){
    rx <- .x %>% aggregate(10)
    rsx <- rs %>% project(rx)
    c(rx, rsx)
  })

  # supprime jour et mois des dates spot pour que les champs soient identiques
  fun2df <- function(.x){
    df <- as.data.frame(.x)
    names(df) <- stringr::str_replace_all(names(df), "-[0-9]{2}-[0-9]{2}_", "_")
    df
  }


  df0 <- do.call(rbind, purrr::map(ls10, fun2df)) %>% na.omit() %>% filter(mnh_h > 5)
  names(df0) <- stringr::str_replace_all(names(df0), "-", "_")


  result <- purrr::map(names(rs), function(y_){

    y_2 <- stringr::str_replace_all(y_, "-", "_")

    qq <- .99 # exclusion des valeurs extrêmes
    df <- df0 %>% mutate(y = !!as.name(y_2)) %>% select(-any_of(names(rs) %>% stringr::str_replace_all("-", "_"))) %>%
      filter(y < quantile(y, qq))


    set.seed(1)

    yn <- strsplit(y_, " |-")[[1]]
    an <- yn[grep("20", yn)]

    starts <- stringr::str_split(names(df), "_", simplify = TRUE)[,1] %>% unique()
    starts <- c(starts[!starts %in% c("spot", names(rs))], paste0("spot_", an), "y")


    p <- df %>%
      select(starts_with(starts)) %>%
      sample_n(2000)


    x_sel <- md_metrics_selection(p, "y")
    p2 <- p %>% select(any_of(c("y", x_sel)))


    rf <- randomForest(y ~ ., data = p2)

    message(crayon::red("% expliqué: ", round(rf$rsq %>% max() *100)), "\n",
            "Variables non retenues: ", crayon::silver(paste(names(p)[! names(p) %in% x_sel], collapse = " + ")))
    util_console_table(rf$importance %>% as.data.frame() %>%
                         mutate(var = rownames(.),
                                importance = util.round(IncNodePurity)) %>%
                         select(-IncNodePurity) %>% arrange(desc(importance, dea)),
                       ali = "right")


    # validation

    set.seed(554)

    pv <- df %>% sample_n(2000)

    valid <- data.frame(
      pred = predict(rf, pv),
      reel = pv$y
    )

    sub <- paste(y_, " R2 = ", round((lm(pred~reel, data = valid) %>% summary())$r.squared, 2))

    plot(valid$pred, valid$reel, main = y_, sub = sub, xlab = paste("qq =", qq))


    list(rf = rf, valid = valid)
  })


  names(result) <- paste0(names(rs), "_", dalle)

  dir <- file.path(dc("dos_modeles_oiseau"), "resample_copernicus")
  Sys.umask(0)
  if(!dir.exists(dir)) dir.create(dir)

  path <- file.path(dir, paste0(dalle, ".rds"))

  saveRDS(result, path)

  message("Modèles sauvegardés sous ", path)



}
