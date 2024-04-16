#' Réechantillonage d'un raster 10m à 1m par régression linéaire
#'
#' @param y10m10 raster numérique de résolution 10m
#' @param an année spot à utiliser
#'
#' @return spatrast 1m
#' @export
#'
#'
md_downscale <- function(y10m10, an = "last", rf = NULL){



  if(is.null(rf)){
    dsp <- util_year2date(an, "spot")  %>% as.character()
    if(dsp[1] == "ok"){
      util_log("md_downscale", paste("Image spot absente: ", an))
    }
    dd <- uDates()


    # --

    x0 <- suppressWarnings(suppressMessages(
      md_predicteurs(res = 1, sentinel = FALSE, copernicus = FALSE, spot_date = an)
    ))

    x <- x0[[md_metrics_used(names(x0))]]

    y1m <- y10m10 %>% project(x)
    y10m <- y10m10 %>% project(x, method = "near")

    names(y1m) <- "y"

    p <- c(y1m, x) %>% as.data.frame() %>%
      na.omit() %>%
      filter(mnh_h > 3) %>%
      sample_n(2000)

    x_sel <- md_metrics_selection(p, "y")
    p2 <- p %>% select(any_of(c("y", x_sel)))

    library(randomForest)

    rf <- randomForest(y ~ ., data = p2)

    message(crayon::red("% expliqué: ", round(rf$rsq %>% max() *100)), "\n",
            "Variables non retenues: ", crayon::silver(paste(names(p)[! names(p) %in% x_sel], collapse = " + ")))
    util_console_table(rf$importance %>% as.data.frame() %>%
                         mutate(var = rownames(.),
                                importance = util.round(IncNodePurity)) %>%
                         select(-IncNodePurity) %>% arrange(desc(importance, dea)),
                       ali = "right")
  }else{

    xn <- rf$importance %>% rownames()

    ind0 <- stringr::str_split(xn[which(startsWith(xn, "spot_"))], "_", simplify = TRUE)
    indices <- ind0[, ncol(ind0)]
    x <- md_predicteurs(res = 1, sentinel = FALSE, copernicus = FALSE,
                        insol = FALSE, dendro = FALSE,
                        spot_date = an,
                        topo = (sum(stringr::str_detect(xn, "topo_")) > 0),
                        spot = (sum(stringr::str_detect(xn, "spot_")) > 0),
                        mnh = (sum(stringr::str_detect(xn, "mnh_")) > 0),
                        vars_topo = stringr::str_remove(xn[which(startsWith(xn, "topo_"))], "topo_"),
                        vars_mnh = stringr::str_remove(xn[which(startsWith(xn, "mnh_"))], "mnh_"),
                        indices = indices
    )
    y1m <- y10m10 %>% project(x)
    y10m <- y10m10 %>% project(x, method = "near")

    names(y1m) <- "y"


  }

  names(x) <- stringr::str_replace_all(names(x), "-", "_")

  message("Prédiction ................")

  # prédit 1m
  y2 <- x[[1]]

  vrf <- rownames(rf$importance)

  # lorsque dates spot au format YY_MM_DD
  bad_names <- vrf[which(!vrf %in% names(x))]

  if(length(bad_names) > 0){
    for(bn in bad_names){
      start <- paste(stringr::str_split(bn, "_")[[1]][1:2], collapse = "_")
      end <- rev(stringr::str_split(bn, "_")[[1]])[1]
      good <- names(x)[which(startsWith(names(x), start) & endsWith(names(x), end))]
      names(x)[which(names(x) == good)] <- bn
    }
  }

  # prédiction
  values(y2) <- predict(rf, x)

  # initial 10m
  dy <- y2/y1m
  dy10m <- aggregate(dy, 10, na.rm = TRUE) %>% project(y1m, method = "bilinear")

  yy <- y1m * dy / dy10m

  names(yy) <- names(y1m)

  return(yy)
}

# names(yy) <- "downscaled"
# names(y1m) <- "ini_smooth"
# names(y10m) <- "ini"
# names(dy10m) <- "dif_smooth"

# e <- dc("shp") %>% st_centroid() %>% st_buffer(50)
# plot(c(y1m, yy) %>% crop(e))
# plot(c(y1m,yy, dy10m) %>% crop(e))

