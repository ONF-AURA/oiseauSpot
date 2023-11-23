
#' Prédiction appliquée sur les couronnes
#'
#' @param mn nom du modèle: nom du fichier où le modèle est enregistré, sans extension .rds
#' @param dir dossier du fichier
#' @param res resolution des rasters d'extraction (defaut tiré du nom du fichier ...-res1-... = 1m)
#' @param spot_an date de l'image spot uitilisée (defaut tiré du nom du fichier ...-spot2018-... = 2018)
#'
#' @return spatraster
#' @export
#'
#'
md_predict_crowns <- function(mn, dir, res = NULL, spot_an = NULL, dest = file.path(dc("dos_modeles"))){


  # mn <- "rf_ESS-res1-spot2018"
  # dir <- "/var/user/sdumas/oiseauX"

  path <- file.path(dir, paste0(mn, ".rds"))
  # "/var/user/sdumas/oiseauX/rf_ESS-res1-spot2018.rds"


  m <- readRDS(path)

  vars <- m$importance %>% rownames()

  #  paramètres tirés du nom du modèle

  if(is.null(res) | is.null(spot_an)){

    message("extration des parametres du modele depuis son nom de fichier")

    prms <- strsplit(mn, "-") %>% unlist()

    if(is.null(res)){
      if(stringr::str_detect(mn, "-res")){
        res <- stringr::str_remove(prms[which(startsWith(prms, "res"))], "res") %>% as.numeric
      }else{
        res <- 10
      }
    }

    if(is.null(spot_an)){
      if(stringr::str_detect(mn, "-spot")){
        an_spot <- stringr::str_remove(prms[which(startsWith(prms, "spot"))], "spot")
      }else{
        an_spot <- "last"
      }
    }
  }

  #

  crowns <- uRast("crowns","last")

  tav <- stringr::str_split(vars, "_", simplify = TRUE)

  tav[tav == "topo"] <- "mnt"
  tav <- cbind(tav, rep("last", nrow(tav)))
  tav[which(tav[,1] == "spot"), 4] <- an_spot

  tav[tav == "med"] <- "median"
  tav[tav == "moy"] <- "mean"

  tav <- cbind(tav, rep("", nrow(tav)))

  tav[which(startsWith(tav[,3], "q")), 5] <- tav[which(startsWith(tav[,3], "q")), 3] %>% stringr::str_remove("q")
  tav[which(startsWith(tav[,3], "q")), 3] <- "quantile"

  ls <- purrr::map(1:nrow(tav), function(i){
    ta <- tav[i,]
    names(ta) <- c("origine", "derive", "fun", "date", "param")

    message("construction du raster prédictif ", toupper(paste(ta[1:2], collapse = " ")))

    if(ta["origine"] == "bdforet"){
      return(data_bdforet(force=F))
    }

    assign(ta["origine"], uRast(ta["origine"], date = ta["date"])) %>% terra::aggregate(res)


    if(ta["origine"] == "spot"){

      if(ta["fun"] == "quantile"){
        fun <- function(x){quantile(x, ta["param"])}
      }else{
        fun <- get(ta["fun"])
      }
      print(spot_formula(ta["derive"]))
      return(eval(parse(text = spot_formula(ta["derive"]))))

    }else if(ta["origine"] %in% c("mnt", "mnh")){

      if(ta["derive"] %in% c("h", "alti")){
        return(get(ta["origine"]))
      }else{
        return(terra::terrain(get(ta["origine"]), ta["derive"]))
      }
    }else if(ta["origine"] %in% c("dendro")){

      return(get(ta["origine"])[[ta["derive"]]])
    }else{

      return(get(ta["origine"]))
    }
  })

  names(ls) <- vars

  pile <- do.call(c, ls) %>% terra::rast()
  pile$cr <- crowns

  # data <- terra::as.data.frame(pile, na.rm = TRUE) %>%
  #   group_by(cr) %>%
  #   summarise(
  #     across(ends_with("med"), median, na.rm = TRUE),
  #     across(ends_with("q10"), quantile, probs = .1, na.rm = TRUE),
  #     across(ends_with("q90"), quantile, probs = .9, na.rm = TRUE),
  #     across(ends_with("moy"), mean, probs = .1, na.rm = TRUE),
  #     across(ends_with("sd"), sd, na.rm = TRUE)
  #   )



  crv <- crowns %>% terra::as.polygons() %>% st_as_sf()
  sf::st_crs(crv) <- 2154
  #
  # ls_e <- purrr::map(1:nrow(tav), function(.x){
  #
  #   message("Extraction ", toupper(paste(tav[.x,], collapse = " ")))
  #   rast <- ls[[.x]]
  #   terra::crs(rast) <- "epsg:2154"
  #   exactextractr::exact_extract(rast, crv,
  #                                fun = tav[.x, 3],
  #                                quantiles = ifelse(tav[.x,5] == "", 0, as.numeric(tav[.x,5]) / 100))
  # })
  #
  # names(ls_e) <- vars

  # x <- do.call(cbind, ls_e) %>% as.data.frame()

  data <- util_extract(pile, "cr")


  message("prédiction Random Forest...")

  library(randomForest)

  data$pred <- predict(m, data) %>% as.character()

  x <- terra::as.data.frame(pile$cr, na.rm = FALSE) %>%
    left_join(data, by = "cr")




  destfile <- file.path(dest, paste0(tools::file_path_sans_ext(stringr::str_remove(mn, "rf_")), ".tif"))

  message("rasterisation des résultats...")


  templ <- uRast("mnt")
  templ[!is.na(templ)] <- NA

  terra::values(templ) <- x$pred

  writeRaster(templ, destfile, overwrite = TRUE)
  write.csv(cats(templ)[[1]], paste0(tools::file_path_sans_ext(destfile), ".csv"), row.names = FALSE)

  # rpred <- terra::rasterize(crv %>% as("SpatVector"), templ, field = "pred",
  #                           filename = destfile,
  #                           overwrite = TRUE
  #                           )

  message("raster écrit sous ", destfile)

  templ
}
