
#' Prédiction appliquée sur les couronnes
#'
#' @param rr objet de classe ResampleResult issu par ex de md_mlr3()
#' @param dir dossier du fichier
#' @param res resolution des rasters d'extraction (defaut tiré du nom du fichier ...-res1-... = 1m)
#' @param spot_an date de l'image spot uitilisée (defaut tiré du nom du fichier ...-spot2018-... = 2018)
#' @param rsp_coper TRUE pour rééchantilloner les rasters Coprenicus
#' @param dest
#'
#' @return spatraster
#' @export
#'
#'
md_predict_crowns <- function(rr,
                              dir = dc("dos_modeles_oiseau"),
                              path_predicteurs = file.path(dc("dos_projet"), "pile_essences.tif"),
                              res = 1, spot_an = NULL,
                              rsp_coper = FALSE,
                              dest = dc("path_essences")){


  # mn <- "rf_ESS-res1-spot2018"


  path_backup <- file.path(dc("dos_user"), "data.rds")

  if(file.exists(path_backup)){
    data <- readRDS(path_backup)
  }else{
    data <- util_extract(pile, "cr")
    saveRDS(data, path_backup)
  }
  # dh

  if("mnh_h0_q90" %in% tav$vars){
    data$mnh_dh <- data$mnh_h_q90 - data$mnh_h0_q90
  }


  tmp <- file.path(dc("dos_user"), "rf_ess")
  dir.create(tmp)

  lsr <- purrr::map(1:length(mx), function(i_model){


    tmpi <-  file.path(tmp, paste0(i_model, ".tif"))


    # if(file.exists(tmpi)){return(NULL)}

    message("prédiction Random Forest...")
    m <- mx[[i_model]]

    data$pred <- predict(m, data) %>% as.character()

    x <- terra::as.data.frame(pile$cr, na.rm = FALSE) %>%
      left_join(data, by = "cr")


    message("rasterisation des résultats...")


    templ <- uRast("mnt")
    templ[!is.na(templ)] <- NA

    terra::values(templ) <- x$pred

    templ <- util_rename_spatrast(templ, names(mx)[i_model])
    writeRaster(templ, tmpi, overwrite = TRUE)
    write.csv(cats(templ)[[1]], str_replace_all(tmpi, ".tif", ".csv"), row.names = FALSE)
    gc()
  })


  r <- rast(list.files(tmp, full.names = TRUE, pattern = ".tif"))

  names(r) <- names(mx)

  lvs <- map(list.files(tmp, full.names = TRUE, pattern = ".csv"), ~ read.csv(.x))

  levels(r) <- lvs

  df <- as.data.frame(r, na.rm = FALSE) %>%
    mutate_all(as.character) %>%
    mutate(ess = case_when(
      famille == "RES" ~ res,
      het == "HET" ~ "HET",
      TRUE ~ af
    )) %>% mutate(ess = as.factor(ess))


  table(df$ess)

  ess <- r$famille

  cats(ess)
  values(ess) <- df$ess

  levels(ess) <- data.frame(
    value = 1:length(levels(df$ess)),
    label  = levels(df$ess),
    stringsAsFactors = F
  )



  cat_ess <- read.csv(file.path(system.file(package = "oiseauData"), "tables", "cat", "essences.csv"),
                      stringsAsFactors = FALSE)

  coltab(ess) <- cats(ess)[[1]] %>% left_join(cat_ess, by = c("label"="type")) %>%
    select(value, couleur_groupe)


  writeRaster(ess, dest, overwrite = TRUE)
  write.csv(cats(ess)[[1]], paste0(tools::file_path_sans_ext(dest), ".csv"), row.names = FALSE)


  message("raster écrit sous ", dest)


  unlink(tmp, recursive = TRUE)
  ess
}
