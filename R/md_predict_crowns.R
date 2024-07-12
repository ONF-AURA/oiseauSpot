
#' Prédiction appliquée sur les couronnes
#'
#' @param mn nom du modèle: nom du fichier où le modèle est enregistré, sans extension .rds
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
md_predict_crowns <- function(mn, dir = dc("dos_modeles_oiseau"),
                              res = NULL, spot_an = NULL,
                              rsp_coper = FALSE,
                              dest = dc("path_essences")){


  # mn <- "rf_ESS-res1-spot2018"
  # dir <- "/var/user/sdumas/oiseauX"

  path <- file.path(dir, paste0(mn, ".rds"))

  path_pile <- file.path(dc("dos_projet"), "pile_essences.tif")
  # "/var/user/sdumas/oiseauX/rf_ESS-res1-spot2018.rds"


  mx <- readRDS(path)

  #  paramètres tirés du nom du modèle

  if(is.null(res) | is.null(spot_an)){

    message("extration des parametres du modele depuis son nom de fichier")

    prms <- strsplit(mn, "-") %>% unlist()

    if(is.null(res)){
      if(stringr::str_detect(mn, "-res")){
        res <- stringr::str_remove(prms[which(startsWith(prms, "res"))], "res") %>% as.numeric
      }else{
        util_log("md_predict_crowns", "La résolution ne peut être extraite du nom du fichier rds du modèle: -res non trouvé")
        return("ko")
      }
    }

    if(is.null(spot_an)){
      if(stringr::str_detect(mn, "-spot")){
        spot_an <- stringr::str_remove(prms[which(startsWith(prms, "spot"))], "spot")
      }else{
        util_log("md_predict_crowns", "L'année de l'image spot utilisée ne peut être extraite du nom du fichier rds du modèle: -spot non trouvé")
        return("ko")
      }
    }

    if("rsp" %in% prms){
      rsp_coper <- TRUE
    }
  }

  # Construction pile prédicteurs ----------------------------------------------------

  crowns <- uRast("crowns","last")

  dtc <- data_table_copernicus()


  ls_tav <- purrr::map(1:length(mx), function(i_model){

    m <- mx[[i_model]]

    vars <- m$importance %>% rownames()

    # vars2 <- stringr::str_replace_all(vars, "_x_", "_")
    # vars2 <- stringr::str_replace_all(vars2, "spot_", "spot-")


    tav <- purrr::map_dfr(vars, function(v){

      vv <- strsplit(v, "_")[[1]]

      if(vv[1] %in% dtc$name){
        # Copernicus
        return(data.frame(origine = "copernicus", derive = vv[1], fun = vv[5],
                          date = as.character(vv[2]), param = "", vars = v, stringsAsFactors = FALSE))
      }
      if(vv[1] == "spot"){
        # spot
        return(data.frame(origine = vv[1], derive = vv[3], fun = vv[4],
                          date = as.character(vv[2]), param = "", vars = v, stringsAsFactors = FALSE))
      }
      if(vv[1] == "topo"){
        # topo
        return(data.frame(origine = "mnt",
                          derive = ifelse(vv[2] == "alti", "", vv[2]),
                          fun = vv[3],
                          date = "", param = "", vars = v, stringsAsFactors = FALSE))
      }
      if(vv[1] == "mnh"){
        # mnh
        return(data.frame(origine = "mnh",
                          derive = ifelse(vv[2] == "h", "", vv[2]),
                          fun = vv[3],
                          date = ifelse(vv[2] == "dh", "all", "last"),
                          param = "", vars = v, stringsAsFactors = FALSE))
      }
    })

    # tav <- stringr::str_split(vars2, "_", simplify = TRUE)
    #
    # tav[tav == "topo"] <- "mnt"
    # tav <- cbind(tav, rep("last", nrow(tav)))
    #
    # if(length(which(startsWith(tav[,1],  "spot"))) > 0){
    # tav[which(startsWith(tav[,1],  "spot")), 4] <- tav[which(startsWith(tav[,1],  "spot")), 1]
    # tav[which(startsWith(tav[,1],  "spot")), 1] <- stringr::str_split(
    #   tav[which(startsWith(tav[,1],  "spot")), 1], "-", simplify = TRUE)[,1]
    #
    # tav[which(startsWith(tav[,1],  "spot")), 4] <- stringr::str_split(
    #   tav[which(startsWith(tav[,1],  "spot")), 4], "-", simplify = TRUE)[,2]
    # }

    tav$fun[tav$fun == "med"] <- "median"
    tav$fun[tav$fun == "moy"] <- "mean"

    lines_q <- which(startsWith(tav$fun, "q"))

    if(length(lines_q) > 0){
      tav$param[lines_q] <- stringr::str_remove(tav$fun[lines_q], "q")
      tav$fun[lines_q] <- "q"
    }
    # tav <- cbind(tav, rep("", nrow(tav)))

    # tav[which(startsWith(tav[,3], "q")), 5] <- tav[which(startsWith(tav[,3], "q")), 3] %>% stringr::str_remove("q")
    # tav[which(startsWith(tav[,3], "q")), 3] <- "quantile"

    # tav <- cbind(tav, vars)
    tav
  })

  tav_tot <- do.call(rbind, ls_tav) %>% #as.data.frame(stringsAsFactors = FALSE) %>%
    filter(!duplicated(.))

  # dh

  if("dh" %in% tav_tot$derive){
    tav_tot <- rbind(tav_tot,
                     data.frame(
                       origine = "mnh", derive = "", fun = "q", date = c("last", "first"),
                       param = "90",
                       vars = c("mnh_h_q90", "mnh_h0_q90")
                     )) %>%
      filter(derive != "dh") %>% # mnh_h0 présent déclanchera dh
      filter(!duplicated(.))
  }


  # Construction des rasters prédicteurs -------------------------------------------



    # Copernicus

    # vars_start <- stringr::str_split(tav_tot$origine, "\\.", simplify = TRUE)[,1]
    # w_coper <- which(vars_start %in% unique(dtc$name))

    # if(length(w_coper) > 0){
    if("copernicus" %in% tav_tot$origine){

      # tav_cop <- tav_tot %>% slice(w_coper)
      # df_cop <- dtc %>% filter(id %in% stringr::str_replace(tav_cop$origine, "\\.", " "))

      id_tav <- tav_tot %>% filter(origine == "copernicus") %>%
        mutate(iid =paste(derive, date)) %>% pull(iid)

      df_cop <- dtc %>% filter(id %in% id_tav)

      co <- data_copernicus(resample = rsp_coper, df_cop = df_cop)

      co <- co[[stringr::str_replace_all(id_tav, " ", "_")]]

      nms <- tav_tot %>% filter(origine == "copernicus") %>% pull(vars) %>% sort()
      names(co) <- nms[order(names(co))]
    }

    # autres

    tav <- tav_tot %>% filter(origine != "copernicus")

    # Extraction des rasters ---------

    if(file.exists(path_pile)){

      pile <- rast(path_pile)

    }else{


    ls <- purrr::map(1:nrow(tav), function(i){

      print(i)

      ta <- tav[i,] %>% as.character()
      names(ta) <- names(tav)

      message("construction du raster prédictif ", toupper(paste(ta[1:2], collapse = " ")))

      if(ta["origine"] == "bdforet"){
        return(data_bdforet(force=F))
      }



      assign(ta["origine"],
             uRast(ta["origine"], date = ta["date"])) %>% terra::aggregate(res)


      if(ta["origine"] == "spot"){

        if(ta["fun"] == "quantile"){
          fun <- function(x){quantile(x, ta["param"])}
        }else{
          fun <- get(ta["fun"] %>% as.character())
        }
        print(spot_formula(ta["derive"]))
        return(eval(parse(text = spot_formula(ta["derive"]))))

      }else if(ta["origine"] %in% c("mnt", "mnh")){

        if(ta["derive"] %in% c("")){
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

    names(ls) <- tav$vars

    pile <- do.call(c, ls) %>% terra::rast()

    if(exists("co")) pile <- c(pile, co %>% project(pile))


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

    writeRaster(pile, path_pile)
  }

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

  # visu_add_raster(ess, compare = TRUE)


  cat_ess <- read.csv(file.path(system.file(package = "oiseauData"), "tables", "cat", "essences.csv"),
                      stringsAsFactors = FALSE)

  coltab(ess) <- cats(ess)[[1]] %>% left_join(cat_ess, by = c("label"="type")) %>%
    select(value, couleur_groupe)


  writeRaster(ess, dest, overwrite = TRUE)
  write.csv(cats(ess)[[1]], paste0(tools::file_path_sans_ext(dest), ".csv"), row.names = FALSE)

  # rpred <- terra::rasterize(crv %>% as("SpatVector"), templ, field = "pred",
  #                           filename = destfile,
  #                           overwrite = TRUE
  #                           )

  message("raster écrit sous ", dest)


  unlink(tmp, recursive = TRUE)
  ess
}
