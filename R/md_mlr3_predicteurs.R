
#' Construction de la pile des prédicteurs utilisés par un modèle MLR3
#'
#' @param fea variables sélectionnée (cf md_mlr3_select)
#' @param res résolution des rasters à compiler
#' @param spot_an année de l'image spot utilisée
#' @param rsp_coper T/F resampler les rasters Copernicus à la résolution res
#' @param dest chemin d'écriture de la pile
#'
#' @return spatraster
#' @export
#'
md_mlr3_predicteurs <- function(fea,
                               res = NULL, spot_an = NULL,
                               rsp_coper = FALSE,
                               dest = file.path(dc("dos_projet"), "pile_essences.tif")
){


  # "/var/user/sdumas/oiseauX/rf_ESS-res1-spot2018.rds"


  # Construction pile prédicteurs ----------------------------------------------------

  crowns <- uRast("crowns","last")

  dtc <- data_table_copernicus()




  tav <- purrr::map_dfr(fea, function(v){

    vv <- strsplit(v, "_")[[1]]

    if(vv[1] %in% dtc$name){
      # Copernicus
      if(vv[2] == "x") vv <- vv[-2]
      return(data.frame(origine = "copernicus", derive = vv[1], fun = vv[5],
                        date = as.character(vv[2]), param = "", vars = v, stringsAsFactors = FALSE))
    }
    if(vv[1] == "spot"){
      # spot
      if(startsWith(vv[2], "20")){
        return(data.frame(origine = vv[1], derive = vv[3], fun = vv[4],
                          date = vv[2], param = "", vars = v, stringsAsFactors = FALSE))
      }else{
        return(data.frame(origine = vv[1], derive = vv[2], fun = vv[3],
                          date = as.character(spot_an), param = "", vars = v, stringsAsFactors = FALSE))
      }

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


  tav$fun[tav$fun == "med"] <- "median"
  tav$fun[tav$fun == "moy"] <- "mean"

  lines_q <- which(startsWith(tav$fun, "q"))

  if(length(lines_q) > 0){
    tav$param[lines_q] <- stringr::str_remove(tav$fun[lines_q], "q")
    tav$fun[lines_q] <- "q"
  }

  tav_tot <- tav
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

  if("copernicus" %in% tav_tot$origine){


    id_tav <- tav_tot %>% filter(origine == "copernicus") %>%
      mutate(date = str_extract(date, "[0-9]{4}")) %>%
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
        if(ta["origine"] == "mnt"){
          return(data_topo()[[paste0("topo_", ta["derive"])]] %>% project(data_ref(1)))

        }else{
          return(terra::terrain(get(ta["origine"]), ta["derive"]))
        }
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

  writeRaster(pile, dest, overwrite = TRUE)

  message("Pile des préditeurs de l'essence écrit sous ", dest)

  pile
}


