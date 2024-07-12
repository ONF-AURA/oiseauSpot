#' Rasters des métriques des couronnes
#'
#' @param date_mnh date du MNH à utiliser
#' @param crowns raster des couronnes issu de data_crowns
#' @param mnh
#'
#' @return spatRaster
#' @export
#'
md_crowns_metrics <- function(date_mnh = util_get_date("last", "mnh"),
                              path_mnh_ts = data_conf("path_mnh_ts"),
                              path_crowns_ts = data_conf("path_crowns_ts"),
                              path_meta = data_conf("tab_crowns"),
                              path_mnt = data_conf("path_mnt"),
                              buffer = data_conf("buffer"),
                              mask = data_conf("shp"),
                              lim_h_rege = data_conf("lim_h_rege"),
                              best_day = data_conf("spot_best_day"),
                              dest_dir = dc("dos_modeles"),
                              spot_date = "last",
                              topo = TRUE, dendro = TRUE, spot = TRUE,
                              insol = TRUE, mnh = TRUE,
                              sentinel = TRUE,
                              copernicus = TRUE,
                              rsp_coper = FALSE,
                              force = FALSE
){

  dest <- file.path(dest_dir, paste0("crowns_metrics_", date_mnh, ".rds"))


  if(file.exists(dest) & !force){
    return(readRDS(dest))
  }

  if(!file.exists(path_mnh_ts)){
    util_log("md_crowns_metrics", paste("Le MNH n'existe pas."))
  }

  # raster des couronnes -------------------

  crowns <- uRast("crowns", path = path_crowns_ts, path_meta = path_meta)

  # names(crowns) <- uMet <- a(path_meta)$origine

  if(! as.character(date_mnh) %in% as.character(terra::time(crowns))){
    util_log("md_crowns_metrics", paste0("La date ", date_mnh, "n'est pas disponible dans la série temporelle des couronnes"))
    return("ko")
  }
  crowns <- crowns[[terra::time(crowns) %>% as.character() == date_mnh]]


  # # données dérivés du MNH ------------------------
  #
  # mnh <- uRast("mnh", date = date_mnh)
  #
  # names(mnh) <- "h"
  #
  # pte <- terra::terrain(mnh, "slope") %>% terra::resample(mnh)
  # names(pte) <- "pte"
  #
  # pte2 <- terra::terrain(pte, "slope") %>% terra::resample(mnh)
  # names(pte2) <- "pte2"
  #
  # tpi <- terra::terrain(mnh, "TPI") %>% terra::resample(mnh)
  # names(tpi) <- "tpi"

  #
  # # spot
  #
  # date_spot <- uDates() %>% dplyr::mutate(diff = abs(date %>% as.Date() - as.Date(util_get_date("last", "mnh")))) %>%
  #   filter(var == "spot") %>%
  #   filter(diff == min(diff)) %>%
  #   dplyr::pull(date) %>% unique()
  #
  # sp <- uRast("spot", date = date_spot)
  #
  # sp$ndvi <- (sp$ir - sp$red) / (sp$ir + sp$red)
  # # names(sp) <- paste(names(sp), terra::time(sp), sep = "_")
  # sp <- sp %>% terra::resample(mnh)

  # synthèse par couronne ----------------------

  # pile <- c(mnh, pte, pte2, tpi, crowns, sp) %>%
  #   terra::mask(mask %>% sf::st_buffer(buffer) %>% as("SpatVector"))


  # rasters de prediction

  message("Constitution des rasters de prédiction")

  pile0 <- suppressMessages(data_predicteurs_crw(spot_date = spot_date,
                                                 topo = topo, dendro = dendro, spot = spot, insol = insol, mnh = mnh,
                                                 copernicus = copernicus, sentinel = sentinel, rsp_coper = rsp_coper)
  )

  pile <- c(crowns$id, pile0)

  # TODO archivage pile

  # writeRaster(pile, file.path(dc("dos_modeles"), "predicteurs_crw.tif"), overwrite = TRUE)
  #
  # write.csv(names(pile), file.path(dc("dos_modeles"), "predicteurs_crw.csv"))

  message("Calcul des prédicteurs par couronne...")

  # TODO alléger les rasters

  # mm <- minmax(pile)
  # ni <- names(which((mm[2,]-mm[1,]) > 10))
  #
  # pile[[ni]] <- round(pile[[ni]])
  #
  # pile$id[is.nan(pile$id)] <- NA

  # traitement par groupe d'id crowns: cf data_crowns

  pat <- floor(crowns$id / 1e7)

  val_pat <- unique(values(pat)) %>% na.omit()

  ls <- list()

  for(ip in val_pat){

    message("métriques des couronnes: ", ip, " / ", max(val_pat))

    msk <- pat
    msk[msk != ip] <- NA
    pilei <- pile %>% mask(msk)
    pilei <- trim(pilei)

    n <- names(pilei)[-1]

    data0 <- util_extract(pilei, "id")

    # data0 <- terra::as.data.frame(pilei, xy = TRUE) %>%
    #   dplyr::filter(!is.nan(id))
    # # dplyr::filter(mnh_h >= lim_h_rege)
    #
    #
    #
    # data1 <- data0 %>%
    #   dplyr::group_by(id) %>%
    #   dplyr::summarise(area = dplyr::n())
    #
    #
    # data2 <- data0 %>%
    #
    #   dplyr::group_by(id) %>%
    #   dplyr::summarise(dplyr::across(-c(x,y), list(
    #     moy = function(x){mean(x, na.rm = TRUE)},
    #     med = function(x){median(x, na.rm = TRUE)},
    #     q10 = function(x){quantile(x, .1, na.rm = TRUE) %>% as.numeric()},
    #     q90 = function(x){quantile(x, .9, na.rm = TRUE) %>% as.numeric()},
    #     sd = function(x){sd(x, na.rm = TRUE)}
    #   ))) %>%
    #   dplyr::mutate(mnh_dh = mnh_h_q90 - mnh_h0_q90) %>%
    #   dplyr::select(-starts_with("mnh_h0"))


    # CALCULER ET FILTRER PAR AREA CROWNS ??

    # area <- values(pile$id) %>% table() %>% as.data.frame() %>%
    #   filter(Freq > 1)
    #
    # data <- data2 %>%
    #   dplyr::left_join(data1, by="id") %>%
    #   dplyr::filter(area > 1) # & mnh_h_q90 >= lim_h_rege)

    ls[[ip]] <- data0
  }

  # data_rcr <- terra::as.data.frame(crowns$id, na.rm = FALSE) %>%
  #   dplyr::left_join(data, by = "id")
  #
  # new_fact <- names(data_rcr %>% dplyr::select(-id))
  #
  # ls_r <- purrr::map(new_fact,
  #                    function(x){
  #                      r <- crowns[[1]]
  #                      terra::values(r) <- data_rcr[[x]]
  #                      names(r) <- x
  #                      r
  #                    })
  #
  # names(ls_r) <- new_fact
  #
  # rcr2 <- do.call(c, ls_r)
  #
  # new <- terra::rast(rcr2)
  #
  #
  #
  # data.ras_merge(new,
  #                            var = "crowns",
  #                            dest = path_crowns_ts,
  #                            path_meta = path_meta,
  #                            path_mnt = path_mnt)

  data <- do.call(rbind, ls)

  dest <- file.path(dest_dir, paste0("crowns_metrics_", date_mnh, ".rds"))

  saveRDS(data, dest)

  util_msg(paste("Métriques des couronnes suvegardées sous", dest), notification = TRUE)


  data
}
