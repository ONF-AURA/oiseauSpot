#' Création image spot6/7 1.5m d'une zone
#'
#' @param dos_spot dossier des images spot6/7
#' @param roi sf de la zone d'intérêt. Si NULL, renvoie seulement la liste des polygones des emprises
#' @param destpath chemin du dossier où écrire les images
#' @param buffer buffer à appliquer à roi
#' @param info TRUE pour ne renvoyer que le taux de couverture par image
#' @param day_opti jour de l'année optimal pour la prise de vue, format MM-JJ
#' @param force TRUE pour écraser une donnée existante
#' @param spec_date année ou date spécifiquement demandée
#'
#' @return liste des polygones es emprises des dalles spots
#' @export
#'

spot_data <- function(dos_spot = oiseauData::data_conf("dos_spot"),
                      roi =  oiseauData::data_conf("shp"),
                      destpath =  oiseauData::data_conf("path_spot_ts"),
                      buffer =  oiseauData::data_conf("buffer"),
                      info = FALSE,
                      day_opti = "06-15",
                      force = FALSE,
                      spec_date = NULL){

  # dos_spot = "/var/partage2/spot6"
  # img = "2021093036472556CP"
  # roi = oiseau2::se_zone_frt("STSULPIC", FALSE) %>% dplyr::filter(CCOD_CACT==8805)

  # vérif day_opti

  if(inherits(try(as.Date(paste0("2020-", day_opti))), "try-error"))
    return(oiseauUtil::util_log(message = "day_opti doit être au format MM-JJ", fonction = "spot_data"))

  # pansharp

  pansharp <- function(pan, multi, destination, fun = mean) {
    '
    Returns a low-frequency component of the high-resolution raster by the
        filter adjusted to the low-resolution raster
    '
    # @param pan - a high-resolution panchromatic raster - Raster object
    # @param multi - low-resolution raster to be pansharpened - Raster object
    # @param filter - a smoothing wondow - matrix
    # @param fun - a function to process filter (part of the focal() function)
    # @return LPF - a low-frequency component of the high-resolution raster - Raster object

    if(! endsWith(destination, ".tif"))
      stop("La destination doit être un chemin de fichier .tif")
    if(! dir.exists(dirname(destination)))
      stop("Le dossier de destination n'existe pas")

    # Adjust filter size
    pan_res <- terra::res(pan) # (x, y) resolution of the panchromatic raster in CRS units (?)
    multi_res <- terra::res(multi) # (x, y) resolution of the lowres raster in CRS units (?)
    x_res_ratio <- round(multi_res[1]/pan_res[1])
    y_res_ratio <- round(multi_res[2]/pan_res[2])
    total <- x_res_ratio + y_res_ratio
    filter <- matrix(1, nc = x_res_ratio, nr = y_res_ratio)

    # Enshure that the matrix has an uneven number of colums and rows (needed by focal())
    if (nrow(filter)%%2 == 0) {
      filter <- rbind(filter, 0)
    }
    if (ncol(filter)%%2 == 0) {
      filter <- cbind(filter, 0)
    }

    LPF <- terra::focal(pan, w = filter, fun = fun) # low-frequency component

    multi <- terra::resample(multi, pan) # resample low-resolution image to match high-res one

    all <- c(multi, pan, LPF)

    bands <- terra::nlyr(multi)
    pan_band <- bands + 1
    lpf_band <- bands + 2

    # Pansharpen layers from low-resolution raster one by one
    pansharp_bands <- list()
    for (band in 1:bands) {
      subset <- all[[c(band, pan_band, lpf_band)]]
      raster <- (subset[[1]] * subset[[2]]) / subset[[3]]
      pansharp_bands[[band]] <- raster
    }

    pansharp <- do.call(c, pansharp_bands)

    terra::writeRaster(pansharp, destination, overwrite = T)
    pansharp
  }

  # Couverture de la zone par les images disponibles ----------------------------------------------------------------

  Sys.umask(0)

  if(!dir.exists(dirname(destpath))) dir.create(dirname(destpath))

  dos_spot_an <- list.dirs(dos_spot, recursive = FALSE)
  dos_spot_an <- dos_spot_an[which(!is.na(as.numeric(basename(dos_spot_an))))]

  tb_img <- read.csv(file.path(dos_spot, "data.csv"), stringsAsFactors = FALSE)

  taux <- list() # taux de couverture de la zone d'intérêt par image
  tx_clouds <- list()
  tx_snow <- list()


  paths_ms <- tb_img %>% dplyr::filter(band == "MS") %>% dplyr::pull(tif)

  # dossier des miniatures
  dir_mini <- file.path(dos_spot, "ext")
  if(!dir.exists(dir_mini)) dir.create(dir_mini)

  # recherche des images concernant la zone d'étude

  area_roi <- sf::st_area(roi) %>% as.numeric() %>% sum(na.rm = TRUE)

  for(i_ms in paths_ms){

    path_mini <- file.path(dir_mini, basename(i_ms))

    if(!file.exists(path_mini)){

      # création des miniatures encore non créées

      message("Création de la miniature SPOT ", basename(i_ms))

      ext <- terra::rast(i_ms[1])[[1]] %>% terra::aggregate(10)
      ext[ext < 0] <- NA
      ext[ext == 0] <- NA
      ext[!is.na(ext)] <- 1

      terra::writeRaster(ext, path_mini, overwrite = TRUE)
    }


    # intersection avec roi

    ext <- terra::rast(path_mini)

    vext <- terra::as.polygons(ext)

    names(vext) <- "sp"

    inter <-
      terra::intersect(roi %>% dplyr::select("id") %>% as("SpatVector"), vext) %>%
      sf::st_as_sf()

    if(nrow(inter) == 0){
      taux[[basename(i_ms)]] <- 0
    }else{
      taux[[basename(i_ms)]] <- inter %>%
        dplyr::filter(sp == 1) %>%
        dplyr::mutate(area = sf::st_area(.) %>% as.numeric()) %>%
        pull(area) %>% sum(na.rm = TRUE) / area_roi
    }

    # masques nuages

    fun_msk <- function(i_ms, roi, inter){

      if(nrow(inter) == 0) return(c(0, 0))


      if(taux[[basename(i_ms)]] > 0) return(c(0, 0))

      msk <- spot.clouds(i_ms)

      if(is.null(msk)) return(c(0, 0))

      i_msk0 <- terra::intersect(roi %>% dplyr::select("id") %>% as("SpatVector"), msk %>% as("SpatVector")) %>%
        sf::st_as_sf()

      if(nrow(i_msk0) == 0) return(c(0, 0))

      i_msk <- i_msk0 %>%
        dplyr::mutate(area = sf::st_area(.) %>% as.numeric()) %>%
        dplyr::group_by(maskType) %>%
        dplyr::summarise(tx = sum(area, na.rm = TRUE) / area_roi)

      txc <- i_msk %>% dplyr::filter(maskType == "CLOUD") %>% dplyr::pull(tx)
      txs <- i_msk %>% dplyr::filter(maskType == "SNOW") %>% dplyr::pull(tx)

      if(length(txc) == 0) txc <- 0
      if(length(txs) == 0) txs <- 0

      return(c(txc, txs))

    }

    tx <- fun_msk(i_ms, roi, inter)

    tx_clouds[[basename(i_ms)]] <- tx[1]
    tx_snow[[basename(i_ms)]] <- tx[2]

  }

  # table des images disponibles

  tb_info <- tb_img %>% dplyr::filter(band == "MS") %>%
    dplyr::mutate(file = basename(tif)) %>%
    dplyr::left_join(
      data.frame(file = names(taux),
                 couverture = unlist(taux) * 100,
                 nuages = unlist(tx_clouds) * 100,
                 neige = unlist(tx_snow) * 100,
                 stringsAsFactors = FALSE),
      by = "file"
    ) %>%
    dplyr::mutate(
      an = as.Date(date) %>% format("%Y") %>% as.numeric(),
      diff_opti = (as.numeric(as.Date(date)) -
                     as.numeric(as.Date(paste0(an, "-", day_opti)))) %>% abs())


  if(info){
    return(tb_info)
  }

  if(tb_info$couverture %>% na.omit() %>% length() == 0){
    oiseauUtil::util_msg("Aucune image spot n'est disponible pour cette zone")
  }

  tb_info <- tb_info %>% dplyr::filter(!is.na(couverture))


  # images déjà enregistrées -----------------------------------

  if(file.exists(destpath)){
    spot_old <- uRast("spot")
    dates_old <- terra::time(spot_old) %>% unique()
  }else{
    dates_old <- NULL
  }

  ans_cherche <- tb_info$an %>% unique() %>% sort()

  if(!force){
    ans_cherche <- ans_cherche[! ans_cherche %in% format(dates_old, "%Y")]
  }

  if(!is.null(spec_date)){

    if(!is.na(stringr::str_match("2029", "^20[1-2][0-9]$") %>% as.character())){

      # date_spec au format année

      if(!spec_date %in% ans_cherche){
        util_log("spot_data", paste0("L'année ", spec_date, " spécifiquement demandée n'est pas disponible."))
        return("ko")
      }

      ans_cherche <- spec_date

    }else{

      if(! as.character(spec_date) %in% tb_info$date){
        util_log("spot_data", paste0("La date spécifiquement demandée n'existe pas: ", spec_date))
        return("ko")
      }

      ans_cherche <- as.Date(spec_date) %>% format("%Y")

    }
  }

  if(length(ans_cherche) == 0){

    message("images SPOT à jour.")
    return(NULL)
  }


  # choix des images par années ---------------------------------------------------------------
  # couverture max, puis jour optimal: préférer
  # le début de saison de végétation (évite les colorations automnales) après débourrement complet

  tmpdir <- tempfile()
  dir.create(tmpdir)

  for(an_ in ans_cherche){

    tb_info_select <- tb_info %>% dplyr::filter(an == an_ & !is.na(couverture) & couverture != 0) %>%
      dplyr::arrange(diff_opti)

    if(nrow(tb_info_select) > 0){

      if(max(tb_info_select$couverture) == 100){
        tb_info_select <- tb_info_select %>% dplyr::filter(couverture == 100) %>% dplyr::slice(1)
      }

      imgs <- tb_info_select$img

      path_ms_an <- tb_img %>% dplyr::filter(img == imgs[1] & band == "MS") %>% dplyr::pull(tif)

      roi2 <- roi %>% sf::st_buffer(buffer) %>%
        sf::st_transform(terra::crs(terra::rast(path_ms_an)))

      ls_img <- purrr::map(imgs, function(i){


        # raster Panchro

        pf <- tb_img %>% dplyr::filter(img == i & band == "PAN") %>% dplyr::pull(tif)
        # pf possiblement tuilé (constitué de plusieurs images, chemins séoparés par " ")
        if(stringr::str_detect(pf, " xxx ")){
          pf <- stringr::str_split(pf, " xxx ", simplify = TRUE)[1,]
          ls_rast <- purrr::map(pf, ~ tryCatch(
            terra::rast(.x) %>% terra::crop(roi2),
            error = function(e){NULL}
          ))
          source <- terra::sprc(ls_rast[which(!purrr::map_lgl(ls_rast, is.null))])
          pa <- terra::merge(source)
        }else{
          pa <- terra::rast(pf)
        }

        # raster multispectral

        msf <- tb_img %>% dplyr::filter(img == i & band == "MS") %>% dplyr::pull(tif)
        msa <- terra::rast(msf) %>% terra::crop(roi2)

        inter <- terra::intersect(
          roi2 %>%  as("SpatVector"),
          terra::ext(pa)
        )
        if(terra::expanse(inter) %>% length > 0){
          list(PAN = pa %>% terra::crop(inter),
               MS = msa %>% terra::crop(inter))
        }else{
          NULL
        }
      })

      ls_img1 <- ls_img[! purrr::map_lgl(ls_img, is.null)]

      ls_pan <- purrr::map(ls_img1, ~.x[["PAN"]])
      ls_ms <- purrr::map(ls_img1, ~.x[["MS"]])

      if(length(ls_img1) > 1){
        pan_tot <- terra::mosaic(terra::sprc(ls_pan))
        ms_tot <- terra::mosaic(terra::sprc(ls_ms))

      }else{
        pan_tot <- ls_pan[[1]]
        ms_tot <- ls_ms[[1]]

      }

      pan <- terra::crop(pan_tot, roi2)
      multi <- terra::crop(ms_tot, roi2)

      result0 <- pansharp(pan, multi, destination = file.path(tmpdir,
                                                              paste0(tb_info_select$date,
                                                                     ".tif")))

      # masque climatiquue

      msk <- spot.clouds(path_ms_an)

      if(is.null(msk)){
        result <- result0
      }else{
        result <- terra::mask(result0,
                              msk)
        message("Masque climatique appliqué sur l'image ", path_ms_an)
      }


      message("image SPOT du ", tb_info_select$date, " créée.")

      gc()
    }
  }

  # écriture série temporelle --------------------------------------------------

  ras <- terra::rast(list.files(tmpdir, full.names = TRUE))

  ls <- purrr::map(list.files(tmpdir, full.names = TRUE), function(x){
    r <- terra::rast(x)
    tm <- stringr::str_remove(basename(x), ".tif") %>% as.Date()
    terra::time(r) <- rep(tm, 4)
    names(r) <- c("red", "green", "blue", "ir")
    r
  })


  cls <- do.call(c, ls)

  tmpdos <- file.path(dirname(destpath), "tmp")

  unlink(tmpdos, recursive = TRUE)

  dir.create(tmpdos)

  terra::writeRaster(cls, file.path(tmpdos, "tmp.tif"))

  data.ras_merge(cls, var = "spot", dest = destpath)


  oiseauUtil::util_msg("Série temporelle SPOT extraite.", notification = TRUE)

}






