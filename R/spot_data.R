#' Création image spot6/7 1.5m d'une zone
#'
#' @param dos_spot dossier des images spot6/7
#' @param roi sf de la zone d'intérêt. Si NULL, renvoie seulement la liste des polygones des emprises
#' @param destpath chemin du dossier où écrire les images
#' @param buffer buffer à appliquer à roi
#'
#' @return liste des polygones es emprises des dalles spots
#' @export
#'

spot_data <- function(dos_spot, roi, destpath, buffer = 100){

  # dos_spot = "/var/partage2/spot6"
  # img = "2021093036472556CP"
  # roi = oiseau2::se_zone_frt("STSULPIC", FALSE) %>% dplyr::filter(CCOD_CACT==8805)

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

  Sys.umask(0)

  if(!dir.exists(destpath)) dir.create(destpath)

  tifs_img <- list.files(dos_spot, pattern = ".TIF", recursive = TRUE, full.names = TRUE)

  # années

  n <- basename(tifs_img)

  n7 <- n[stringr::str_detect(n, "SPOT7")]
  date7 <- stringr::str_split(n7, "_", simplify = TRUE)[,7] %>%
    unique
  n6 <- n[!stringr::str_detect(n, "SPOT7")]
  date6 <- stringr::str_split(n6, "_", simplify = TRUE)[,3] %>%
    stringr::str_sub(1,8) %>%
    unique

  ls_ext <- list() # liste' des polygones des étendues des images

  for(d in c(date6, date7)){

    dp <- tifs_img[stringr::str_detect(basename(tifs_img), d)]

    pan_file <- dp[stringr::str_detect(dp, "_S6P_|PAN_|S7P")]
    multi_file <- dp[stringr::str_detect(dp, "_S6X_|MS_|S7X")]

    ls_ext[[length(ls_ext) + 1]] <- terra::as.polygons(terra::rast(multi_file), extent = TRUE)
    names(ls_ext)[length(ls_ext)] <- paste0(d,"xxx", basename(dp))


    if(!is.null(roi)){
      terra::terraOptions(todisk = TRUE)

      roi2 <- roi %>% sf::st_buffer(buffer) %>%
        sf::st_transform(terra::crs(terra::rast(pan_file[1])))


      if(length(pan_file) > 1){

        basename(pan_file)
        ls_pan <- purrr::map(pan_file, function(pf){

          pa <- terra::rast(pf)
          inter <- terra::intersect(
            roi2 %>%  as("SpatVector"),
            terra::ext(pa)
          )
          if(terra::expanse(inter) %>% length > 0){
            pa %>% terra::crop(inter)
          }else{
            NULL
          }
        })

        ls_pan1 <- ls_pan[! purrr::map_lgl(ls_pan, is.null)]

        if(length(ls_pan1) > 1){
          pan_tot <- terra::mosaic(terra::src(ls_pan1))
        }else{
          pan_tot <- ls_pan1[[1]]
        }

      }else{
        pan_tot <- terra::rast(pan_file)
      }

      multi_tot <- purrr::map(multi_file, ~ terra::rast(.x) %>% terra::crop(roi2))
      multi <- do.call(c, multi_tot)

      pan <- terra::crop(pan_tot, roi2)

      result <- pansharp(pan, multi, destination = file.path(destpath, paste0("spot", d, ".tif")))

      terra::terraOptions(todisk = FALSE)

      message("image", d, "créée.")
    }
  }

  ls_ext <- purrr::map(ls_ext, function(n){
    n <- sf::st_as_sf(n)
    sf::st_crs(n) <- 2154
    n
  })

  do.call("rbind", ls_ext)

}



