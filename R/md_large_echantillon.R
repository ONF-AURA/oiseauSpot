#' Echantillonnage de rasters à large échelle
#'
#' @param rs pile de raster (y) pour lesquels on cherche un échantillon de prédicteurs
#' @param an an spot utilisée
#' @param nspl nb de grappes à utiliser
#' @param larg_grappe largeur de la garppe carrée
#'
#' @return liste de
#' @export
#'

md_large_echantillon <- function(rs, ans = NULL, nspl = 10, larg_grappe = 1000,
                                 rsp_coper = FALSE,
                                 dest = dc("dos_modeles_oiseau")){

  e <- ext(rs) %>% project(crs(rs), "epsg:2154") %>% as.polygons() %>% st_as_sf()
  st_crs(e) <- 2154

  disp <- util_large_dispo(e, an_spot = ans)

  if(is.null(disp)){
    return("ko")
  }

  disp1km <- disp %>% aggregate(10)
  disp1km[disp1km != 1] <- NA


  s <- terra::spatSample(disp1km, nspl, na.rm = TRUE, xy = TRUE)


  tmp_dir <- tempfile()
  dir.create(tmp_dir)

  for(i in 1:nspl){


    try({
      message(crayon::green("=======================================\n-----------------", i, "--------------------\n====================================\n"))

      z <- util_xy2circle(s$x[i], s$y[i], larg_grappe/2)

      prj <- paste0("tmp_", sample.int(1e15,1))

      data_new_projet(prj, shp = z)

      data_mnt()
      data_mnh()
      data_spot(ans)


      pr <- md_predicteurs(res = 1, sentinel = FALSE, copernicus = FALSE, insol = FALSE, dendro = FALSE, spot_date = ans, rsp_coper = rsp_coper)

      writeRaster(pr, file.path(tmp_dir, paste0("pred", i, ".tif")))
    })

  }


  message(
    "rasters écrits sous ", tmp_dir
    )



  # Sys.umask(0)
  # dos_samples <- file.path(dest, "samples")
  #
  # if(!dir.exists(dos_samples)) dir.create(dos_samples)



  purrr::map(list.files(tmp_dir, full.names = TRUE), rast)
}
