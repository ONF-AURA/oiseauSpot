



#' Mise à jour du MNH avec images spots
#'
#' L'utilisateur sélectionne 1 image par an puis les seuils de détection pour chaque année
#'
#' @param an_fin OBLOGATOIRE: dernière année à mettre à jour
#' @param ext sf de la zone
#' @param dos_spot dossier des images spot
#' @param path_spot_ts chemin du fichier .nc d'écriture de la série temporelle
#' @param path_mnh chemin du MNH
#' @param an_mnh année du MNH
#' @param buffer buffer shp
#' @param .dir Dossier du projet si oiseauData est utilisé. Omet tous les autres arguments sauf an_fin
#' @param dest_masques chemin d'écriture du fichier raster .tif des masques
#' @param dest_mnh chemin d'écriture du fichier raster .tif des mnh
#' @param seuil_diff_spot seuil de différence à partir duquel la couronne est éliminée
#'
#' @return liste des masques annuels à appliquer au MNT
#' @export
#'
spot_maj_mnh <- function(ext = oiseauData::data_conf("shp"),
                         dos_spot = oiseauData::data_conf("dos_spot"),
                         path_spot_ts = oiseauData::data_conf("path_spot_ts"),
                         path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
                         dest_masques = oiseauData::data_conf("path_mnh_mask_ts"),
                         dest_mnh = oiseauData::data_conf("path_mnh_ts"),
                         buffer  = oiseauData::data_conf("buffer"),
                         seuil_diff_spot = oiseauData::data_conf("seuil_diff_spot")
){

  # spot_maj_mnh(ext, dos_spot="/var/partage2/spot6", dos_data_spot="/var/user/sdumas/test_spot",
  #              path_mnh="/var/partage2/lidar/ST-SULPICE/mnh1.tif", an_mnh=2018, an_fin=2022)

  # oiseauData::data_check("shp","dos_spot","dos_data_spot","path_mnh1","an1","path_mnh_ts","path_mnh_mask_ts","seuil_diff_spot", "buffer")
  #
  # if(.dir == "unset"){
  #   message("Aucun projet oiseauData ouvert: les arguments de la fonction sont utilisés et doivent être entrés par l'opérateur")
  # }else{
  #   dc <- oiseauData::data_conf
  #   ext <- dc("shp")
  #   dos_spot <- dc("dos_spot")
  #   dos_data_spot <- dc("dos_data_spot")
  #   path_mnh <- dc("path_mnh1")
  #   an_mnh <- dc("an1")
  #   dest_mnh <- dc("path_mnh_ts")
  #   dest_masques <- dc("path_mnh_mask_ts")
  #   seuil_diff_spot <- dc("seuil_diff_spot")
  #   buffer <- dc("buffer")
  #
  # }


  spots <- terra::rast(path_spot_ts)
  mnhs <- terra::rast(path_mnh_ts)

  ans_spot <- format(terra::time(spots), "%Y") %>% as.numeric()
  ans_mnh <- format(terra::time(mnhs), "%Y") %>% as.numeric()

  ans <- max(ans_mnh):max(ans_spot)


  # Correction MNH0: suppression des arbres présent sur le MNH mais sec ou disparus sur Spot

  diff <- spot_differences(an = min(ans), path_mnh_mask_ts = NULL)

  spot_plotDiff(diff)

  # Choix du seuil

  if(is.null(seuil_diff_spot)){

    # saisie interactive du seuil


    valid <- FALSE

    while(!valid){

      seuil_an0 <- as.numeric(readline(paste(ans[1], ": Seuil choisi :")))


      mask_mnh0 <- spot_perturbation(seuil_an0, diff = diff, liste_spots = spots, an = 2021,
                                     overlay = ext, mask = TRUE)

      valid <- askYesNo("Valider ?")
    }

    layout(matrix(c(1:2), nrow = 1))

    pal <- colorRampPalette(c("white", "red"))(20)

  }else{
    # seuil fixé a priori

    mask_mnh0 <- spot_perturbation(seuil_diff_spot, diff = diff, liste_spots = spots, an = 2021,
                                   overlay = ext, mask = TRUE)

  }

  ls <- list(mask_mnh0)
  names(ls) <- paste0("an_", ans[1])

  # Années suivantes

  for(a in ans[-1]){

    diff <- spot_differences(paths = path_spots, ext = ext, an = a, buffer = buffer, path_mnh = path_mnh, an_mnh = an_mnh,
                             formula = "(spot$ir - spot$r) / (spot$ir + spot$r)",
                             mask_mnh = ls[[paste0("an_", a)]])

    if(is.null(seuil_diff_spot)){

      # saisie interactive du seuil

      spot_plotDiff(diff)

      valid <- FALSE

      while(!valid){

        seuil <- as.numeric(readline(paste(a, ": Seuil choisi :")))

        mask_mnh <- spot_perturbation(seuil, diff = diff,liste_spots = spots, an = a, overlay = ext, mask = TRUE)

        valid <- askYesNo("Valider ?")
      }
    }else{

      # seuil fixé a priori

      mask_mnh <- spot_perturbation(seuil_diff_spot, diff = diff,liste_spots = spots, an = a, overlay = ext, mask = TRUE)

    }

    ls[[paste0("an_", a)]] <- mask_mnh
  }

  layout(matrix(c(1:(length(ans) + 1)), nrow = 1))
  pal <- colorRampPalette(c("white", "red"))(20)

  msk_ext <- ext %>% as("SpatVector")

  crs0 <- terra::crs(msk_ext)

  mnh0 <- terra::rast(path_mnh) %>% terra::crop(ext) %>% terra::mask(msk_ext)
  terra::plot(mnh0, axes = FALSE, main = "avec morts", col = pal, legend = FALSE)

  terra::crs(mnh0) <- terra::crs(mask_mnh0) <- crs0

  new <- list(mnh0)
  names(new) <- paste0("mnh", ans[1])

  mnh0c <- mnh0 * mask_mnh0 %>% terra::crop(ext) %>% terra::mask(msk_ext)
  terra::plot(mnh0c, axes = FALSE, legend = FALSE, main = "sans morts", col = pal)

  new[[paste0("mnhcor", ans[1])]] <- mnh0c

  for(m in ls) terra::crs(m) <- crs0

  terra::crs(msk_ext) <- crs0

  for(a in ans[-1]){
    msk_an <- ls[[a]]
    terra::crs(msk_an) <- crs0

    mnh <- mnh0c * msk_an %>% terra::crop(ext) %>% terra::mask(msk_ext)
    terra::plot(mnh, axes = FALSE, legend = FALSE, main = a, col = pal)
    new[[paste0("mnh", a)]] <- mnh

  }

  mnh = terra::rast(new)
  names(mnh) <- stringr::str_remove(names(mnh), "mnh")

  ini <- terra::rast(path_mnh)
  add <- purrr::map_lgl(names(ini), ~ .x %in% names(mnh))

  if(!(all(add))){
    ajout <- ini %>% terra::subset(which(add == FALSE))
  }

  mnh2 <- c(ajout, mnh %>% terra::resample(ini))


  r <- list(masques = terra::rast(ls), mnh = mnh2)

  # bug pour ecrire sur meme fichier:
  tmp <- tempfile(fileext = ".tif")
  terra::writeRaster(mnh2, tmp, overwrite = TRUE)

  rm(mnh2)
  terra::writeRaster(terra::rast(tmp), dest_mnh, overwrite = TRUE)
  unlink(tmp)

  terra::writeRaster(terra::rast(ls), dest_masques, overwrite = TRUE)

}

