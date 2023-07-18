



#' Mise à jour du MNH avec images spots
#'
#' L'utilisateur sélectionne 1 image par an puis les seuils de détection pour chaque année
#'
#' @param an_fin OBLOGATOIRE: dernière année à mettre à jour
#' @param ext sf de la zone
#' @param path_spot_ts chemin du fichier .nc d'écriture de la série temporelle
#' @param path_mnh_ts chemin du MNH
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
                         path_spot_ts = oiseauData::data_conf("path_spot_ts"),
                         path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
                         dest_masques = oiseauData::data_conf("path_mnh_dead_ts"),
                         dest_mnh = oiseauData::data_conf("path_mnh_ts"),
                         buffer  = oiseauData::data_conf("buffer"),
                         seuil_diff_spot = oiseauData::data_conf("seuil_diff_spot"),
                         replace = FALSE
){


  mnhs <- terra::rast(path_mnh_ts)

  date_mnh <- max(terra::time(mnhs) %>% as.Date())

  spots <- terra::rast(path_spot_ts)
  dates_spot <- terra::time(spots) %>% as.Date()

  # date précédant le MNH, pour suppression des arbres détectés mais morts

  date0 <- max(dates_spot[which(format(dates_spot, "%Y") < format(date_mnh, "%Y"))])

  # dates suivantes

  dates <- c(date0, dates_spot[which(dates_spot > date0)] %>% unique() %>% sort())



  maj <- function(date0, date1, date_mnh){

    mnhs <- terra::rast(path_mnh_ts)


    maj0 <- spot_disparitions(date0 = date0, date1 = date1, date_mnh = date_mnh)


    mnh0 <- mnhs[[as.Date(terra::time(mnhs)) == as.Date(date_mnh)]]

    mnh0c <- mnh0 * maj0
    terra::time(mnh0c) <- date1

      mnh_cor_new <- c(mnhs, mnh0c)
      terra::time(mnh_cor_new) <- c(terra::time(mnhs) %>% as.Date(),
                                    terra::time(mnh0c) %>% as.Date())

      tmp <- tempfile(fileext = ".nc")
      terra::writeCDF(mnh_cor_new, tmp, overwrite = TRUE)
      new <- terra::rast(tmp)
      terra::writeCDF(new, dest_mnh, overwrite = TRUE)
      unlink(tmp)

      if(file.exists(dest_masques)){
        prev <- terra::rast(dest_masques)
        dis <- c(prev, maj0)
        terra::time(dis) <- c(terra::time(prev) %>% as.Date,
                              terra::time(maj0) %>% as.Date)
      }else{
        dis <- maj0
      }

      terra::writeCDF(dis, tmp, overwrite = TRUE)
      new <- terra::rast(tmp)
      terra::writeCDF(new, dest_masques, overwrite = TRUE)
      unlink(tmp)

    return(maj0)
  }


  # Correction MNH0: suppression des arbres présent sur le MNH mais sec ou disparus sur Spot
  # ensuite, utilise le MNH corrigé

  ls_dis <- purrr::map(1:length(dates[-1]),
                       ~ ifelse(.x == 1,
                                maj(dates[.x], dates[.x+1], date_mnh),
                                maj(dates[.x], dates[.x+1], dates[.x])))


terra::rast(dest_masques) %>% terra::plot(col=c("red", "gray"))


  message(crayon::green("MNH mis à jour"))
}

