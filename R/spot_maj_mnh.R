



#' Mise à jour du MNH avec images spots
#'
#' L'utilisateur sélectionne 1 image par an puis les seuils de détection pour chaque année
#'
#' @param an_fin OBLOGATOIRE: dernière année à mettre à jour
#' @param ext sf de la zone
#' @param path_spot_ts chemin du fichier .nc d'écriture de la série temporelle
#' @param path_mnh_ts chemin du MNH
#' @param path_crowns_ts chemin vers le raster de série temporelle des couronnes (résolution 1m)
#' @param tab_crowns chemin du fichier meta de la série temporelle des couronnes
#' @param buffer buffer shp
#' @param .dir Dossier du projet si oiseauData est utilisé. Omet tous les autres arguments sauf an_fin
#' @param dest_deads chemin d'écriture du fichier raster .tif des masques
#' @param dest_mnh chemin d'écriture du fichier raster .tif des mnh
#' @param seuil_diff_spot seuil de différence à partir duquel la couronne est éliminée
#'
#' @return liste des masques annuels à appliquer au MNT
#' @export
#'
spot_maj_mnh <- function(ext = oiseauData::data_conf("shp"),
                         path_spot_ts = oiseauData::data_conf("path_spot_ts"),
                         path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
                         tab_mnh = oiseauData::data_conf("tab_mnh"),
                         path_crowns_ts = oiseauData::data_conf("path_crowns_ts"),
                         tab_crowns = oiseauData::data_conf("tab_crowns"),
                         tab_deads = oiseauData::data_conf("tab_deads"),
                         dest_deads = oiseauData::data_conf("path_deads_ts"),
                         dest_mnh = oiseauData::data_conf("path_mnh_ts"),
                         buffer  = oiseauData::data_conf("buffer"),
                         seuil_diff_spot = oiseauData::data_conf("seuil_diff_spot"),
                         replace = FALSE
){



  date_mnh <- uDates("mnh") %>%
    dplyr::filter(origine != "spot") %>%
    dplyr::pull(date) %>%
    as.Date() %>% max() %>% unique()

  dates_spot <- uDates("spot") %>% dplyr::pull(date) %>% as.Date()

  # date précédant le MNH, pour suppression des arbres détectés mais morts

  date0 <- max(dates_spot[which(format(dates_spot, "%Y") < format(date_mnh, "%Y"))])

  # dates suivantes

  dates <- c(date0, dates_spot[which(dates_spot > date0)] %>% unique() %>% sort())



  maj <- function(dates, n, ini = FALSE){

    date0 <- dates[n]
    date1 <- dates[n+1]

    message("mise à jour ", as.character(date1), " ....")


    ras_dif <- spot_differences(date1)

    plot(ras_dif, main=paste("diff", date1, date0))

    ras_dif[ras_dif < seuil_diff_spot] <- 0
    ras_dif[ras_dif >= seuil_diff_spot] <- 1

    plot(ras_dif, main=paste("deads", date1, date0))

    if(n == 1){
      mnh0 <- uRast("mnh", date_mnh, "-spot")
    }else{
      mnh0 <- uRast("mnh", date0, "spot")
    }

    mnh0c <- mnh0 * (1 - ras_dif)

    oiseauData::data.mnh_merge(mnh0c, dest_mnh,
                               date_new = as.character(date1),
                               origine_new = "spot",
                               path_meta = tab_mnh)



    oiseauData::data.ras_merge(ras_dif, "deads",
                               dest = dest_deads,
                               date_new = as.character(date1),
                               path_meta = tab_deads
                               )

    return(mnh0c)
  }


  # Correction MNH0: suppression des arbres présent sur le MNH mais sec ou disparus sur Spot
  # ensuite, utilise le MNH corrigé

  ls_dis <- purrr::map(1:(length(dates) - 1),
                       ~ maj(dates, .x, date_mnh))


  terra::rast(dest_deads) %>% terra::plot(col=c("red", "gray"))


  message(crayon::green("MNH mis à jour"))
}

