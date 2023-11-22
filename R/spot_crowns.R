#' Détection des couronnes sur MNH
#'
#' @param date_mnh  du MNH utilisé
#' @param ext polygone sf de l'étendue à détecter
#' @param path_mnh_ts chemin du MNH
#' @param buffer tampon à appliquer à exr
#' @param lim_h_rege hauteur minimale des apex à détecter
#' @param pente voir lidR::locate_trees
#' @param intercept voir lidR::locate_trees
#' @param smooth fenêtre de lissage du mnh
#' @param uniqueness incremental ou bitmerge
#' @param return_apex si TRUE, renvoie une liste: crowns et apex
#' @param dest_crowns_ts chemin du fichier d'écriture
#' @param path_meta chemin du fichier des métadonnées
#' @param path_mnt chemin du MNT
#'
#' @return spatvector des couronnes ou liste si return_apex
#' @export
#'
spot_crowns <- function(
    date_mnh = "last",
    ext = oiseauData::data_conf("shp"),
    path_mnh_ts = oiseauData::data_conf("path_mnh_ts"),
    dest_crowns_ts = oiseauData::data_conf("path_crowns_ts"),
    path_meta = oiseauData::data_conf("tab_crowns"),
    path_mnt = oiseauData::data_conf("path_mnt"),
    buffer = oiseauData::data_conf("buffer"),
    lim_h_rege = oiseauData::data_conf("lim_h_rege"),
    pente = .07,
    intercept = 2,
    smooth = 5,
    uniqueness  = "incremental",
    return_apex = FALSE
    ){


  mnh0 <- oiseauUtil::uRast("mnh", date = date_mnh, origine = "-spot")

  if(is.null(mnh0)){
    util_log("spot_crown", "Aucun MNH n'est disponible.")
    return("ko")
  }

  message("Cartographie des couronnes en cours...")

  h0 <- mnh0 %>% terra::crop(as(ext %>% sf::st_buffer(buffer), "SpatVector")) %>%
    terra::mask(as(ext %>% sf::st_buffer(buffer), "SpatVector"))

  # if(!is.null(path_deads_ts)){
  #   if(file.exists(path_deads_ts)){
  #     h0_msk <- terra::rast(path_deads_ts)
  #     dates_msk <- terra::time(h0_msk)
  #     dates_msk_util <- dates_msk[which(dates_msk < )]
  #   }
  #
  #   if(an %in% names(h0_msk)){
  #     terra::subset(as.character(an)) %>%
  #       terra::crop(ext %>% st_buffer(buffer)) %>%
  #       terra::mask(as(ext %>% st_buffer(buffer), "SpatVector"))
  #
  #     h0 <- h0 * h0_msk
  #   }else{
  #     message("Couche de masque indisponible.")
  #   }
  # }

  a <- oiseauSpot::spot_apex(h0, lim_h_rege, pente, intercept, smooth = smooth, uniqueness = uniqueness)

  # utilise stars car pb raster not in memory et pb terra poj4...

  # h0r <- oiseauUtil::util_spat2rast(h0) %>%
  #   stars::st_as_stars()

  a_sf <- sf::st_as_sf(a)

  sf::st_crs(a_sf) <- crs(h0)

  h00 <- terra::wrap(h0)
  h000 <- terra::unwrap(h00)

  algo <- lidR::dalponte2016(h000, a_sf)
  cr_ini <- algo()
  # cr <- as(cr_ini, "Raster") %>% terra::rast()

  cr <- cr_ini
  names(cr) <- "id"

message("filtre des courones")

  crowns <- terra::as.polygons(cr) %>% sf::st_as_sf() %>%
    dplyr::mutate(area = sf::st_area(.) %>% as.numeric()) %>%
    dplyr::filter(area > 1) %>%
    dplyr::select(-area) %>%
    as("SpatVector")

  message("écriture raster")

  mnt <- terra::rast(path_mnt)
  rcr <- terra::rasterize(crowns, mnt, field = "id")

  terra::time(rcr) <- terra::time(mnh0)

  oiseauData::data.ras_merge(rcr,
                 var = "crowns",
                 dest = dest_crowns_ts,
                 path_meta = path_meta,
                 path_mnt = path_mnt)


  if(return_apex){
    list(crowns = crowns,
         apex = a_sf)
  }else{
    crowns
  }
}
