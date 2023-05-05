



#' Mise à jour du MNH avec images spots
#'
#' L'utilisateur sélectionne 1 image par an puis les seuils de détection pour chaque année
#'
#' @param ext sf de la zone
#' @param dos_spot dossier des images spot
#' @param path_data dossier où écrire les résultats
#' @param path_mnh chemin du MNH
#' @param an_mnh année du MNH
#' @param an_fin dernière année à mettre à jour
#'
#' @return liste des masques annuels à appliquer au MNT
#' @export
#'
spot_maj_mnh <- function(ext, dos_spot, path_data, path_mnh, an_mnh, an_fin){

  # spot_maj_mnh(ext, dos_spot="/var/partage2/spot6", path_data="/var/user/sdumas/test_spot",
  #              path_mnh="/var/partage2/lidar/ST-SULPICE/mnh1.tif", an_mnh=2018, an_fin=2022)



  ans <- an_mnh:an_fin

  spot_data(dos_spot, ext, path_data, buffer = 100)
  # chemins vers les images SPOT 4 bandes à résolution 1.5m
  path_spots <- list.files(path_data, pattern = "spot", full.names = TRUE)
  names(path_spots) <- stringr::str_remove_all(basename(path_spots), "[a-z]|\\.")


  # Tampon à appliquer autour de l'UG
  buffer <- 10

  spots <- spot_get(paths = path_spots, ext = ext, buffer = buffer, template = terra::rast(path_mnh))

  # choix des images

  ls_rem <- purrr::map(c(an_mnh - 1, ans), function(a){

    spot_a <- spots[startsWith(names(spots), paste0("spot", a))]

    if(length(spot_a) == 0){
      if(a %in% c(an_mnh, an_mnh - 1)){
        stop(paste("Donnée manquante: image spot", a))
      }else{
        NULL
      }
    }

    spot_plotRGB(spot_a, mask = TRUE, overlay = ext)

    num <- as.numeric(readline(paste("Quelle image pour", a, "? 0 pour aucune ou 1 à",
                                     length(spot_a), ":")))

    while(is.na(num) | !num %in% 0:length(spot_a)){
      num <- as.numeric(readline(paste("Entrée invalide. Quelle image pour", a,
                                       "? 0 pour aucune ou 1 à", length(spot_a))))
    }

    rem <- names(spot_a)
    if(num > 0) rem <- rem[-num]

    rem
  })

  spots <- spots[!names(spots) %in% unlist(ls_rem)]

  names(spots) <- stringr::str_sub(names(spots), 1, 8)


  # Correction MNH0: suppression des arbres présent sur le MNH mais sec ou disparus sur Spot

  diff <- spot_differences(paths = path_spots, ext = ext, an = ans[1],
                           buffer = buffer, path_mnh = path_mnh, an_mnh = an_mnh,
                           formula = "(spot$ir - spot$r) / (spot$ir + spot$r)")

  spot_plotDiff(diff)

  # Choix du seuil

  valid <- FALSE

  while(!valid){

    seuil_an0 <- as.numeric(readline(paste(ans[1], ": Seuil choisi :")))


    mask_mnh0 <- spot_perturbation(seuil_an0, diff = diff, liste_spots = spots, an = 2021,
                                   overlay = ext, mask = TRUE)

    valid <- askYesNo("Valider ?")
  }

  layout(matrix(c(1:2), nrow = 1))

  pal <- colorRampPalette(c("white", "red"))(20)

  ls <- list(mask_mnh0)
  names(ls) <- paste0("an_", ans[1])

  # Années suivantes

  for(a in ans[-1]){

    diff <- spot_differences(paths = path_spots, ext = ext, an = a, buffer = buffer, path_mnh = path_mnh, an_mnh = an_mnh,
                             formula = "(spot$ir - spot$r) / (spot$ir + spot$r)",
                             mask_mnh = ls[[paste0("an_", a)]])

    spot_plotDiff(diff)

    valid <- FALSE

    while(!valid){

      seuil <- as.numeric(readline(paste(a, ": Seuil choisi :")))

      mask_mnh <- spot_perturbation(seuil, diff = diff,liste_spots = spots, an = a, overlay = ext, mask = TRUE)

      valid <- askYesNo("Valider ?")
    }

    ls[[paste0("an_", a)]] <- mask_mnh
  }

  layout(matrix(c(1:(length(ans) + 1)), nrow = 1))

  msk_ext <- ext %>% as("SpatVector")

  crs0 <- terra::crs(msk_ext)

  new <- list(mnh0)
  names(new) <- paste0("mnh", ans[1])

  mnh0 <- terra::rast(path_mnh) %>% terra::crop(ext) %>% terra::mask(msk_ext)
  terra::plot(mnh0, axes = FALSE, main = "avec morts", col = pal, legend = FALSE)

  terra::crs(mnh0) <- terra::crs(mask_mnh0) <- crs0

  mnh0c <- mnh0 * mask_mnh0 %>% terra::crop(ext) %>% terra::mask(msk_ext)
  terra::plot(mnh0c, axes = FALSE, legend = FALSE, main = "sans morts", col = pal)

  new[[paste0("mnhcor", ans[1])]] <- mnh0c

  for(m in ls) terra::crs(m) <- crs0

  for(a in ans[-1]){
    mnh <- mnh0 * ls[[paste0("an_", a)]] %>% terra::crop(ext) %>% terra::mask(msk_ext)
    terra::plot(mnh, axes = FALSE, legend = FALSE, main = a, col = pal)
    new[[paste0("mnh", a)]] <- mnh

  }

  list(masques = do.call(c, ls), mnh = do.call(c, new))

}

