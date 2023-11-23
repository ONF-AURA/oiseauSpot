#' Import des images depuis le serveur THEIA
#'
#' Voir la vignette GEOSUD
#'
#' @param path_tar chemin contenant les fichiers .tar.gz téléchargés
#' @param dir dossier dos_spot défini par oiseauData
#' @param unlink_tar TRUE pour supprimer les archives tar.gz
#'
#' @return décompresse selon l'architecture oiseau les fichiers présents
#' @export
#'
#'
spot_untar <- function(path_tar = NULL,
                       dir = data_conf("dos_spot"),
                       unlink_tar = FALSE){

  Sys.umask(0)

  tars <- list.files(path_tar, pattern = ".tar.gz", full.names = TRUE, recursive = FALSE)

  if(length(tars) == 0){
    message("..... pas de fichiers à décompresser")
    return("aucun fichier .tar.gz")
  }

  message(length(tars), " fichiers à intégrer au dossier SPOT: ")

  for(tar in tars){

    message("------ Décompressions de ", basename(tar))


    tmp <- tempfile(tmpdir = file.path(dir))
    dir.create(tmp)

    utils::untar(tar, exdir = tmp)

    #  date de l'image

    xml_file <- list.files(tmp, recursive = TRUE, pattern = ".xml", full.names = TRUE)
    xml <- xml2::as_list(xml2::read_xml(xml_file))
    lsxml <- xml %>% unlist

    # title <- lsxml[which(stringr::str_detect(names(lsxml), "identificationInfo.GSD_Identification.citation.CI_Citation.title"))]

    an <- stringr::str_sub(
      lsxml[which(stringr::str_detect(names(lsxml), "MD_Metadata.identificationInfo.GSD_Identification.citation.CI_Citation.date.CI_Date.date.DateTime"))],
      1,4
    )

    title <- lsxml[which(stringr::str_detect(names(lsxml), "MD_Metadata.fileIdentifier.CharacterString"))]

    # date <- lsxml[which(stringr::str_detect(names(lsxml), "dateStamp"))] %>% as.Date()
    # an <- date %>% format("%Y")

    title_spl <- stringr::str_split(title, "_")[[1]]
    title_dir <- stringr::str_replace_all(title, "MS_", "PAN_")

    dir_an <- file.path(dir, an)
    dir_name <- file.path(dir, an, title_dir)

    if(!dir.exists(dir_an)) dir.create(dir_an)
    if(!dir.exists(dir_name)) dir.create(dir_name)

    dir_name_sub <- file.path(dir_name, title)

    if(dir.exists(dir_name_sub)){
      message(crayon::red(".....", basename(dir_name_sub), " existe déjà. Les fichiers n'ont pas été décompressés."))

      unlink(tmp)
    }else{
      file.rename(tmp, dir_name_sub)
    }
  }

  if(unlink_tar) unlink(tars)

  # mise à jour du fichier des images -------------------------------------

  # récupération des chemins des images ------------------------------------------

  (dirs <- list.files(dir, pattern = "[0-9]{4}$", full.names = TRUE))

  meta <- list.files(dirs, recursive = TRUE, full.names = TRUE, pattern = "metadata-iso.xml")

  ls <- purrr::map(meta, function(xml_file){
    tryCatch({
      xml <- xml2::as_list(xml2::read_xml(xml_file))
      lsxml <- xml %>% unlist

      # names(lsxml) <- stringr::str_replace_all(names(lsxml), "\\.", "xxx")
      car <- lsxml[which(stringr::str_detect(names(lsxml),
                                             # ""))] %>%
                                             # "fileIdentifier|parentIdentifier|westBoundLongitude|eastBoundLongitude|northBoundLatitude|southBoundLatitude|MD_Metadata.identificationInfo.GSD_Identification.citation.CI_Citation.date.CI_Date.date.DateTime"))] %>%
                                             "fileIdentifier|parentIdentifier|MD_Metadata.identificationInfo.GSD_Identification.citation.CI_Citation.date.CI_Date.date.DateTime"))] %>%
        as.list() %>%
        as.data.frame(stringsAsFactors = FALSE)

      names(car) <- c("identifier", "parent", "time")
      car$xml <- xml_file
      car
      car %>% dplyr::mutate(band = ifelse(endsWith(parent, "-MS"), "MS", "PAN"),
                            date = stringr::str_sub(time, 1, 10),
                            tif = paste(list.files(dirname(xml), pattern = ".TIF", full.names = TRUE, recursive = TRUE), collapse = " xxx "))

    }, error = function(e){NULL}
    )
  })


  ls2 <- ls[which(!purrr::map_lgl(ls, is.null))]

  tb <- do.call(rbind, ls2) %>% dplyr::filter(!duplicated(identifier))

  # les dates proches sont regroupées -------------

  dates <- unique(tb$date) %>% sort() %>% as.Date()

  diff <- as.numeric(dates[2:length(dates)] - dates[1:(length(dates)-1)])

  dates_gr <- dates[1]
  for(n in 1:length(diff)){
    if(diff[n] > 5){
      dates_gr <- c(dates_gr, dates[n+1])
    }else{
      dates_gr <- c(dates_gr, dates_gr[length(dates_gr)])
    }
  }

  tb2 <- tb %>% dplyr::left_join(
    data.frame(date_gr = as.character(dates_gr), date = as.character(dates), stringsAsFactors = FALSE),
    by = "date")

  # appariement MS+PAN : n° identifier égaux ou conjoints (pas d'autres critères ???) ------

  tb3 <- tb2 %>% dplyr::mutate(id = stringr::str_replace_all(identifier, "_", "/") %>%
                                 basename() %>%
                                 stringr::str_remove_all("CP") %>%
                                 as.numeric(),
                               img = as.character(id)) %>%
    dplyr::arrange(id, band)

  for(n in 2:nrow(tb3)){
    if(tb3$band[n] == "MS"){
      if(tb3$band[n-1] == "PAN" & abs(tb3$id[n] - tb3$id[n-1]) < 2){
        tb3$img[n] <- as.character(tb3$id[n-1])
      }else if(n<nrow(tb3)){
        if(tb3$band[n+1] == "PAN" & abs(tb3$id[n] - tb3$id[n+1]) < 2){
          tb3$img[n] <- as.character(tb3$id[n+1])
        }
      }
    }
  }

  # supprime les images n'ayant pas PAN et MS

  freq <- table(tb3$img) %>% as.data.frame(stringsAsFactors = FALSE) %>%
    dplyr::filter(Freq == 1) %>%
    dplyr::pull(Var1)

  if(length(freq) > 0){

    tb3 <- tb3 %>% dplyr::filter(!img %in% freq)
    message("Les images ", paste(tb3 %>% dplyr::filter(img %in% freq) %>%
                                   dplyr::pull(identifier),
                                 collapse = ", "),
            "n'ont pas d'image PAN ou MULTISPECTRALE associée")
  }

  write.csv(tb3, file.path(dir, "data.csv"))



}









