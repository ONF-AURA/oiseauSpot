#' Série temporelle des images spot
#'
#' @param paths liste des chemins d'images spot d'une même tuile. Si non généré par data_spot(), nommée par l'année de prise de vue
#' @param ext sf de découpage
#' @param buffer buffer à appliquer à ext
#' @param ignore liste des dates à ignorer, au format 2022-02-25
#' @param template spatraster sur lequel s'aligner
#'
#' @return liste de spatRasters 4 bandes
#' @export
#'
spot_get <- function(paths, ext = NULL, buffer = 10, ignore = NULL, template = NULL){

  if(any(is.na(as.numeric(names(paths))))){
    stop("La liste des chemins doit être nommée selon l'année de prise de vue")
  }

  spots <- purrr::map(names(paths), function(an){
    rs <- terra::rast(paths[an])
    names(rs) <- rev(c("b", "g", "r", "ir"))
    rs
  })

  if(is.null(names(paths))){

    # noms issus de data_spot: spot20220602.tif
    # ne conserver qu'une image par an

    if(!is.null(ignore)){
      paths2 <- paths[!stringr::str_detect(basename(paths),
                                          paste(
                                            stringr::str_remove_all(ignore, "-"),
                                            collapse = "|")
                                          )]
    }

    ans <- stringr::str_sub(basename(paths), 5, 8)

    if(sum(duplicated(ans)) > 0){
      message("la ou les année(s) ", paste(ans[duplicated(ans)], collapse = ", "), " disposent de plusieurs images")
      for(an in ans[duplicated(ans)]){
        i <- basename(paths)[startsWith(basename(paths), paste0("spot", an))] %>%
          stringr::str_remove("spot") %>% stringr::str_remove(".tif") %>%
          as.Date("%Y%m%d")
        message("Année ", an, " : ",paste(i, collapse = ", "))
      }
      message("Utiliser l'argument 'ignore' pour éliminer des dates.")
    }
  }

  names(spots) <- paste("spot", names(paths), sep = "")

  ref_crop <- as(ext %>% sf::st_buffer(buffer), "SpatVector")

  if(!is.null(ext)){
    spots <- purrr::map(spots,
                 ~ terra::crop(.x, ref_crop))

      spots <- purrr::map(spots,
                          ~ terra::mask(.x, ref_crop))

  }

  if(!is.null(template)){
    spots <- purrr::map(spots, ~terra::resample(.x, template %>% terra::crop(ref_crop)))
  }

  spots
}

