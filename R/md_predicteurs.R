#' Raster de prédicteurs pour spatialisation
#'
#' @param topo
#' @param dendro
#' @param spot
#' @param insol
#' @param mnh
#' @param suppress_rasters_with_na retire des prédicteurs les rasters dont le % de valeurs NA est supérieur au nombre donné (100: garde tout, 0: supprime si une valeur absente)
#' @param res resolution du pixel en m
#'
#' @return spatRaster
#' @export
#'
#'
md_predicteurs <- function(topo = TRUE, dendro = TRUE, spot = TRUE, insol = TRUE,
                             mnh = TRUE, sentinel = TRUE, suppress_rasters_with_na = 100, res = 10,
                             spot_date = "last"
                    ){

  mnt <- uRast("mnt", "last") %>% aggregate(res, "mean", na.rm = TRUE)
  names(mnt) <- "topo_alti"

  pile <- mnt

  if(topo){

    cat("1/5: topographie\n")

    terr <- terrain(mnt, c( "slope", "aspect", "TPI", "TRI", "TRIriley", "TRIrmsd", "roughness", "flowdir"))

    names(terr) <- paste0("topo_", names(terr))
    pile <- c(pile, terr)
  }

  if(dendro){

    cat("2/5: dendrométrie\n")

    tryCatch({
      dendro <- uRast("dendro") %>% resample(mnt)
      names(dendro) <- paste0("dend_", names(dendro))

      pile <- c(pile, dendro)
    }, error = function(e){message("Pas de rasters de prédictions dendrométriques.")}
    )
  }

  if(spot){

    cat("3/5: spot\n")

    tryCatch({
      spot <- uRast("spot", spot_date)

      indices <- spot_formula(list = TRUE)

      ind <- purrr::map(indices, ~eval(parse(text = spot_formula(.x))))

      indc <- do.call(c, ind)
      names(indc) <- paste0("spot_", indices)

      pile <- c(pile, indc %>% aggregate(res, "mean", na.rm = TRUE))

    }, error = function(e){message("Pas de rasters Spot.")}
    )
  }

  if(insol){

    cat("4/5: insolation\n")

    tryCatch({
      inso <- uRast("insolation", "last") %>% resample(mnt)

      pile <- c(pile, inso)
    }, error = function(e){message("Pas de rasters d'insolation.")}
    )
  }

  if(mnh){

    cat("5/5: mnh\n")

    tryCatch({

      mnh <- uRast("mnh", "last") %>% aggregate(res, "mean", na.rm = TRUE)
      names(mnh) <- "mnh_h"

      terr <- terrain(mnh, c( "slope", "aspect", "TPI", "TRI", "TRIriley", "TRIrmsd", "roughness", "flowdir"))

      names(terr) <- paste0("mnh_", names(terr))
      pile <- c(pile, mnh, terr)

    }, error = function(e){message("Pas de rasters issus du MNH")}
    )
  }

  pilec <- pile %>% terra::mask(dc("shp") %>% sf::st_buffer(dc("buffer")))

  if(suppress_rasters_with_na < 100){

    cat("suppression des variables comportant trop de données manquantes\n")

  e <- extract(pilec, dc("shp") %>% dplyr::summarise(geometry = sf::st_union(geometry))) %>%
    dplyr::select( - dplyr::any_of(c("Id", "ID", "id")))

  e <- e[purrr::map_lgl(e, ~length(unique(.x)) > 1)]

  nas <- purrr::map_dbl(e, ~ sum(is.na(.x)) / nrow(e))

  layer2rm <- names(e)[which(nas > (suppress_rasters_with_na /100)) & ! startsWith(names(e), "mnh")]

  if(length(layer2rm) > 0){
    message("le ou les rasters suivant(s) sont exclus par le paramètre de filtre fixant à ",
            suppress_rasters_with_na, "% max la proportion de données manquantes:\n\t",
            crayon::red(paste(layer2rm, collapse = "\t")))

    pilec <- pilec[[names(e)[!names(e) %in% layer2rm]]]
  }
  }

  return(pilec)
}
