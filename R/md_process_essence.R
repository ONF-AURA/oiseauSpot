#' Création interactive du raster des essences
#'
#' @param defaut_train nom
#'
#' @return rien, raster accessible avec data_crowns_essences()
#' @export
#'
md_process_essence <- function(defaut_train = "crown_ess_new2_ain_res1-spot2018.rds"){

  message("Choix du modèle")
  new <- askYesNo("Voulez-vous créer un modèle prédictif d'essence?\nSi non, vous devrez choisir parmis les modèles existants.")


  if(new){
    md_process_essence_new_model(defaut_train = defaut_train)
  }

  tab <- md_process_essence_predict()

  md_process_essence_rasterize()
}

#' Prédiction de l'essence des couronnes
#'
#' @param nom_modele modèle, sans prefix mlr3ess_ et sans extention .rds
#' @param dos_modele dossier
#'
#' @return table, cr=id couronne, pred=prediction
#' @export
#'

md_process_essence_predict <- function(nom_modele = NULL, dos_modele = dc("dos_modeles_oiseau")){

  if(is.null(nom_modele)){
    mds <- list.files(dos_modele, pattern = "mlr3") %>% str_remove(".rds") %>% str_remove("mlr3")

    nom_modele <- utils::select.list(mds, title = "Modèle à appliquer")
  }

  path_rds <- file.path(dos_modele, paste0("mlr3", nom_modele, ".rds"))

  if(!file.exists(path_rds)){stop(paste(path_rds, " n'existe pas"))}

  lea <- readRDS(path_rds)


  fea <- lea$state$data_prototype %>% names()
  fea <- fea[-1]

  message("création des asters prédictifs")
  pile <- md_mlr3_predicteurs(fea, res = 1, spot_an = 2018)


  message("extraction caractéristiques des couronnes")
  data <- util_extract(pile, "cr")

  message("prédiction")
  data0 <- data %>% na.omit()
  pr <- lea$predict_newdata(data0)
  data0$pred <- pr$response

  tab <- rbind(data0,
               data %>% filter(!cr %in% data0$cr) %>% mutate(pred = NA_character_))
  tab
}


#' Choix des données de calibration des modèles essences
#'
#' @param nom_ref nom du jeu de calibration ou NULL pour choix interactif
#' @param defaut_train jeu par défaut
#' @param path_cal NULL ou chemin des couronnes de calibration créés avec md_ref_crowns_params()
#'
#' @return table
#' @export
#'

md_process_essence_select_train <- function(path_cal = NULL, nom_ref = NULL, defaut_train = NULL){

  if(is.null(path_cal)){
    if(is.null(nom_ref)){

      nms <- list.files(dc("dos_modeles_oiseau"), pattern = "crown_") %>%
        stringr::str_remove_all("crown_") %>% tools::file_path_sans_ext()

      num_defaut <- which(nms == basename(defaut_train) %>%
                            stringr::str_remove_all("crown_") %>% tools::file_path_sans_ext())

      titre_defaut <- paste(nms[num_defaut], "(ref)")

      nms <- c(titre_defaut, nms[-num_defaut])

      nom_ref <- utils::select.list(nms, title = "Choisissez un jeu de couronnes de référence :")
      nom_ref <- str_remove(nom_ref, " \\(ref\\)")
    }
    path_cal = file.path(dc("dos_modeles_oiseau"), paste0("crown_", nom_ref, ".rds"))
  }



  cal0 <- readRDS(path_cal) %>% dplyr::mutate(id = rownames(.))

  res <- try(str_extract(basename(path_cal), "[-|_]res[0-9]?[-|_]") %>%
               str_extract("[0-9]") %>% as.numeric()
  )
  an_spot <- try(str_extract(basename(path_cal), "spot[0-9]{4}") %>%
                   str_extract("[0-9]{4}") %>% as.numeric()
  )

  ys <- names(cal0)[which(startsWith(names(cal0), "y__"))]

  if(length(ys) == 0){stop("données de calibration invalides: les champs à prédire doivent avoir pour prefixe y__", path_cal)}
  if(inherits(res, "try-error")){stop("Nom du fichier de calibration invalide: res absent", path_cal)}
  if(inherits(an_spot, "try-error")){stop("Nom du fichier de calibration invalide: spot (année) absent", path_cal)}

  cal0 %>%
    na.omit() %>% select(-id) %>%
    mutate_if(is.character, as.factor) %>%
    select(! tidyselect::contains("flowdir"))
}


#' Création d'un modèle d'essences
#'
#' @param defaut_train nom du jeu de calibration utilisé par défaut
#' @param dest dossier des modèles oiseau
#' @param name nom à donner au modèle
#'
#' @return
#' @export
#'

md_process_essence_new_model <- function(defaut_train, dest = dc("dos_modeles_oiseau"),
                                         name = NULL){

  cal <- md_process_essence_select_train(defaut_train = defaut_train)

  ys <- names(cal)[which(startsWith(names(cal), "y__"))]

  calx <- cal %>% select(-any_of(ys))
  caly <- cal %>% select(any_of(ys))

  codes <- read.csv(dc("cat00_essences")) %>% filter(code %in%  caly[[ys]]) %>%
    select(code, famille) %>%
    mutate_all(as.character) %>%
    rename(y__ess = code, y__fam = famille)
  caly <- left_join(caly, codes)

  ys <- names(caly)

  # choix de la méthode

  meth_all <- str_remove(mlr3::lrns()$keys("classif."), "classif.")
  meth = c("ranger", "log_reg")

  test <- askYesNo(paste(c("Voulez-vous tester les différentes méthodes", paste(meth, collapse = ", "))))

  if(test){
    l <- list()
    for(yy in ys){

      ls <- md_mlr3_compare(cbind(caly %>% select(yy), calx),
                            y = yy, op = "classif",
                            meth = meth)
      l[[yy]] <- md_mlr3_compare_as_table(ls)
    }

    print(l)
  }

  y <- utils::select.list(ys, title = "Choisissez la variable à prédire :")
  m <- utils::select.list(meth, title = "Choisissez la méthode :")

  # selection des variables

  message("Première sélection de variables")
  fea0 <- md_mlr3_select(cbind(caly %>% select(y), calx), m = m, y = y,
                         op = "classif", opt = NULL)

  # données douteuses

  messages("Suppression des données douteuses ('outliers')")

  cal2 <- util_remove_outliers(cbind(caly %>% select(y), calx),
                               y, fea0)

  fea <- md_mlr3_select(cal2, m = m, y = y,
                        op = "classif", opt = NULL)

  dataf <- calx %>% select(fea) %>% mutate(y=caly[[y]])

  task = as_task_classif(y ~ ., data = dataf)
  lea <- lrn(paste0("classif.", m))
  lea$train(task)

  print(lea$oob_error())

  if(is.null(name)){
    name <- rstudioapi::showPrompt("Enregistrement du modèle", "Nom du modèle créé:")
  }

  if(is.null(name)) return("opération abandonnée.")

  pref <- paste0("mlr3", str_remove(y, "y__"), "_")
  olds <- list.files(dest, pattern = pref)

  if(paste0(pref, name) %in% olds){
    name <- rstudioapi::showPrompt(paste("Le modèle", name, "existe déjà."),
                                   paste("Nom du modèle créé (existants:", paste(olds, collapse = ", ")))
  }

  if(is.null(name)) return("opération abandonnée.")

  saveRDS(lea, file.path(dest, paste0(pref, name, ".rds")))

}


#' Raster des essences à res=1m
#'
#' @param tab cr = id crown (cf data_crowns), pred = code essence à 3 caractères
#' @param dest chemin d'écriture du tif
#'
#' @return écrit tif et csv des cat
#' @export
#'

md_process_essence_rasterize <- function(tab, dest = dc("path_essences")){

  r_cr <- data_crowns()

  ess_id <- tab$pred %>% levels()

  rcl <- data.frame(is = tab$cr, becomes = tab$pred %>% as.integer(),
                    stringsAsFactors = FALSE) %>% as.matrix()

  r_ess <- terra::classify(r_cr, rcl)

  cat_ess <- read.csv(file.path(system.file(package = "oiseauData"), "tables", "cat", "essences.csv"),
                      stringsAsFactors = FALSE)

  levels(r_ess) <- data.frame(id = 1:length(ess_id),
                              label = ess_id)

  coltab(r_ess) <- cat_ess %>% filter(type %in% ess_id) %>%
    arrange(type) %>%
    mutate(value = 1:nrow(.)) %>%
    select(value, couleur_groupe)


  # tmp <- uCropId(r_ess, "NOUVS1")
  # visu_raster(tmp=tmp)

  writeRaster(r_ess, dest, overwrite = TRUE)
  write.csv(cats(r_ess)[[1]],
            str_replace(dest, ".tif", ".csv"),
            row.names = FALSE)


  message("raster écrit sous ", dest)

}

