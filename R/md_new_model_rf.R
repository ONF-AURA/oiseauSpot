#' md_new_model_rf
#'
#' @param tab table des prédicteurs
#' @param col_y nom de la colonne à prédire
#' @param pvalid taux de lignes à conserver pour la validAtion
#'
#' @return modèle RF
#' @export
#'
md_new_model_rf <- function(tab, col_y, pvalid = .2, nb_cat = NULL){

  if(! col_y %in% names(tab)){
    util_log('md_model_rf', col_y, " n'est pas un champ de la table fournie.")
    return("ko")
  }


  tab$y <- tab[[col_y]]
  tab[[col_y]] <- NULL



  if(pvalid > 0){
    set.seed(1)
    verif <- sample.int(nrow(tab), round(pvalid * nrow(tab)))
    train <- tab %>% slice(-verif)
  }else{
    train <- tab
    verif <- 1:nrow(tab)
  }


  table(train$y)

  # randomForest

  sel <- varSelRF::varSelRF(xdata = train %>% select(-y), Class = train$y,
                            whole.range = TRUE)

  sel <- sel$selec.history

  test <- purrr::map(sel$Number.Variables[which(sel$Number.Variables < 10)], function(choix){

    (vars <- sel$Vars.in.Forest[which(sel$Number.Variables == choix)] %>% as.character() %>% strsplit(" \\+ ") %>% unlist)

    model <- randomForest::randomForest(train$y ~ ., data = train %>% select(all_of(vars)),
                                        importance = TRUE)


    valid <- tab %>% slice(verif)
    valid$pr <- predict(model, valid %>% select(-y))
    valid <- valid %>% mutate(err = (pr != y))

    list(matconf = table(valid$pr, valid$y), err = mean(valid$err), vars = vars)
  })

  dferr <- data.frame(
    n_vars = purrr::map_int(test, ~.x$vars %>% length),
    errs = round(purrr::map_dbl(test, ~.x$err), 3) * 100,
    vars = purrr::map_chr(test, ~ paste(.x$vars, collapse = " + "))
  )


  plot(dferr %>% select(-vars))


  if(is.null(nb_cat)){
    choix <- 99999999
  }else{

    diffs <- abs(dferr$n_vars - nb_cat)
    choix <- dferr$n_vars[which(diffs == min(diffs))[1]]
  }

  while(! as.numeric(choix) %in% dferr$n_vars){

    util_console_table(dferr, title = "nb var | %err", col = c("red", "black", "black"), ali = c("center", "left", "left"))

    choix <- readline(cat(paste0(
      ifelse(choix == 99999999, "", crayon::red(paste0("Le modèle à ", choix, " variable n'existe pas.\n"))),
      crayon::inverse("Sélectionnez le nombre de variables à utiliser :\n"))))

  }



  x_sel <- test[[which(dferr$n_vars == as.numeric(choix))]]$vars

  model <- randomForest::randomForest(tab$y ~ ., data = tab %>% select(all_of(x_sel)),
                                      importance = TRUE)

  message("variables sélectionnées : ", paste(x_sel, collapse = " + "))

  model
}
