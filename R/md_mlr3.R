#' Modèle par package mlr3: selection des variables explicatives
#'
#' @param data table
#' @param m méthode: 2nde partie de la clé d'apprentissage. Voir lrns()
#' @param y nom du champ à prédire
#' @param op classif, clust ou regr
#' @param opt liste des options. Ex: list(rpart = "cp = 0.01")
#' @param n_max nb max à sélectionner
#' @param test TRUE pour md_mlr3_compare
#'
#' @return liste des variables retenues
#' @export
#'
md_mlr3_select <- function(data, m, y, op = "classif", opt = NULL, n_max = 8,
                           test = FALSE){

  ls <- md_mlr3_learner(data = data, m = m, y = y, op = "classif",
                        opt = NULL, n_max = 8)

  afs = auto_fselector(
    fselector = fs("random_search", max_features = n_max),
    learner = ls$lea,
    resampling = rsmp ("holdout"),
    measure = msr("classif.ce"),
    term_evals = 4)

  resampling_outer = rsmp("cv", folds = 2)
  rr <- mlr3::resample(ls$task, afs, resampling_outer, store_models = TRUE)

  if(test){return(rr)}

  extract_inner_fselect_results(rr)$features %>% unlist() %>% unique()

}


md_mlr3_modele <- function(data, m, y, op = "classif", opt = NULL, n_max = 8){

  data$yyy <- data[[y]]
  data[[y]] <- NULL

  task = as_task_classif(yyy ~ ., data = data)


  if(m %in% names(options)){
    lea <- eval(parse(text = paste0('lrn(paste0(op, ".", m), ', opt,')')))
  }else{
    lea <- lrn(paste0(op, ".", m))
  }


  resampling_outer = rsmp("cv", folds = 3)

  mlr3::resample(task, lea, resampling_outer, store_models = TRUE)

}


md_mlr3_learner <- function(data, m, y, op = "classif", opt = NULL, n_max = 8){

  data$yyy <- data[[y]]
  data[[y]] <- NULL

  task = as_task_classif(yyy ~ ., data = data)


  if(m %in% names(options)){
    lea <- eval(parse(text = paste0('lrn(paste0(op, ".", m), ', opt,')')))
  }else{
    lea <- lrn(paste0(op, ".", m))
  }

  return(list(task = task, lea = lea))

}

# ----------------------------------------------------------------

#' Comparatif des méthode d'apprentissage
#'
#' @param options
#' @param data table
#' @param y nom du champ à prédire
#' @param op classif, clust ou regr
#' @param meth liste des méthodes à tester: 2nde partie de la clé d'apprentissage. Voir lrns()
#' @param options liste des options. Ex: list(rpart = "cp = 0.01")
#'
#' @return
#' @export
#'
#' @examples
md_mlr3_compare <- function(data, y, op = "classif", meth = c("kknn", "naive_bayes", "nnet", "glmnet", "svm", "rpart",
                                                                          "featureless", "lda", "log_reg", "multinom", "qda",
                                                                          "ranger", "xgboost"),
                            options = list(rpart = "cp = 0.01")){


  ls <- list()


  for(m in meth){

    rr <- try(md_mlr3_select(data, m, y = y, op = op, opt = options[[m]],
              test = TRUE))
    if(!inherits(rr, "try-error")){

      cf = rr$prediction()$confusion

      ls[[m]] <- list(score = rr$aggregate(),
                      conf = cf,
                      tx = map_dbl(1:nrow(cf), ~cf[.x, .x]) %>% sum() / sum(cf),
                      txi = map(1:nrow(cf), ~cf[.x, .x]/sum(cf[.x,])))
      names(ls[[m]]$txi) <- colnames(cf)


    }
  }

  ls
}
# fam <- fun(cal_fam)
#
# tout <- fun(cal_s, meth = c("ranger", "kknn", "rpart"))
#


#' Présentation des données issues de md_mlr3_compare()
#'
#' @param ls liste issue de md_mlr3_compare()
#'
#' @return table
#' @export
#'

md_mlr3_compare_as_table <- function(ls){
  txe <- map(ls, function(.x){
    xx <- .x$txi
    names(xx) <- str_replace(names(xx), "\\.", "_")
    xx
  }) %>% as.data.frame() %>% t()
  data.frame(val = txe[,1], txt = rownames(txe)) %>%
    mutate(mod = str_split_i(txt, "\\.", 1),
           ess = str_split_i(txt, "\\.", 2)) %>%
    select(-txt) %>%
    tidyr::pivot_wider(names_from = ess, values_from = val) %>%
    mutate(TOT = map_dbl(ls, ~.x$tx)) %>%
    arrange(desc(TOT))
}
