#' Sélection automaatique de variables
#'
#' @param x
#' @param y
#' @param family
#' @param cutoff
#' @param nb_fact
#'
#' @return
#' @export
#'
spa_select_vars <- function (x, y, family = NULL, cutoff = 0.75, nb_fact = 5)
{
  x = x[, purrr::map_lgl(x, ~length(unique(.x)) > 1)]
  x = x[, purrr::map_lgl(x, ~length(.x[is.na(.x)]) == 0)]
  cat("\nsélection des variables (lasso)")
  xm = model.matrix(y ~ ., data = x)
  xm = as.data.frame(xm)
  xm = xm[, !stringr::str_detect(colnames(xm), "val.X.")]
  xm = xm[, !stringr::str_detect(colnames(xm), "Intercept")]
  correlationMatrix <- cor(xm)
  highlyCorrelated <- caret::findCorrelation(correlationMatrix, cutoff = cutoff,
                                      exact = T)
  xm = xm[, -highlyCorrelated]
  xm = model.matrix(y ~ ., data = xm)
  xm = xm[, !stringr::str_detect(colnames(xm), "Intercept")]
  if (is.null(family)) {
    yu = unique(y)
    if (class(yu) == "character") {
      family = "category"
    }
    else if (sum(yu) == 1 & length(yu) == 2) {
      family = "binomial"
    }
    else if (is.numeric(yu)) {
      family = "gaussian"
    }
    else {
      family = "category"
    }
  }
  if (family %in% c("binomial")) {
    alpha = 1
    varsel = NULL
    while (length(varsel) == 0) {
      print(alpha)
      lasso_model_cv = glmnet::cv.glmnet(xm, y, family = family,
                                         alpha = alpha)
      numero_du_best_model = which(lasso_model_cv$lambda ==
                                     lasso_model_cv$lambda.min)
      vl = lasso_model_cv$glmnet.fit$beta[, numero_du_best_model]
      varsel = names(vl[vl != 0])
      length(varsel)
      alpha = alpha - 0.1
    }
    stop_if(length(varsel) == 0, msg = "Les valeurs des placettes ne permettent pas de construire un modèle valide.")
  }
  else {
    varsel <- colnames(xm)
  }
  varsel = as.data.frame(stringr::str_split(stringr::str_remove_all(varsel, "`"),
                                   ".X.", simplify = T))
  if (is.null(varsel$V2))
    varsel$V2 <- ""
  varsel_num = varsel %>% dplyr::filter(V2 == "")
  x2 = x %>% dplyr::select(as.character(stringr::str_remove_all(varsel_num$V1,
                                                       "\\\\")))
  varsel_cat = varsel %>% dplyr::filter(V2 != "")
  if (nrow(varsel_cat) > 0) {
    for (li in 1:nrow(varsel_cat)) {
      f = paste0(as.character(varsel_cat$V1[li]), ".X.")
      ff = as.character(varsel_cat$V2[li])
      xi = x[[f]]
      xi[x[[f]] != ff] = 0
      xi[x[[f]] == ff] = 1
      x2[[paste0(f, ff)]] = as.numeric(xi)
    }
  }
  if (family == "binomial") {
    nb_var_sel <- length(levels(varsel$V1))
  }
  else {
    nb_var_sel <- nrow(varsel)
  }
  if (nb_var_sel > nb_fact) {
    message("sélection des variables du meilleur modèle à ",
            nb_fact, " variables")
    if (family == "gaussian") {
      best_models_forward = try(leaps::regsubsets(y ~ ., data = as.matrix(cbind(y,
                                                                         xm)), nvmax = nb_fact))
      if (class(best_models_forward) == "try-error") {
        best_models_forward = try(leaps::regsubsets(y ~ ., data = cbind(y,
                                                                 xm) %>% as.data.frame, nvmax = nb_fact, really.big = TRUE))
      }
    }
    else {
      best_models_forward = try(leaps::regsubsets(y = as.factor(y),
                                           x = x2, nvmax = (nb_var_sel - 1)))
      if (class(best_models_forward) == "try-error") {
        best_models_forward = leaps::regsubsets(y = as.factor(y),
                                         x = x2, nvmax = nb_var_sel - 1, really.big = T)
      }
    }
    summ <- summary(best_models_forward)
    outmat <- summ$outmat
    outmat[outmat == " "] <- 0
    outmat[outmat == "*"] <- 1
    mode(outmat) <- "numeric"
    outmat <- as.data.frame(outmat)
    col_shp <- stringr::str_split(colnames(outmat), ".X.", simplify = TRUE)
    if (ncol(col_shp) == 2) {
      col_shp <- unique(col_shp[col_shp[, 2] != "", 1])
      for (col in col_shp) {
        col_a_sommer <- colnames(outmat)[stringr::str_detect(colnames(outmat),
                                                    col)]
        if (length(col_a_sommer) == 1) {
          outmat[[col]] <- outmat[[col_a_sommer]]
          outmat[[col_a_sommer]] <- NULL
        }
        else {
          colsum <- apply(outmat[, col_a_sommer], 1,
                          sum)
          outmat <- outmat[, !colnames(outmat) %in% col_a_sommer]
          outmat[[col]] <- colsum
        }
      }
    }
    outmat[outmat > 1] <- 1
    outmat$nb <- apply(outmat, 1, sum)
    outmat$num <- 1:nrow(outmat)
    outmat$bic <- summ$bic
    outmat$r2 <- summ$adjr2
    outmat$rsq <- summ$rsq
    outmat$rss <- summ$rss
    outmat$cp <- summ$cp
    of <- outmat %>% filter(nb <= nb_fact)
    if (nrow(of) == 0) {
      cat(crayon::bgRed("\n Aucun modèle valable n'a été trouvé \n"))
      return(NULL)
    }
    lig <- of %>% arrange((cp)) %>% select(num) %>% slice(1) %>%
      pull()
    cat("\nVariables finales retenues:\n..........................\n")
    sel = names(summ$which[lig, ])[summ$which[lig, ]]
    sel = sel[sel != "(Intercept)"]
    cat(paste0(sel, "\n"))
    sel = stringr::str_remove_all(sel, "`")
    x2 <- x %>% select(sel)
  }
  return(x2)
}
