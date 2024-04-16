#' Suppresion de variables hautement corrélées
#'
#' @param df table
#' @param lim correlation max tolérée
#'
#' @return
#' @export
#'

md_rm_corr_var <- function(df, lim = .9){


  dfn <- df %>% select_if(is.numeric)

  cor_matrix_rm <- cor(dfn)                  # Modify correlation matrix
  cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
  diag(cor_matrix_rm) <- 0
  cor_matrix_rm

  names(dfn)[which(
    apply(cor_matrix_rm,2,
        function(x) any(x > lim))
  )]

}
