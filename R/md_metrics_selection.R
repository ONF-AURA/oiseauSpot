

#' Sélection de variables numériques
#'
#' @param data dataframe
#' @param y champ à prédire
#'
#' @return nom des variables sélectionnées
#' @export
#'
md_metrics_selection <- function(data, y, big=FALSE){

  data$y <- data[[y]]

  if(y != "y") data[[y]] <- NULL


  rg <- leaps::regsubsets(y ~ ., data = data, really.big = big)
  reg.summary = summary(rg)

  # par(mfrow=c(2 ,2))
  # plot(reg.summary$rss , xlab =" Number of Variables " , ylab =" RSS " ,type ="l")
  # plot(reg.summary$adjr2 , xlab =" Number of Variables " , ylab =" Adjusted RSq " , type ="l")
  # points(which.max(reg.summary$adjr2), reg.summary$adjr2[which.max(reg.summary$adjr2)], col =" red " , cex =2 , pch =20)
  #
  # plot(reg.summary$cp , xlab =" Number of Variables " , ylab =" Cp " ,type = "l")
  #
  #
  # points(which.min(reg.summary$cp), reg.summary$cp[which.min(reg.summary$cp)], col =" red " , cex =2 , pch =20)
  #
  # plot(reg.summary$bic , xlab =" Number of Variables " , ylab =" BIC " , type ="l")
  #
  # points(which.min(reg.summary$bic), reg.summary$bic[which.min(reg.summary$bic)], col =" red " , cex =2 , pch =20)
  #
  # par(mfrow=c(1, 1))


  nbest <- c(which.max(reg.summary$adjr2),
             which.min(reg.summary$cp),
             which.min(reg.summary$bic)) %>% modal() %>% min()

  mbest <- reg.summary$which[nbest,]

  names(mbest)[mbest][-1]
}
