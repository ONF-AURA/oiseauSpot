


#' Classification automatique par CAH ou kmeans
#'
#' @param data table
#' @param methode kmeans ou cah
#' @param nb_cat NULL ou nombre de classes voulu
#'
#' @return liste: cats = catégories, vars = variables représentatives
#' @export
#'

md_new_classif <- function(data, methode = "kmeans", nb_cat = NULL){


  if(methode != "kmeans"){

  z <- data %>% as.matrix()

  # suppression des champs à valeur unique
  rm <- which(apply(z,2,sd) == 0)

  if(length(rm) > 0){
    data <- data[, -rm]
    z <- data %>% as.matrix()
  }

  means <- apply(z,2,mean)
  sds <- apply(z,2,sd)
  nor <- scale(z,center=means,scale=sds)

  vrs <- apply(nor,2,var)


  distance = dist(nor)

  data.hclust = hclust(distance)

  par(mfrow = c(5, 4), mar = c(2,2,1,1))

  ncat <- map(3:min(nrow(data), 22), function(n){
    ct <- table(cutree(data.hclust,n))
    plot(ct)
    text(ct, adj = 0, srt = 90, labels = as.character(ct), col = "red")
  })
  par(mfrow = c(1, 1))

  choix <- readline(crayon::green("nb de catégories à utiliser:\n"))

  ct <- cutree(data.hclust,choix)

  dtrf <- as.data.frame(z)

  }else{
    if(is.null(nb_cat)){
      util_log("md_new_classif", "nb_cat obligatoire pour methode kmeans")
      return("ko")
    }
    k <- kmeans(data, centers = nb_cat)
    ct <- k$cluster

    dtrf <- data

  }

  names(dtrf) <- str_replace_all(names(dtrf), "-", "_")
  dtrf$cat <- ct %>% as.factor()


  # recherche des variables explicatives-----------------------

  data_rf <- dtrf %>% slice_sample(by = "cat", n = 100)

  mod <- md_new_model_rf(data_rf, "cat", pvalid = 0, nb_cat = 8)

  return(list(cats = ct, vars = mod$importance %>% rownames()))
}



