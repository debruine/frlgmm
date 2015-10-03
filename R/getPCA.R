#' Get PCA Data
#' 
#' @param data Data structure from geomorph or created by readTem()
#' @return A table summarizing the percent variation explained by each PC and the set of PC scores
#' @examples
#' faces$pca <- getPCA(faces)

getPCA <- function(data) {
  coords <- getCoords(data)
  pca <- prcomp(two.d.array(coords), center = TRUE, scale. = FALSE)
  return(pca)
}