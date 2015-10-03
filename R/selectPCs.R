#' Select PCs using different criteria
#' 
#' @param data Data structure from geomorph or created by readTem()
#' @param method The method to use to choose PCs (Default "mean eigenvalue")
#' @return A list of chosen PCs
#' @examples
#' chosen.pcs <- selectPCs(data)

selectPCs <- function(data, method="mean eigenvalue") {
  if (is.null(data$pca$sdev)) { data$pca <- getPCA(data) }
  
  if (method == "mean eigenvalue") {
    ev <- data$pca$sdev^2
    mean.ev <- mean(ev)
    selected.pcs <- sapply(ev, function(x){ ifelse(x >= mean.ev, 1, 0) })
    pcs <- rep(1:sum(selected.pcs))
    
    return(pcs)
  }
}