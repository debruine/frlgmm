#' Create a data frame with data from data$info and PCs from data$pca
#' 
#' @param data Data structure from geomorph or created by readTem()
#' @param pcs An array of PCs to add (Default NULL = include all)
#' @return A data frame
#' @examples
#' mydata <- saveData(data, pcs = rep(1:10), save.name = "~/Desktop/mydata.csv")

getData <- function(data, pcs = c()) {
  if (is.null(data$info)) stop("There is no data information")
  
  if (is.null(data$pca$x)) {
    data$pca <- getPCA(data)
  }
  
  info.n <- dim(data$info)[1]
  pca.n <- dim(data$pca$x)[1]
  
  if (info.n != pca.n) stop(paste("The info and PCA are not compatible 
                                    (info has", info.n, "items, 
                                    PCA has", pca.n, "items"))
  
  info <- as.data.frame(data$info)
  scores <- as.data.frame(data$pca$x[, pcs])
  mydata <- cbind(info,scores)
  
  return(mydata)
}