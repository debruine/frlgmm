#' Create a graph comparing min and max values on one PC
#'
#' @param data Data structure from geomorph or created by readTem()
#' @param pc The PC to be visualised (Defaults to 1)
#' @param mag The magnification factor of the visualised difference (Deafults to 1)
#' @param method The graphing method for plotRefToTarget (Defaults to "TPS")
#' @examples
#' faces$pca <- graphMinMax(faces, 2)

graphMinMax <- function(data, pc = 1, mag = 1, method = "TPS") {
  oldpar <- (par(no.readonly = TRUE))
  on.exit(par(oldpar), add = TRUE)

  coords <- getCoords(data)
  PCA <- geomorph::plotTangentSpace(coords, axis1 = pc, axis2 = pc, verbose = T, warpgrids = FALSE)

  lb <- paste("Principal Component ", pc, " (", round(PCA$pc.summary$importance[2,
                                                                                pc] * 100, 1), "%)", sep = "")

  layout(matrix(c(1, 2), 2, 2, byrow = TRUE))
  par(mar = c(0, 0, 0, 0)) # sets the margins
  par(cex = 0.5)
  par(cex.sub = 2)

  # Plot the two TPS grids
  mean.shape <- geomorph::mshape(coords)
  geomorph::plotRefToTarget(mean.shape, PCA$pc.shapes[[1]], mag = mag, method = method)
  geomorph::plotRefToTarget(mean.shape, PCA$pc.shapes[[2]], mag = mag, method = method)
}
