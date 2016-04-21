#' Create a graph plotting two PCs and visualising min and max for those PCs.
#'
#' @param data Data structure from geomorph or created by readTem()
#' @param axis1 The PC to be shown on the x axis (Defaults to 1)
#' @param axis2 The PC to be shown on the y axis (Defaults to 2)
#' @param gp An array of group names for each shape (Defaults to NULL array: c())
#' @param col.gp An array of colours for the groups (Defaults to ROYGBP: c("#d11141", "#f47835", "#ffc425", "#00b159", "#00aedb", "#a200ff"))
#' @return TRUE
#' @examples
#' faces$pca <- graphCompare2(faces$aligned$coords, 2, 3)
#' @export

graphCompare2 <- function(data, axis1 = 1, axis2 = 2, gp = c(),
                          col.gp = c("#d11141", "#f47835", "#ffc425", "#00b159", "#00aedb", "#a200ff")) {

  oldpar <- (par(no.readonly = TRUE))
  on.exit(par(oldpar), add = TRUE)

  coords <- getCoords(data)
  PCA <- geomorph::plotTangentSpace(coords, axis1 = axis1, axis2 = axis2, verbose = T, warpgrids = FALSE)
  mean.shape <- geomorph::mshape(coords)
  xlab <- paste("Principal Component ", axis1, " (", round(PCA$pc.summary$importance[2,
                                                                                     axis1] * 100, 1), "%)", sep = "")
  ylab <- paste("Principal Component ", axis2, " (", round(PCA$pc.summary$importance[2,
                                                                                     axis2] * 100, 1), "%)", sep = "")

  if (length(gp) > 0) {
    gp.levels <- levels(as.factor(gp))
    nlevels <- length(gp.levels)
    col.gp <- rep_len(col.gp, nlevels)
    # make sure col.gp is same length as number of gp.levels by repeating col
    names(col.gp) <- gp.levels
    col.gp <- col.gp[match(gp.levels, names(col.gp))]
  }

  mat <- matrix(c(4, 5, 0, 1, 1, 2, 1, 1, 3), 3)
  layout(mat, widths = c(1, 1, 1), heights = c(1, 1, 1)) # set the size of the rows and columns
  par(mar = c(4, 4, 1, 1)) # sets the margins
  plot(PCA$pc.scores[, axis1], PCA$pc.scores[, axis2], pch = 21, cex = 2, bg = col.gp,
       xlab = xlab, ylab = ylab, asp = T)

  if (length(gp) > 0) {
    legend(-0.09, 0.07, legend = unique(as.factor(gp)), pch = 19, col = unique(col.gp))
  }
  # Plot the four TPS grids
  par(mar = c(0, 0, 0, 0)) # sets the margins
  par(cex = 0.5) # make the points smaller
  par(cex.sub = 2)
  method <- "TPS"

  geomorph::plotRefToTarget(mean.shape, PCA$pc.shapes[[1]], method = method)
  geomorph::plotRefToTarget(mean.shape, PCA$pc.shapes[[2]], method = method)
  geomorph::plotRefToTarget(mean.shape, PCA$pc.shapes[[3]], method = method)
  geomorph::plotRefToTarget(mean.shape, PCA$pc.shapes[[4]], method = method)

  return(TRUE)
}
