#' Project (and optionally graph and/or save to a file) a shape using an array of SDs for each PC
#'
#' @param data Data structure from geomorph or created by readTem()
#' @param set.pc An array of values or SDs for each PC (unspecified PCs will be 0) (Defaults to c(0))
#' @param by Whether set.pc specifies "SD" or "value" (Defaults to "SD")
#' @param show.image Whether to display a graph of the resulting array (Defaults to F)
#' @param save.name A file name to save to (Defaults to ""; no saving)
#' @return An array of coordinate values for the resulting shape
#' @examples
#' face.sd3.min <- graphShape(faces, c(0,0,-2), show.image=T)
#' face.sd3.max <- graphShape(faces, c(0,0,2), show.image = F, savename="sd3max.tem")

graphShape <- function(data, set.pc = c(0), by = "SD", show.image = F, save.name = "") {
  # modified from http://www.geomorph.net/2015/06/tips-tricks-9-shape-changes-and.html

  if (is.list(data) && is.null(data$pca)) { data$pca <- getPCA(data); }

  coords <- getCoords(data);
  mean.shape <- geomorph::mshape(coords);

  # set number of dimensions and number of landmarks
  k <- dim(coords)[2];
  p <- dim(coords)[1];

  # create a vector of PC values
  if (by == "value") { # set.pc values are raw values
    pc.vec <- set.pc;
  } else { # set.pc values are SDs
    pc.vec <- set.pc * data$pca$sdev[1:length(set.pc)];
  }
  vec.0s <- rep(0, length(data$pca$sdev) - length(pc.vec));
  pc.vec <- c(pc.vec, vec.0s);

  pc.vis <- geomorph::arrayspecs(as.matrix(pc.vec %*% (t(data$pca$rotation))), p, k)[, , 1] + mean.shape;

  # create image
  if (show.image) {
    oldpar <- (par(no.readonly = TRUE));
    on.exit(par(oldpar), add = TRUE);
    par(cex = 0.5); # make symbols smaller
    par(bg = "#FFFFFF");
    geomorph::plotRefToTarget(mean.shape, pc.vis);
  }

  # save to a file
  if (save.name != "") {
    saveTem(coords = pc.vis, save.name = save.name, data = data);
  }

  return(pc.vis);
}
