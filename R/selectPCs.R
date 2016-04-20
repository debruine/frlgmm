#' Select PCs using different criteria (from D.A. Jackson (1993). Ecology, 74, 2204-2214.)
#'
#' @param data Data structure from geomorph or created by readTem()
#' @param method The method to use to choose PCs (Default "broken-stick")
#' @param total.var Total variance to choose for "total variance" method (deafult .95)
#' @return A list of chosen PCs
#' @examples
#' chosen.pcs <- selectPCs(data)

selectPCs <- function(data, method="broken-stick", total.var = .95) {
  if (is.null(data$pca$sdev)) { data$pca <- getPCA(data); }

  ev <- data$pca$sdev^2;

  if (method == "broken-stick") {
    # compare variance explained to null model
    p <- length(ev);
    i <- rep(1:p);
    b.k <- sapply(i, function(x) sum(1/i[x:p]));
    var <- ev*100/sum(ev);
    pc.end <- sum(var >= b.k);
  } else if (method == "Kaiser-Guttman") {
    # return PCs with eigenvalues greater than the mean eigenvalue
    pc.end <- sum(ev >= mean(ev));
  } else if (method == "total variance") {
    # return PCs explaining at least total.var variance
    cumvar <- cumsum(ev / sum(ev));
    pc.end <- sum(cumvar < total.var) + 1;
  }

  pcs <- rep(1:pc.end)
  return(pcs);
}
