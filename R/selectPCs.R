#' Select PCs using different criteria
#'
#' \code{selectPCs} returns a list of significant PCs as chosen by one of 3 methods
#' (from D.A. Jackson (1993). Ecology, 74, 2204-2214). Defaults to the most accurate
#' and conservative "broken-stick"/"bs" method. Other methods are "Kaiser-Guttman"/"kg"
#' (PCs with eigenvalues greater than the mean eigenvalue) and "total variance"/"tv"
#' (PCs explaining at least total.var variance)
#'
#' @param data Data structure from geomorph or created by readTem()
#' @param method The method to use to choose PCs (Default "broken-stick")
#' @param total.var Total variance to choose for "total variance" method (deafult .95)
#' @return A list of chosen PCs
#' @examples
#' chosen.pcs <- selectPCs(data)
#' @export

selectPCs <- function(data, method="broken-stick", total.var = .95) {
  if (is.null(data$pca$sdev)) { data$pca <- getPCA(data); }

  ev <- data$pca$sdev^2
  method <- tolower(method)

  if (method == "broken-stick" | method == "bs") {
    # compare variance explained to null model
    n_ev <- length(ev)
    bsm <- data.frame(
      j = seq(1:n_ev),
      p = 0
    )
    bsm$p[1] <- 1/n_ev
    for (i in 2:n_ev) bsm$p[i] <- bsm$p[i-1] + (1/(n_ev + 1 - i))
    bsm$p <- 100*bsm$p/n_ev
    test <- cbind(100*ev/sum(ev), bsm$p[n_ev:1])
    pc_end <- sum(test[,1] >= test[,2])
  } else if (method == "kaiser-guttman" |  method == "kg") {
    # return PCs with eigenvalues greater than the mean eigenvalue
    pc_end <- sum(ev >= mean(ev));
  } else if (method == "total variance" | method == "tv") {
    # return PCs explaining at least total.var variance
    cumvar <- cumsum(ev / sum(ev));
    pc_end <- sum(cumvar < total.var) + 1;
  }

  pcs <- rep(1:pc_end)
  return(pcs);
}
