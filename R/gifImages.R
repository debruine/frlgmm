#' Create images for a moving gif
#'
#' @param data Data structure from geomorph or created by readTem()
#' @param pc The PC to manipulate (Defaults to 1)
#' @param dir The directory to save images in (Defaults to working directory)
#' @param start The starting SD (Defaults to -1)
#' @param end The ending SD (Defaults to 1)
#' @param steps The number of steps in the continuum (Defaults to 20)
#' @param height The height of the output png in pixels (Defaults to 400)
#' @param width The width of the output png in pixels (Defaults to 400)
#' @return NULL
#' @examples
#' gifImages(faces, pc = 1, dir = "~/Desktop/sd1")

gifImages <- function(data, pc = 1, dir = "",
                      start = -1, end = 1, steps = 20,
                      height = 400, width = 300) {

  oldpar <- (par(no.readonly = TRUE))
  on.exit(par(oldpar), add = TRUE)
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)

  if (dir == "") {
    dir = getwd()
  } else if (!file.exists(dir) || !file.info(dir)[1, "isdir"]) {
    dir = getwd()
    warning(paste(dir, "was not found"))
  }
  message(paste("Saving images to directory:", dir))

  if (is.null(data$pca)) { data$pca <- getPCA(data) }

  coords <- getCoords(data)
  mean.shape <- geomorph::mshape(coords)

  # set number of dimensions and number of landmarks
  k <- dim(coords)[2]
  p <- dim(coords)[1]

  # create a vector of PC values, start with all 0s
  pc.vec <- rep(0, length(data$pca$sdev))

  if(dev.cur() == 1) dev.new() # open new graphic window
  par(cex = 1) # make symbols normal size
  par(bg = "#FFFFFF")
  par(mar = c(0,0,0,0))

  for (i in 0:steps) {
    pcnt <- (i*(end - start)/steps) + start
    pc.vec[pc] <- pcnt * data$pca$sdev[pc]
    pc.vis <- geomorph::arrayspecs(as.matrix(pc.vec %*% (t(data$pca$rotation))), p, k)[, , 1] + mean.shape
    filename <- paste(dir, "/img", sprintf("%03d", i), ".png", sep="")

    geomorph::plotRefToTarget(mean.shape, pc.vis)
    dev.print(png, filename, height = height, width = width)
  }
}
