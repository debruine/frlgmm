#' Read Psychomorph template files from a directory or list of directories.
#' 
#' @param tem.loc Directory or csv table of filenames where .tem files are found
#' @param semi.landmarks Number of curve points to graphically define
#' @param curvefile Name of curvepts file
#' @return A data structure ready for geomorph, with land, curvepts, and file info
#' @examples
#' faces <- readTems("~/Dropbox/faces")
#' faces <- readTems("~/Dropbox/faces/faceinfo.csv", curvefile = "~/Dropbox/faces/curveslide.csv"))

readTems <- function(tem.loc, semi.landmarks = 0, curvefile = "") {
  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  
  coordinates <- list()
  
  if (file.info(tem.loc)[1, "isdir"]) {
    # get all tem files in each directory
    message(paste("Getting tem files from directory:", tem.loc))
    setwd(tem.loc)
    temfiles <- dir(pattern = ".tem", recursive = TRUE)
    teminfo <- as.data.frame(list(filename = temfiles))
  } else if (file.exists(tem.loc)) {
    message(paste("Getting tem files from table:", tem.loc))
    teminfo <- read.csv(tem.loc, header=T, stringsAsFactors = FALSE)
    temfiles <- teminfo$filename
    setwd(dirname(tem.loc))
  } else {
    stop(paste(tem.loc, "is neither a directory or a list of files"))
  }
  
  if (length(temfiles) == 0) {
    stop("No tem files were found")
  } else {
    message(paste("Tem files found:", length(temfiles)))
  }
  
  if (is.null(teminfo$id)) {
    teminfo$id <- teminfo$filename
  }
  
  dimnames(teminfo)[[1]] <- as.list(teminfo$id)
  
  # read number of landmarks from the first file
  # make sure all tem files have the same delineation
  landmarks <- strtoi(readLines(temfiles[1], n = 1))
  
  for (i in 1:length(temfiles)) {
    if (file.exists(temfiles[i])) {
      coords <- read.table(temfiles[i], skip = 1, nrows = landmarks, 
                           stringsAsFactors = FALSE)
      coordinates <- rbind(coordinates, coords)
    } else {
      stop(paste(temfiles[i], "could not be found, import failed"))
    }
  }
  
  coordinates[, 2] <- coordinates[, 2] * -1
  
  # reorganise data to make a 3-dimensional array
  dimensions <- 2
  land <- arrayspecs(coordinates, landmarks, dimensions)
  
  dimnames(land)[[3]] <- as.list(teminfo$id)
  
  # load description of landmarks and semilandmarks
  
  if (curvefile != "") {
    print(paste("Loading curve file:", curvefile))
    curvepts <- as.matrix(read.csv(curvefile, header=T))
  } else if (semi.landmarks > 0) {
    curvepts <- define.sliders(mshape(land), semi.landmarks)
  } else {
    print("No curve file")
    curvepts <- data.frame(before = c(), slide = c(), after = c())
  }
  
  gmm.data <- list(land, curvepts, teminfo)
  names(gmm.data) = c("land", "curvepts", "info")
  return(gmm.data)
}