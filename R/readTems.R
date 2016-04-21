#' Read Psychomorph template files
#'
#' \code{readTems} reads Psychomorph/WebMorph .tem files from a directory, a list of directories,
#' or a CSV table with the filenames of each template file to be loaded (e.g., "subdir/file1.tem")
#'
#' @param tem.loc Directory or csv table of filenames where .tem files are found
#' @param semi.landmarks Number of curve points to graphically define
#' @param curvefile Name of curvepts file
#' @param analyse Whether to include aligned data and PCA analysis using FRL defaults (Defaults to TRUE)
#' @param method The method to use to choose PCs (Default "broken-stick")
#' @return A data structure with land, curvepts, info and (optionally) aligned and PCA analysed
#' @examples
#' faces <- readTems("~/Dropbox/faces")
#' faces <- readTems("~/Dropbox/faces/faceinfo.csv", curvefile = "~/Dropbox/faces/curveslide.csv"))
#' @export

readTems <- function(tem.loc, semi.landmarks = 0, curvefile = "", analyse = TRUE, method="broken-stick") {
  oldwd <- getwd();
  on.exit(setwd(oldwd), add = TRUE);

  coordinates <- list();

  if (file.info(tem.loc)[1, "isdir"]) {
    # get all tem files in each directory
    message(paste("Getting tem files from directory:", tem.loc));
    setwd(tem.loc);
    temfiles <- dir(pattern = ".tem", recursive = TRUE);
    teminfo <- as.data.frame(list(filename = temfiles));
  } else if (file.exists(tem.loc)) {
    message(paste("Getting tem files from table:", tem.loc));
    teminfo <- read.csv(tem.loc, header=T, stringsAsFactors = FALSE);
    temfiles <- teminfo$filename;
    setwd(dirname(tem.loc));
  } else {
    stop(paste(tem.loc, "is neither a directory or a list of files"));
  }

  if (length(temfiles) == 0) {
    stop("No tem files were found");
  } else {
    message(paste("Tem files found:", length(temfiles)));
  }

  if (is.null(teminfo$id)) {
    teminfo$id <- teminfo$filename;
  }

  dimnames(teminfo)[[1]] <- as.list(teminfo$id);

  # read number of landmarks from the first file
  # make sure all tem files have the same delineation
  landmarks <- strtoi(readLines(temfiles[1], n = 1));

  for (i in 1:length(temfiles)) {
    if (file.exists(temfiles[i])) {
      coords <- read.table(temfiles[i], skip = 1, nrows = landmarks,
                           stringsAsFactors = FALSE);
      coordinates <- rbind(coordinates, coords);
    } else {
      stop(paste(temfiles[i], "could not be found, import failed"));
    }
  }

  # reverse the y-coordinates because psychomorph (0,0) is the upper left corner
  coordinates[, 2] <- coordinates[, 2] * -1;

  # reorganise data to make a 3-dimensional array
  dimensions <- 2;
  land <- geomorph::arrayspecs(coordinates, landmarks, dimensions);

  dimnames(land)[[3]] <- as.list(teminfo$id);

  # load description of landmarks and semilandmarks

  if (curvefile != "") {
    message(paste("Loading curve file:", curvefile));
    curvepts <- as.matrix(read.csv(curvefile, header=T));
  } else if (semi.landmarks > 0) {
    curvepts <- geomorph::define.sliders(geomorph::mshape(land), semi.landmarks);
  } else {
    message("No curve file");
    curvepts <- 0;
  }

  # create mean shape and add to a mean.tem file with the lines
  message("Creating mean shape");
  mean.shape <- geomorph::mshape(land);
  mean.shape[, 2] <- mean.shape[, 2] * -1;
  sample.tem <- readLines(temfiles[1], n = -1L)
  for (i in 1:landmarks) {
    sample.tem[i + 1] <- paste(mean.shape[i, 1], mean.shape[i, 2], sep = "\t");
  }

  mydata <- list(land, curvepts, teminfo, sample.tem);
  names(mydata) = c("land", "curvepts", "info", "mean.tem");

  if (analyse) {
    mydata$aligned <- procrustesAlign(mydata);
    mydata$pca <- getPCA(mydata);
    mydata$data <- getData(mydata, selectPCs(mydata, method=method));
  }

  return(mydata);
}
