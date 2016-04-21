#' Save coordinates to a file
#'
#' @param coords An array of projected coordinates
#' @param save.name A file name to save to (Defaults to ""; no saving)
#' @param sample.tem An optional array or filename for a sample tem file from which to get lines
#' @param align.to An array of coordinates numbers to align the tem to (taken from the sample tem)
#' @param data An frlgmm data object to align to
#' @examples
#' saveTem(coords, save.name="sd3max.tem", sample.tem=faces$info$filename[1]))
#' @export

saveTem <- function(coords, save.name = "", sample.tem = "", align.to = c(1,2), data = NULL) {
  pt.n <- dim(coords)[1];
  tem <- list();
  coords[, 2] <- coords[, 2] * -1;

  if (!is.null(data) && !is.null(data$mean.tem)) {
    sample.tem <- data$mean.tem;
    landmarks <- as.numeric(sample.tem[1]);
    if (landmarks == pt.n) {
      message(paste("Sample tem matches point number:", pt.n));
      tem <- sample.tem;
    } else {
      warning(paste("Sample tem (n =", landmarks, ") does not match point number:", pt.n));
    }
  } else if (sample.tem != "" && file.exists(sample.tem)) {
    message(paste("Loading sample tem from file:", sample.tem));
    landmarks <- strtoi(readLines(sample.tem, n = 1));
    if (landmarks == pt.n) {
      message(paste("Sample tem matches point number:", pt.n));
      tem <- readLines(sample.tem, n = -1L);
    } else {
      warning(paste("Sample tem (n =", landmarks, ") does not match point number:", pt.n));
    }
  } else {
    warning(paste(sample.tem, "was not found"));
  }

  tem[1] <- pt.n;

  # align image
  if (!is.null(data) && !is.null(data$aligned)) {
    # resize.factor <- mean(faces$aligned$Csize);
    resize.factor <- 3818.326;

    shift.x <- data$aligned$shift$x;
    shift.y <- data$aligned$shift$y;
  } else {
    message(paste("Aligning image to points", align.to[1], "and", align.to[2]));

    temsplit <- function(x) { return(c(as.numeric(x[[1]]), as.numeric(x[[2]]))); }
    pt1 <- sapply(strsplit(tem[align.to[1]+1], split = "\t"), temsplit);
    pt2 <- sapply(strsplit(tem[align.to[2]+1], split = "\t"), temsplit);

    orig.pt1.x <- pt1[1];
    orig.pt2.x <- pt2[1];
    orig.pt1.y <- pt1[2];
    orig.pt2.y <- pt2[2];
    orig.size <- sqrt( (orig.pt1.x - orig.pt2.x)^2 + (orig.pt1.y - orig.pt2.y)^2 );

    aligned.mean <- geomorph::mshape(coords);
    aligned.pt1.x <- aligned.mean[align.to[1],1];
    aligned.pt2.x <- aligned.mean[align.to[2],1];
    aligned.pt1.y <- aligned.mean[align.to[1],2];
    aligned.pt2.y <- aligned.mean[align.to[2],2];
    aligned.size <- sqrt( (aligned.pt1.x - aligned.pt2.x)^2 + (aligned.pt1.y - aligned.pt2.y)^2 );

    resize.factor <- orig.size / aligned.size;

    shift.x <- orig.pt1.x - (aligned.pt1.x * resize.factor)
    shift.y <- orig.pt1.y - (aligned.pt1.y * resize.factor)
  }

  message(paste("Adding", pt.n, "point coordinates"))
  for (i in 1:pt.n) {
    new.x <- (coords[i, 1] * resize.factor) + shift.x;
    new.y <- (coords[i, 2] * resize.factor) + shift.y;
    tem[i + 1] <- paste(new.x, new.y, sep = "\t");
  }

  if (save.name != "") {
    message(paste("Saving tem to file:", save.name))
    write(tem, file = save.name)
  }
}
