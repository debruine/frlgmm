#' Save coordinates to a file
#' 
#' @param coords An array of projected coordinates
#' @param save.name A file name to save to (Defaults to ""; no saving)
#' @param sample.tem An optional filename for a sample tem file from wich to get lines
#' @param align.to An array of coordinates to align the tem to (usually the mean of the original set)
#' @return true if file saved successfully
#' @examples
#' saveTem(coords, save.name="sd3max.tem", sample.tem=faces$info$filename[1]))

saveTem <- function(coords, save.name = "", sample.tem = "", align.to = NULL) {
  pt.n <- dim(coords)[1]
  
  if (sample.tem != "") {
    if (file.exists(sample.tem)) {
      message(paste("Loading sample tem:", sample.tem))
      landmarks <- strtoi(readLines(sample.tem, n = 1))
      if (landmarks == pt.n) {
        message(paste("Sample tem matches point number:", pt.n))
        tem <- readLines(sample.tem, n = -1L)
      } else {
        warning(paste("Sample tem (n =", landmarks, ") does not match point number:", pt.n))
      }
    } else {
      warning(paste(sample.tem, "was not found"))
    }
  } else {
    tem <- list()
  }
  tem[1] <- pt.n
  
  message(paste("Adding", pt.n, "point coordinates"))
  for (i in 1:pt.n) {
    tem[i + 1] <- paste(coords[i, 1], coords[i, 2], sep = "\t")
  }
  
  if (save.name != "") {
    message(paste("Saving tem to file:", save.name))
    write(tem, file = save.name)
    return(TRUE)
  } else {
    return(FALSE)
  }
}