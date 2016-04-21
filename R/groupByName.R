#' Allocate images to groups based on filenames and regex patterns
#'
#' @param img.list A list of filenames
#' @param names A list of the groups to be allocated
#' @param patterns A list of regex patterns to match for group allocation
#' @return A list of group names for each filename in img.list
#' @examples
#' faces$info$group <- groupByName(faces$info$filename,
#'                                 names=c("male","female"),
#'                                 patterns=c("^male/*","^female/*"))
#' @export

groupByName <- function(img.list, names, patterns) {
  img.n <- length(img.list)
  grp.n <- length(names)

  group.list <- character(img.n)
  for (i in 1:img.n) {
    for (g in 1:grp.n) {
      ingroup <- grep(patterns[g], img.list[i])
      if (length(ingroup) == 1) {
        group.list[i] = names[g]
      }
    }
  }
  return(group.list)
}
