#' Get or create a 3D array of coordinates
#'
#' \code{getCoords} returns a 3D array of aligned coordinates
#' from an existing 3D array or a data object with coordinates
#' or landmarks already specified.
#'
#' @param data Data structure to check, could be from:
#' \itemize{
#'   \item Data structure from geomorph or created by readTem()
#'   \item Data structure with a 3D array named coords
#'   \item Data structure with a 3D array named aligned$coords
#'   \item 3D array of coordinate values
#' }
#' @return A 3D array of coordinate values
#' @examples
#' faces$coords <- getCoords(faces)
#' @export

getCoords <- function(data) {

  check3D <- function(obj) {
    dimensions <- dim(obj)
    is.3D <- (length(dimensions) == 3 && dimensions[2] == 2)
    return(is.3D)
  }


  if (check3D(data)) {
    # data is probably already a 3D array of coordinates
    return(data)
  } else if(!is.null(data$coords) && check3D(data$coords)) {
    # data has a 3D array named coords
    return(data$coords)
  } else if(!is.null(data$aligned$coords) && check3D(data$aligned$coords)) {
    # data had a 3D array in aligned$coords
    return(data$aligned$coords)
  } else if (!is.null(data$land)) {
    # data only has landmarks, create aligned coords
    aligned <- procrustesAlign(data)
    message("Coordinates created with procrustesAlign()")
    return(aligned$coords)
  } else {
    warning("No coordinates found")
    return(FALSE)
  }
}
