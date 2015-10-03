#' Rotate aligned data to upright using eye point locations
#' 
#' @param coords A 3D Array of procrustes-aligned coordinates
#' @param left.eye Index of the left eye point (Defaults to 1)
#' @param right.eye Index of the right eye point (Defaults to 2)
#' @return A 3D Array of procrustes-aligned coordinates
#' @examples
#' faces$aligned$coords <- uprightFace(faces$aligned$coords)

uprightFace <- function(coords, left.eye = 1, right.eye = 2) {
  xdif <- abs(coords[left.eye, 1, 1] - coords[right.eye, 1, 1])
  ydif <- abs(coords[left.eye, 2, 1] - coords[right.eye, 2, 1])
  
  if (xdif < ydif) {
    # rotate coordinates 90-degrees
    coords <- coords[, c(2, 1), ]
  }
  
  if (coords[left.eye, 1, 1] < coords[right.eye, 1, 1]) {
    # rotate coordinates 180-degrees
    coords[, 1:2, ] <- coords[, 1:2, ] * -1
  }
  
  return(coords)
}