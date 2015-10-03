#' Procrustes align frlgmm object with FRL defaults.
#' 
#' @param data Data structure from geomorph or created by readTem()
#' @return 3D Array of procrustes-aligned coordinates
#' @examples
#' faces$aligned <- procrustesAlign(faces)

procrustesAlign <- function(data) {
  # procrustes align landmarks
  # do not rotate to principal axes (usually turns faces sideways)
  aligned.data <- gpagen(data$land, 
                         curves = data$curvepts, 
                         PrinAxes = FALSE, 
                         pointscale = .2, 
                         meansize = 1)
  return(aligned.data)
}