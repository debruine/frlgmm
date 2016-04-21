#' Procrustes align frlgmm object.
#'
#' Uses geomorph::gpagen with FRL defaults.
#' Proj = TRUE: project template landmark data into tangent space.
#' ProcD = FALSE: Does not rotate output to principal axes, as this usually turns heads sideways.
#' PrinAxes = FALSE: Uses minimising bending energy to slide semilandmarks if curvepts defined.
#'
#' @param data Data structure from geomorph or created by readTem()
#' @return 3D Array of procrustes-aligned coordinates
#' @examples
#' faces$aligned <- procrustesAlign(faces)
#' @export

procrustesAlign <- function(data) {
  message("Procrustes aligning landmarks");
  if (data$curvepts[1] == 0) {
    aligned.data <- geomorph::gpagen(data$land,
                                     Proj = TRUE,
                                     PrinAxes = FALSE);
  } else {
    aligned.data <- geomorph::gpagen(data$land,
                                     curves = data$curvepts,
                                     Proj = TRUE,
                                     ProcD = FALSE,
                                     PrinAxes = FALSE);
  }

  # calculate scale and translate for later resizing to psychomorph templates
  mean.orig   <- geomorph::mshape(data$land);
  resize.factor <- mean(aligned.data$Csize);

  shift.x <- mean(mean.orig[,1]);
  shift.y <- -1 * mean(mean.orig[,2]);

  aligned.data$shift <- list(shift.x, shift.y);
  names(aligned.data$shift) = c("x", "y");

  return(aligned.data)
}
