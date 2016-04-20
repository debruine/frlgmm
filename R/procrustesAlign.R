#' Procrustes align frlgmm object with FRL defaults.
#'
#' @param data Data structure from geomorph or created by readTem()
#' @return 3D Array of procrustes-aligned coordinates
#' @examples
#' faces$aligned <- procrustesAlign(faces)

procrustesAlign <- function(data) {
  message("Procrustes aligning landmarks");
  if (data$curvepts[1] == 0) {
    aligned.data <- geomorph::gpagen(data$land,
                                     Proj = TRUE,              # project data into tangent space (geomorph default)
                                     ProcD = FALSE,            # use minimising bending energy to slide semilandmarks (not geomorph default)
                                     PrinAxes = FALSE);        # do not rotate output to principal axes (usually turns heads sideways)
  } else {
    aligned.data <- geomorph::gpagen(data$land,
                                   curves = data$curvepts,
                                   Proj = TRUE,              # project data into tangent space (geomorph default)
                                   ProcD = FALSE,            # use minimising bending energy to slide semilandmarks (not geomorph default)
                                   PrinAxes = FALSE);        # do not rotate output to principal axes (usually turns heads sideways)
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
