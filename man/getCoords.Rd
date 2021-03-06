% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getCoords.R
\name{getCoords}
\alias{getCoords}
\title{Get or create a 3D array of coordinates}
\usage{
getCoords(data)
}
\arguments{
\item{data}{Data structure to check, could be from:
\itemize{
  \item Data structure from geomorph or created by readTem()
  \item Data structure with a 3D array named coords
  \item Data structure with a 3D array named aligned$coords
  \item 3D array of coordinate values
}}
}
\value{
A 3D array of coordinate values
}
\description{
\code{getCoords} returns a 3D array of aligned coordinates
from an existing 3D array or a data object with coordinates
or landmarks already specified.
}
\examples{
faces$coords <- getCoords(faces)
}
