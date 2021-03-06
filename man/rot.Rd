% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting.R
\name{rot}
\alias{rot}
\title{Rotate the factor plot}
\usage{
rot(x, y, flip)
}
\arguments{
\item{x}{First factor coordinates}

\item{y}{Second factor coordinates}

\item{flip}{Degrees of rotation}
}
\value{
A two-column matrix with the rotated coordinates
}
\description{
Takes the coordinates of calculated factors and rotates them by \code{flip} degrees. This is useful for
aligning the plot in the usual left-right direction.
}
