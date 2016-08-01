\name{traps}
\alias{traps}
\docType{data}
\title{
An object of class \code{secr::traps}
}
\description{
The location of traps in the \code{patch} polygon, arranged in clusters in a systematic random design.
}
\usage{data("traps")}
\format{
  A traps object is a data frame with the x-y coordinates of the traps; trap identifiers are used as row names. It has a number of attributes describing the trapping method, see \code{\link[secr]{traps}} for details.
}
\source{
Simulated data.
}
\examples{
data(patch)
data(traps)

MASS::eqscplot(patch, type='l')
points(traps, pch=3, col='red')
}
\keyword{datasets}
