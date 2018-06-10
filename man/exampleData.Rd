\name{exampleData}
\alias{exampleData}
\alias{JAGSoutput}
\alias{patch}
\alias{traps}
\docType{data}
\title{
Simulated data for use as example.
}
\description{
The data set includes: \code{patch} - the boundary of an irregular polygon in metres; \code{traps} - the location of traps in the \code{patch} polygon, arranged in clusters in a systematic random design; \code{JAGSoutput} - MCMC chains for locations of ACs for 5 animals captured and 5 "animals" from the augmented data set. For MCMC steps where the animal is not available for capture, the locations are replaced with NA. The locations are in the units used for the habitat mask.
}
\usage{data("exampleData")}
\format{
  The data consist of three objects:
\describe{
  \item{patch }{a data frame with 2 columns giving the x and y coordinates of the vertices of the polygon.}
  \item{traps }{a data frame with the x-y coordinates of the traps; trap identifiers are used as row names. See \code{\link[secr]{traps}} for details.}
  \item{JAGSoutput}{ A list of 2 matrices, each 1350 x 10, with the x and y coordinates of the activity centres.}
}
}
\source{
Simulated data.
}

\examples{
data(exampleData)
MASS::eqscplot(patch, type='l')
points(traps, pch=3, col='red')

## for JAGSoutput, see the examples in convertOutput.
}
\keyword{datasets}
