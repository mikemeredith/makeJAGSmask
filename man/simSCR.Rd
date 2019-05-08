\name{simSCR}
\alias{simSCR}
\docType{data}
\title{
Simulated Spatial Capture Recapture data for use as example.
}
\description{
The data set .
}
\usage{data("simSCR")}
\format{
  A list with the following components:
\describe{
  \item{patchDF }{a data frame with 2 columns giving the x and y coordinates of the vertices of the polygon containing suitable habitat.}
  \item{patchSP }{a SpatialPolygons object describing the habitat patch.}
  \item{patchR }{a RasterLayer with the distance from the boundary of the habitat patch.}
  \item{traps }{a data frame with the x-y coordinates of the 'proximity' traps; trap identifiers are used as row names. See \code{\link[secr]{traps}} for details.}
  \item{Y}{ Simulated capture histories: an animals x traps matrix with the number of times each animal was caught in each trap out of a total of 90 occasions.}
  \item{JAGSmask}{ A JAGSmask object corresponding to the study.}
  \item{sims.list}{ A list of MCMC interations returned by JAGS.}
}
}
\source{
Simulated data.
}

\examples{
data(simSCR)
str(simSCR, 2)

require(sp)
plot(simSCR$patchSP)
points(simSCR$patchDF, col='blue')
points(simSCR$traps, pch=3, col='red')

plot(simSCR$JAGSmask)

## for JAGSoutput, see the examples in convertOutput.
}
\keyword{datasets}
