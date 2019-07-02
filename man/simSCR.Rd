\name{simSCR}
\alias{simSCR}
\docType{data}
\title{
Simulated Spatial Capture Recapture data for use as example.
}
\description{
A simulated data set for use in the examples.
}
\usage{data("simSCR")}
\format{
  A list with the following components:
\describe{
  \item{patchDF }{a data frame with the x and y coordinates of the vertices of the polygon containing suitable habitat.}
  \item{patchSP }{a SpatialPolygons object describing the habitat patch.}
  \item{patchR }{a RasterLayer with the distance from the edge of the habitat patch.}
  \item{patchRS }{a RasterStack with 2 layers: (1) rand = a random field, (2) edge = the distance from the edge.}
  \item{Arkadia }{a data frame with the x and y coordinates of the vertices of the boundary of Arkadia municipality.}
  \item{trueACs }{a data frame with the x and y coordinates of the true activity centres.}
  \item{traps }{a data frame with the x and y coordinates of the 'proximity' traps; trap identifiers are used as row names. See \code{\link[secr]{traps}} for details.}
  \item{Y}{ Simulated capture histories: an animals x traps matrix with the number of times each animal was caught in each trap out of a total of 90 occasions.}
  \item{JAGSmask}{ A JAGSmask object corresponding to the study.}
  \item{sims.list}{ A list of MCMC iterations returned by JAGS.}
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
