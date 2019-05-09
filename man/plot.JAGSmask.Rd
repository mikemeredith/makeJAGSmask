\name{plot.JAGSmask}
\alias{plot.JAGSmask}
\title{
A plotting method for class \code{JAGSmask}
}
\description{
A plotting method for class \code{JAGSmask}
}
\usage{
\method{plot}{JAGSmask}(x, col, verify = TRUE, \dots)
}

\arguments{
  \item{x}{
An object of class \code{JAGSmask} as produced by \code{convertMask}.
}
  \item{col}{
A vector of colours for habitat classes. Note that traps are plotted in red.
}
  \item{verify}{
If TRUE, the function will check that traps are inside the area of suitable habitat defined by the habitat matrix.
}
 \item{\dots}{
Other graphical parameters to pass to \code{image.default}.
}
}

\value{
Returns nothing.
}
\author{
Mike Meredith
}
\examples{
data(simSCR)
plot(simSCR$JAGSmask)  # habitat yellow, non-habitat grey.
# add a core
Arkadia <- cbind(x=c(140000, 140000, 200000, 200000),
  y=c(50000,805000, 805000, 50000))
Amask <- addCore(simSCR$JAGSmask, type="poly", poly=Arkadia)
plot(Amask)  # core yellow, non-core habitat white.

# Create a mask from a raster
require(raster)
Rmask <- with(simSCR, convertRaster(patchR, traps, plot=TRUE)) # non-habitat black
ARmask <- addCore(Rmask, type="poly", poly=Arkadia) # non-core grey
}
\keyword{hplot}
