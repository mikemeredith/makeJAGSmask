\name{convertMask}
\alias{convertMask}
\title{
Habitat mask conversion
}
\description{
Convert a habitat mask produced by \pkg{secr} to the form needed for JAGS analysis.
}
\usage{
convertMask(secrmask, secrtraps, plot = TRUE)
}
\arguments{
  \item{secrmask}{
An object of class \code{secr::mask} as produced by the function \code{secr::make.mask}.
}
  \item{secrtraps}{
An object of class \code{secr::traps} as produced by the function \code{secr::make.traps}.
}
  \item{plot}{
If TRUE, the resulting mask and trap locations will be plotted.
}
}
\value{
An object of class \code{JAGSmask}, a list with the following elements:
\item{habMat }{A 1/0 matrix, \code{habMat[x, y] = 1} if location (x, y) is in good habitat.}
\item{trapMat }{A 2-column matrix with the x and y coordinates of the traps in the same units as \code{habMat}}
\item{upperLimit }{A length 2 vector giving the upper limit of x and y for \code{habMat} in pixel units (the lower limit for each is always 1).}
\item{pixelWidth }{The width and height of the cells in \code{habMat} in the original units (usually metres).}
\item{area }{The area of suitable habitat in the original units, typically square metres.}

It will also have the following attributes, which can be used to convert JAGS output back to the original coordinate system:
\item{boundingbox }{A 4x2 matrix with coordinates of the 4 corners of \code{habMat} in the original units (usually metres).}
\item{origin }{A vector of length 2 giving the location in the original coordinates of point {0, 0} in pixel units.}
\item{pixelWidth }{The width and height of the cells in \code{habMat} in the original units (usually metres).}

The \code{pixelWidth} attribute will be removed in future versions.
}
\author{
Mike Meredith
}

\seealso{
\code{\link[secr]{make.mask}}
}
\examples{
require(sp)
data(simSCR)
plot(simSCR$patchSP)
points(simSCR$traps, pch=3, col='red')

# Create the 'secr' mask:
require("secr")
secrmask <- make.mask(simSCR$traps, spacing=1000,
  type='polygon', poly=simSCR$patchSP)
plot(secrmask)

# Convert to 'JAGSmask' format:
mymask <- convertMask(secrmask, simSCR$traps, plot=TRUE)
}
