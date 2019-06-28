\name{convertRaster}
\alias{convertRaster}
\title{
Create a habitat mask from a raster
}
\description{
Create a habitat mask suitable for JAGS from a raster in which non-habitat is indicated by NAs. The raster is converted to a look-up matrix and included in the output.
}
\usage{
convertRaster(raster, traps, plot = TRUE)
}
\arguments{
  \item{raster}{
An object of class \code{RasterLayer}, possibly with a covariate to be used for the analysis, where pixels corresponding to non-habitat have NA values. It must use a projected CRS, not lat/long.
}
  \item{traps}{
A 2-column matrix or data frame with the x and y coordinates of the traps.
}
  \item{plot}{
If TRUE, the resulting mask and trap locations will be plotted.
}
}
\value{
An object of class \code{JAGSmask}, a list with the following elements:
\item{habMat }{A 1/0 matrix, \code{habMat[x, y] = 1} if location (x, y) is in good habitat.}
\item{covMat }{A matrix corresponding to habMat with the values of the covariate in the original raster. NA values in the raster replaced with 0.}
\item{trapMat }{A 2-column matrix with the x and y coordinates of the traps in the same units as \code{habMat}}
\item{upperLimit }{A length 2 vector giving the upper limit of x and y for \code{habMat} in pixel units (the lower limit for each is always 1).}
\item{pixelWidth }{The width and height of the cells in \code{habMat} in the original units (usually metres).}
\item{area }{The area of suitable habitat in the original units, typically square metres.}

It will also have the following attributes, which can be used to convert JAGS output back to the original coordinate system:
\item{boundingbox }{A 4x2 matrix with coordinates of the 4 corners of \code{habMat} in the original units (usually metres).}
\item{origin }{A vector of length 2 giving the location in the original coordinates of point {0, 0} in pixel units.}
}
\author{
Mike Meredith
}

\seealso{
\code{\link{convertMask}}
}
\examples{
require(raster)
data(simSCR)
plot(simSCR$patchR)
points(simSCR$traps, pch=3, col='red')

# Create the 'JAGSmask' object from the raster:
mymask <- with(simSCR, convertRaster(patchR, traps, plot=TRUE))
str(mymask)
}
