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
If TRUE, the resulting mask and trap  locations will be plotted.
}
}
\value{
A list with the following elements:
\item{habMat}{A 1/0 matrix, habMat[x, y] = 1 if location (x, y) is in good habitat.}
\item{trapMat }{A 2-column matrix with the x and y coordinates of the traps in the same units as \code{habMat}}
\item{bbox }{A 4x2 matrix coordinates of the 4 corners of \code{habMat} in the original units (usually metres)}
\item{pixelWidth }{The width and height of the cells in \code{habMat} in the original units (usually metres)}
}
\author{
Mike Meredith
}

\seealso{
\code{\link[secr]{make.mask}}
}
\examples{
data(patch)
data(traps)
# Create the 'secr' mask:
secrmask <- secr::make.mask(traps, spacing=2000, type='polygon', poly=patch)
# Convert to 'habMat' format:
mymask <- convertMask(secrmask, traps, plot=TRUE)
}
