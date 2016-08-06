\name{convertOutput}
\alias{convertOutput}
\title{
Convert JAGS output back to the original coordinates
}
\description{
The activity centre locations output from a JAGS run using a habitat mask is in units of pixels. This function converts them to the original coordinate system.
}
\usage{
convertOutput(ACs, JAGSmask)
}
\arguments{
  \item{ACs}{
A 2-column matrix or data frame, or an array of dimensions iterations x animals x 2, with the MCMC output for x and y coordinates of the ACs. It can also be a list with 2 elements, the first specifying the x coordinates and the second the y coordinates.
}
  \item{JAGSmask}{
The object of class \code{JAGSmask} that was used as input to the JAGS run.
}
}
\value{
An object of the same class as \code{ACs} with the x and y coordinates of the ACs in the original coordinate system.
}
\author{
Mike Meredith
}

\seealso{
\code{\link{convertMask}}
}
\examples{
# Generate the mask used for the analysis:
data(patch)
data(traps)
secrmask <- secr::make.mask(traps, spacing=2000, type='polygon', poly=patch)
mymask <- convertMask(secrmask, traps, plot=TRUE)

# The analysis in JAGS is slow, so just look at the output:
data(JAGSoutput)
str(JAGSoutput)
myresult <- convertOutput(JAGSoutput, JAGSmask = mymask)

# Plot one animal that was captured and 5 that were not:
MASS::eqscplot(patch, type='l')
points(myresult[, 1, ], col=2)
points(myresult[, 6:10, 1], myresult[, 6:10, 2], col='grey')
points(traps, pch=3, col='red')
}
