\name{convertOutput}
\alias{convertOutput}
\title{
Convert JAGS output back to the original coordinates}
\description{
The activity centre locations output from a JAGS run using a habitat mask is in units of pixels. This function converts them to the original coordinate system.
}
\usage{
convertOutput(x, y, JAGSmask)
}
\arguments{
  \item{x}{
A matrix or data frame with the MCMC output for x coordinates of the ACs, with a column for each animal and a row for each MCMC iteration. It can also be a list with 2 elements, the first specifying the x coordinates and the second the y coordinates; in this case argument y is ignored.
}
  \item{y}{
A corresponding matrix or data frame with the MCMC output for the y coordinates. Ignored if x is a list.
}
  \item{JAGSmask}{
The object of class \code{JAGSmask} that was used as input to the JAGS run.
}
}
\value{
A list with two matrices:
\item{east }{The Easting of the AC location in the original coordinate system, with the same configuration as the \code{x} argument.}
\item{north }{The corresponding Northings.}
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
points(myresult$east[, 1], myresult$north[, 1], col=2)
points(myresult$east[, 6:10], myresult$north[, 6:10], col=3)
points(traps, pch=3, col='red')
}
