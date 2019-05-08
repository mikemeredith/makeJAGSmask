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
\code{\link{convertMask}}, \code{\link{plotACs}}.
}
\examples{
data(simSCR)
str(simSCR, 1)
str(simSCR$sims.list)

# The output in simSCR$sims.list$S has MCMC chains for the coordinates
#  of activity centres on the pixel scale. Convert to the original units:
AC <- convertOutput(simSCR$sims.list$S, simSCR$JAGSmask)
str(AC)

# Plot the posterior distribution for 1 animal:
require(sp)
plot(simSCR$patchSP)
points(simSCR$traps, pch=3, col='red')
points(AC[, 1, ], pch=16, cex=0.2, col='blue')

# To plot the ACs for animals not captured, we need to remove the iterations
#  where the animal was not available for capture (ie, was a "phantom").
AC[simSCR$sims.list$w == 0] <- NA
points(AC[, 6, ], pch=16, cex=0.2, col='darkgreen')
}
