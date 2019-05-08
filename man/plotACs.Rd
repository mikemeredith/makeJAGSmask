\name{plotACs}
\alias{plotACs}
\title{
Plot activity centres from Bayesian SCR output
}
\description{
Plot activity centres from Bayesian SCR output
}
\usage{
plotACs(which=NA, ACs, traps, Y, hab, howMany=3000, show.labels=TRUE)
}

\arguments{
  \item{which}{
The numbers of the ACs to plot; if NA, all captured animals' ACs will be plotted.
}
  \item{ACs}{
A 3D array of MCMC output, iterations x animals x 2, with the x and y coordinates of the activity centres. Typically this would be output from \code{\link{convertOutput}}. ACs for phantom animals should be converted to NA.
}
  \item{traps}{
An optional 2-column matrix or data frame with the coordinates of the traps.
}
  \item{Y}{
An optional detection history matrix, animals x traps, with Y[i,j] > 0 if animal i was caught in trap j, 0 otherwise. Row names are taken to be animal identifiers.
}
  \item{hab}{
Optional; either a SpatialPolygons* object or a 2-column matrix or data frame with the coordinates of the vertices of a polygon describing the extent of the available habitat.
}
 \item{howMany}{
The number of points to plot for each posterior distribution.
}
 \item{show.labels}{
If TRUE, animal identifiers are added to the plot.
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
str(simSCR$sims.list)

# Convert AC locations to the original units, remove phantoms:
AC <- convertOutput(simSCR$sims.list$S, simSCR$JAGSmask)
AC[simSCR$sims.list$w == 0] <- NA

# Default: plot the animals captured:
with(simSCR, plotACs(which=NA, AC=AC, traps=traps, Y=Y, hab=patchSP))

# Omit optional items, some information will not be displayed:
with(simSCR, plotACs(which=1:6, AC=AC))
with(simSCR, plotACs(which=1:6, AC=AC, traps=traps, hab=patchSP))
with(simSCR, plotACs(which=NA, AC=AC, Y=Y, hab=patchSP))
}
\keyword{hplot}