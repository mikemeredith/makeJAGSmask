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
A 3D array of MCMC output, iterations x animals x 2, with the x and y coordinates of the activity centres. Typically this would be output from \code{\link{convertOutput}}.
}
  \item{traps}{
A 2-column matrix or data frame with the coordinates of the traps.
}
  \item{Y}{
A detection history matrix, animals x traps, with Y[i,j] = 0 if animal i was not caught in trap j. Row names are taken to be animal identifiers.
}
  \item{hab}{
A SpatialPolygons* object describing the extent of the available habitat.
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
\keyword{hplot}
