\name{randomPoints}
\alias{randomPoints}
\title{
Randomly selected points in good habitat.
}
\description{
Generates the coordinates of randomly selected points in good habitat. This can be used to generate starting values for JAGS analysis.
}
\usage{
randomPoints(n, JAGSmask, fixed)
}
\arguments{
  \item{n}{
The number of random points required.
}
  \item{JAGSmask}{
An object of class \code{JAGSmask} which specifies the good habitat.
}
  \item{fixed}{
A 2-column matrix or data frame with fixed locations, which will be included unchanged at the top of the output matrix (if \code{nrows(fixed) < n}). Any rows in \code{fixed} with NAs will be replaced with random values. A warning is given if any locations in \code{fixed} are in bad habitat.
}
}

\value{
An n x 2 matrix with the coordinates of points in suitable habitat on the scale of \code{habMat}.
}
\author{
Mike Meredith
}
\examples{
# Create a JAGSmask object:
data(exampleData)
secrmask <- secr::make.mask(traps, spacing=2000, type='polygon',
              poly=patch, cell.overlap="any")
mymask <- convertMask(secrmask, traps, plot=TRUE)

# Create a few locations in good habitat:
( locs <- randomPoints(3, mymask) )
# Treat these as fixed locations in a set of 10:
randomPoints(10, mymask, fixed=locs)
# Change one of the 'locs' to bad habitat
locs[2, ] <- c(32, 30)
randomPoints(10, mymask, fixed=locs) # Gives a warning.
}


