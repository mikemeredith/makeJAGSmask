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
A 2-column matrix or data frame with fixed locations, which will be included unchanged in the output matrix. If \code{nrows(fixed) < n}, only the initial rows of the output will be changed.
}
}

\value{
An n x 2 matrix with the coordinates of points in suitable habitat on the scale of \code{habMat}.
}
\author{
Mike Meredith
}


