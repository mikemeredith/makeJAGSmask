\name{patch}
\alias{patch}
\docType{data}
\title{
A polygon boundary
}
\description{
The boundary of an irregular polygon in metres.
}
\usage{data("patch")}
\format{
  A data frame with 2 columns giving the x and y coordinates of the vertices of the polygon.
}
\examples{
data(patch)
MASS::eqscplot(patch, type='l')

# See convertMask for more.
}
\keyword{datasets}
