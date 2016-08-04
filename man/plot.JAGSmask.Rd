\name{plot.JAGSmask}
\alias{plot.JAGSmask}
\title{
A plotting method for class \code{JAGSmask}
}
\description{
A plotting method for class \code{JAGSmask}
}
\usage{
\method{plot}{JAGSmask}(x, colors = c("black", "skyblue", "red"), points = c(1, 1, 3), verify = TRUE, \dots)
}

\arguments{
  \item{x}{
An object of class \code{JAGSmask} as produced by \code{convertMask}.
}
  \item{colors}{
A vector of colours for unsuitable habitat, suitable habitat, and traps.
}
  \item{points}{
A vector of symbols for unsuitable habitat, suitable habitat, and traps; see \code{\link{points}} for details.
}
  \item{verify}{
If TRUE, the function will check that traps are inside the area of suitable habitat defined by the habitat matrix.
}
 \item{\dots}{
Other graphical parameters to pass to \code{plot.default}.
}
}

\value{
Returns nothing.
}
\author{
Mike Meredith
}
\keyword{hplot}
