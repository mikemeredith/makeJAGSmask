\name{plot.JAGSmask}
\alias{plot.JAGSmask}
\title{
A plotting method for class \code{JAGSmask}
}
\description{
A plotting method for class \code{JAGSmask}
}
\usage{
\method{plot}{JAGSmask}(x, colors = c("grey", "white", "yellow"),
                 verify = TRUE, \dots)
}

\arguments{
  \item{x}{
An object of class \code{JAGSmask} as produced by \code{convertMask}.
}
  \item{colors}{
A vector of colours for unsuitable habitat, non-core habitat, and core habitat; if no core raster is present, the core colour will be used for all the habitat. Note that traps are plotted in red.
}
  \item{verify}{
If TRUE, the function will check that traps are inside the area of suitable habitat defined by the habitat matrix.
}
 \item{\dots}{
Other graphical parameters to pass to \code{image.default}.
}
}

\value{
Returns nothing.
}
\author{
Mike Meredith
}
\keyword{hplot}
