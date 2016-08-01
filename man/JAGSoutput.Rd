\name{JAGSoutput}
\alias{JAGSoutput}
\docType{data}
\title{
JAGS output for Activity Centre locations for simulated data.
}
\description{
MCMC chains for locations of ACs for 5 animals captured and 5 "animals" from the augmented data set. For MCMC steps where the animal is not available for capture, the locations are replaced with NA. The locations are in the units used for the habitat mask.
}
\usage{data("JAGSoutput")}
\format{
 A list of 2 matrices
 $ ACx: a 1350 x 10 matrix with the x coordinates of the ACs.
 $ ACy: a matching matrix with the y coordinates.
}
\source{
Simulated data.
}

\examples{

## see the examples in convertOutput.
}
\keyword{datasets}
