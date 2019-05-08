\name{getACinCore}
\alias{getACinCore}
\title{
Extract an MCMC chain for number of Activity Centres in the core area
}
\description{
Often we are interested in the number of activity centres in a core area which is smaller than the full extent of the available habitat. This function produces an MCMC chain for the posteror distribution of the number of activity centres in the core.
}
\usage{
getACinCore(S, w, JAGSmask)
}

\arguments{
  \item{S}{
A 3D array of MCMC output, iterations x animals x 2, with the x and y coordinates of the activity centres on the pixels scale. Typically this would be output from JAGS.
}
  \item{w}{
A 1/0 matrix of MCMC output, iterations x animals, with 1 if the animal is in the study area and available for capture, 0 otherwise. Typically this would be output from JAGS.
}
  \item{JAGSmask}{
An object of class JAGSmask which includes information on the core, see \code{\link{addCore}}.
}
}

\value{
Returns an MCMC chain with the posterior distribution for the number of activity centres located inside the core area.
}
\author{
Mike Meredith
}
\examples{
## See the examples for addCore.
}