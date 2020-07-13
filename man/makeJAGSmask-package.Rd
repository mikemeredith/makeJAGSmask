\name{makeJAGSmask-package}
\alias{makeJAGSmask}
\docType{package}
\title{
Habitat matrices for use with SECR analysis in JAGS or BUGS}
\description{
Functions to create habitat masks for spatially explicit capture-recapture (SECR) analysis in BUGS or JAGS. Currently this is done by converting a mask prepared by the \pkg{secr} package.
}

\details{
The original code for spatially explicit capture-recapture (SECR) analysis in BUGS or JAGS worked with a rectangular state space assumed to be uniformly good habitat. The JAGS or BUGS model can be modified to allow for part of the rectangle to be designated as unsuitable habitat; proposed activity centre (AC) locations are checked against a matrix to determine if they lie in good or bad habitat, and locations in bad habitat are rejected.

This look-up procedure is repeated for every animal in the augmented data set and for every iteration of the MCMC chain; so if you have 200 animals in the augmented data set and run 100,000 interations, that means 20 million look-ups. It makes sense to minimize the code needed for the look-up and do the complicated stuff before or after the MCMC run.

The JAGS code is written so that truncating the x and y coordinates of the proposed AC produces the indices into the habitat matrix. This matrix has to be configured so that \code{habMat[x, y]} returns 1 if the AC is in suitable habitat, 0 otherwise.

The matrix needed for JAGS is constructed by the functions \code{\link{convertRaster}} or \code{\link{convertMask}} starting from a raster or a mask produced by the function \code{secr::make.mask}.

The estimate of \code{sigma} is in units of pixel width. This can be extracted from the mask object with the function \code{pixelWidth}.

AC locations in the MCMC output from JAGS will also be in the units used for the habitat matrix, ie, pixel width, with origin at (1, 1); the function \code{\link{convertOutput}} converts these back to the original coordinate system.
}

\author{
Mike Meredith

Maintainer: Mike Meredith <mike@mmeredith.net>
}

% Put in an extended example here once we have some example data

\keyword{ package }
