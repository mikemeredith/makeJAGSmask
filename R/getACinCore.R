
# Get the number of activity centres in the core

# Returns an MCMC chain with posterior probability of number of ACs in the core.

getACinCore <- function(S,          # iters x animals x 2 array with AC locations on the pixel scale
                        w,          # iters x animals 1/0 matrix, 1 if the animal is real
                        JAGSmask){  # the JAGSmask used for the analysis with a component coreMat

  S[w==0] <- NA
  inCore <- array(NA, dim(S)[1:2])
  for(j in 1:ncol(inCore))
    inCore[, j] <- JAGSmask$coreMat[S[, j, ]]
  return(rowSums(inCore, na.rm=TRUE))
}

