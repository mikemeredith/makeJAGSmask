
# Get the number of activity centres in the core

# Returns an MCMC chain with posterior probability of number of ACs in the core.

getACinCore <- function(S,          # iters x animals x 2 array with AC locations on the pixel scale
                        w,          # iters x animals 1/0 matrix, 1 if the animal is real
                        JAGSmask){  # the JAGSmask used for the analysis with a component coreMat
  if(!inherits(JAGSmask, "JAGSmask"))
    stop(deparse(substitute(JAGSmask)), " is not a valid JAGSmask object.", call.=FALSE)
  if(is.null(JAGSmask$coreMat))
    stop(deparse(substitute(JAGSmask)), " does not contain core information.", call.=FALSE)
  if(length(dim(S)) != 3 || dim(S)[3] != 2)
    stop(deparse(substitute(JAGSmask)), " is not a proper activity centre array.", call.=FALSE)
  dimMat <- dim(JAGSmask$coreMat)
  if(max(S[,,1], na.rm=TRUE) > dimMat[1] + 1 ||
      min(S[,,1], na.rm=TRUE) < 1 ||
      max(S[,,2], na.rm=TRUE) > dimMat[2] + 1 ||
      min(S[,,2], na.rm=TRUE) < 1)
  stop(deparse(substitute(S)), " has values outside the range of the matrix. Are these on the pixel scale?", call.=FALSE)
      
  if(!missing(w))
    S[w==0] <- NA
  inCore <- array(NA, dim(S)[1:2])
  for(j in 1:ncol(inCore))
    inCore[, j] <- JAGSmask$coreMat[S[, j, ]]
  return(rowSums(inCore, na.rm=TRUE))
}

