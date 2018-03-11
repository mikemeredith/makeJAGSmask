# Generate the coordinates of randomly selected points in good habitat.

# This is intended to provide starting values. The starting locations for animals
#   caught can be fixed.

randomPoints <- function(n, JAGSmask, fixed) {

  # generate 2 x n random locations:
  XY <- cbind(x=runif(2*n, 1, JAGSmask$upperLimit[1]),
              y=runif(2*n, 1, JAGSmask$upperLimit[2]))
  # check habitat
  ok <- JAGSmask$habMat[XY] == 1
  # reject locations in bad habitat
  XY <- XY[ok, , drop=FALSE]
  # if not enough values, do more
  while(nrow(XY) < n) {
    more <- cbind(runif(n, 1, JAGSmask$upperLimit[1]),
                  runif(n, 1, JAGSmask$upperLimit[2]))
    ok <- JAGSmask$habMat[more] == 1
    XY <- rbind(XY, more[ok, , drop=FALSE])
  }
  # Truncate to length n
  XY <- XY[1:n, ]
  # Insert fixed values, ignoring NAs
  if(!missing(fixed)) {
    fix <- which(!is.na(rowSums(fixed)))
    fixed0 <- fixed[fix, , drop=FALSE]
    if(any(fixed0 < 1) ||
        any(fixed0[, 1] > JAGSmask$upperLimit[1]) ||
        any(fixed0[, 2] > JAGSmask$upperLimit[2]))
      stop("At least 1 fixed location is outside the habitat mask.")
    ok <- JAGSmask$habMat[fixed0] == 1
    if(any(!ok))
      warning("Fixed locations ", which(!ok), " are in bad habitat.")
    XY[fix, ] <- fixed[fix, ]
  }
  return(XY)
}
