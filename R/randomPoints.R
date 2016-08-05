# Generate the coordinates of randomly selected points in good habitat.

# This is intended to provide starting values. The starting locations for animals
#   caught can be fixed.

randomPoints <- function(n, JAGSmask, fixed) {

  # generate 2 x n random locations:
  xx <- runif(2*n, 1, JAGSmask$upperLimit[1])
  yy <- runif(2*n, 1, JAGSmask$upperLimit[2])
  # check habitat
  ok <- JAGSmask$habMat[cbind(xx, yy)] == 1
  # reject locations in bad habitat
  xx <- xx[ok]
  yy <- yy[ok]
  # if not enough values, do more
  while(length(xx) < n) {
    x1 <- runif(n, 1, JAGSmask$upperLimit[1])
    y1 <- runif(n, 1, JAGSmask$upperLimit[2])
    ok <- JAGSmask$habMat[cbind(x1, y1)] == 1
    xx <- c(xx, x1[ok])
    yy <- c(yy, y1[ok])
  }
  # Truncate to length n
  xx <- xx[1:n]
  yy <- yy[1:n]
  # Insert fixed values, ignoring NAs
  if(!missing(fixed)) {
    fix <- which(!is.na(rowSums(fixed)))
    xx[fix] <- fixed[fix, 1]
    yy[fix] <- fixed[fix, 2]
  }
  return(cbind(x=xx, y=yy))
}
