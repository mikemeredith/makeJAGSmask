# Convert JAGS AC location output back to the original
#   coordinate reference system
# Output...

# Helper function for x/y names
pmatch_xy <- function(names) {
  xy <- c(pmatch("x", tolower(names)),
          pmatch("y", tolower(names)))
  if(is.na(diff(xy)) || diff(xy) == 0)
    return(1:2)
  return(xy)
}

convertOutput <- function(ACs, JAGSmask) {
  if(!inherits(JAGSmask, "JAGSmask"))
    stop("'", deparse(substitute(JAGSmask)), "' is not a valid 'JAGSmask' object.")
  classOut <- class(ACs)
  if(is.list(ACs) && length(ACs) == 2) {
    xy <- pmatch_xy(names(ACs))
    x <- ACs[[xy[1]]]
    y <- ACs[[xy[2]]]
  } else if(is.matrix(ACs) && ncol(ACs) == 2) {
    xy <- pmatch_xy(colnames(ACs))
    x <- ACs[, xy[1]]
    y <- ACs[, xy[2]]
  } else if(is.array(ACs) && length(dim(ACs)) == 3 && dim(ACs)[3] == 2) {
    x <- ACs[, , 1]
    y <- ACs[, , 2]
  } else {
    stop("invalid input")
  }
  # Get pixel width and original false origin
  pixWidth <- pixelWidth(JAGSmask)
  origin <- attr(JAGSmask, "origin")

  x1 <- x * pixWidth + origin[1]
  y1 <- y * pixWidth + origin[2]

  out <- switch(classOut,
    matrix = cbind(x = x1, y = y1),
    array = abind(x = x1, y = y1, along=3),
    data.frame = data.frame(x = x1, y = y1),
    list(x = x1, y = y1)
  )
  return(out)
}
  