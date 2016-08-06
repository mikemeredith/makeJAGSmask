# Convert JAGS AC location output back to the original
#   coordinate reference system
# Output...

convertOutput <- function(ACs, JAGSmask) {
  classOut <- class(ACs)
  if(is.list(ACs) && length(ACs) == 2) {
    x <- ACs[[1]]
    y <- ACs[[2]]
  } else if(is.matrix(ACs) && ncol(ACs) == 2) {
    x <- ACs[, 1]
    y <- ACs[, 2]
  } else if(is.array(ACs) && length(dim(ACs)) == 3 && dim(ACs)[3] == 2) {
    x <- ACs[, , 1]
    y <- ACs[, , 2]
  } else {
    stop("invalid input")
  }
  # Get pixel width and original false origin
  pixWidth <- attr(JAGSmask, "pixelWidth")
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
  