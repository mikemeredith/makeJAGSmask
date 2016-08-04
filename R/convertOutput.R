# Convert JAGS AC location output back to the original
#   coordinate reference system
# Output...

convertOutput <- function(x, y, JAGSmask) {
  # Deal with x and y
  if(is.list(x) && length(x) == 2) {
    y <- x[[2]]
    x <- x[[1]]
  }
  # Get pixel width and original false origin
  pixWidth <- attr(JAGSmask, "pixelWidth")
  origin <- attr(JAGSmask, "origin")

  east <- as.matrix(x) * pixWidth + origin[1]
  north <- as.matrix(y) * pixWidth + origin[2]

  out <- list(east=east, north=north)
  return(out)
}
  
