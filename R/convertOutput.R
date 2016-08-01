# Convert JAGS AC location output back to the original
#   coordinate reference system
# Output...

convertOutput <- function(x, y, convertMask) {

  # Get pixel width and original false origin
  pixWidth <- convertMask$pixelWidth
  origin <- convertMask$bbox[1, ] - pixWidth

  east <- as.matrix(x) * pixWidth + origin[1]
  north <- as.matrix(y) * pixWidth + origin[2]

  out <- list(east=east, north=north)
  return(out)
}
  
