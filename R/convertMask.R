# Convert secr mask and traps objects for use with JAGS
# Output: list with habitat matrix, new traps coords as a matrix,
#   extent, pixel width, and area in original units;
#   attributes: original bounding box as a matrix, false origin, and pixel width.

convertMask <- function(secrmask, secrtraps, plot=TRUE) {

  # Get point spacing, which will be our pixelWidth
  pixWidth <- attr(secrmask, "spacing")
  # Do we need to deal with masks without spacing?
  # min(abs(diff(secrmask$x))) # should be same

  bbox <- as.matrix(attr(secrmask, "boundingbox"))
  # Create 'false origin' so that SW corner of matrix is at [1, 1]
  origin <- bbox[1, ] - pixWidth

  # Get dimensions of the matrix
  nrows <- round((bbox[2, 1] - bbox[1, 1]) / pixWidth)
  ncols <- round((bbox[3, 2] - bbox[2, 2]) / pixWidth)
  habMat <- matrix(0L, nrow=nrows, ncol=ncols)
  # Convert mask x and y to col/row numbers
  dex <- as.matrix(floor(sweep(secrmask, 2, origin) / pixWidth))
  for(i in 1:nrow(dex))
    habMat[dex[i,1], dex[i,2]] <- 1L

  # Convert trap coordinates to the new units:
  newtraps <- sweep(secrtraps, 2, origin) / pixWidth

  out <- list(habMat = habMat, 
              trapMat = as.matrix(newtraps), 
              upperLimit = c(x=nrows+1, y=ncols+1),
              pixelWidth = pixWidth,
              area = sum(habMat) * pixWidth^2)
  attr(out, "boundingbox") <- bbox
  attr(out, "origin") <- origin
  attr(out, "pixelWidth") <- pixWidth
  class(out) <- "JAGSmask"
  
  if(plot)
    plot.JAGSmask(out)
  
  return(out)
}




