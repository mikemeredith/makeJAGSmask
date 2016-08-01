# Convert secr mask and traps objects for use with JAGS
# Output: list with habitat matrix, new traps coords as a matrix,
#   original bounding box as a matrix, and pixel width.

convertMask <- function(secrmask, secrtraps, plot=TRUE) {

  # Get point spacing, which will be our pixelWidth
  pixWidth <- attr(secrmask, "spacing")
  # Do we need to deal with masks without spacing?
  # min(abs(diff(secrmask$x))) # should be same

  bbox <- as.matrix(attr(secrmask, "boundingbox"))
  # Create 'false origin' so that SW corner of matrix is at [1, 1]
  origin <- bbox[1, ] - pixWidth

  # Get dimensions of the matrix
  nrows <- (bbox[2, 1] - bbox[1, 1]) / pixWidth
  ncols <- (bbox[3, 2] - bbox[2, 2]) / pixWidth
  # Convert mask x and y to col/row numbers
  dex <- as.matrix(floor(sweep(secrmask, 2, origin) / pixWidth))
  habMat <- matrix(0L, nrow=nrows, ncol=ncols)
  for(i in 1:nrow(dex))
    habMat[dex[i,1], dex[i,2]] <- 1L

  # Convert trap coordinates to the new units:
  newtraps <- sweep(secrtraps, 2, origin) / pixWidth
  # Check locations of traps
  trapcells <- floor(newtraps)
  ok <- numeric(nrow(trapcells))
  for(i in 1:nrow(trapcells))
    ok[i] <- habMat[trapcells[i,1], trapcells[i,2]]
  if(!all(ok == 1)) {
    cat("The following traps appear to be in bad habitat:", which(!ok), "\n")
    cat("They are circled in the plot.\n")
    cat("If on the edge, this is probably due to rasterization of the habitat polygon.\n")
  }

  if(plot) {
    MASS::eqscplot(1,1, xlim=c(1,ncols+1), ylim=c(1,nrows+1), type='n')
    rect(1,1, nrows+1, ncols+1, col='grey95')
    # Plot pixel centres, centre is 0.5 units greater than SW corner
    rdex <- row(habMat)
    cdex <- col(habMat)
    points(x=rdex+0.5, y=cdex+0.5, col=habMat+1)
    points(newtraps, pch=3, col='red', xpd=TRUE)
    if(!all(ok == 1))
      points(newtraps[!ok, ], col='red', cex=2, xpd=TRUE)
  }

  return(list(habMat=habMat, 
              trapMat=as.matrix(newtraps), 
              bbox = bbox,
              pixelWidth = pixWidth))
}




