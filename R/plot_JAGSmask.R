# Function to plot a JAGSmask object

plot.JAGSmask <- function(x, colors=c("black", "skyblue", "red"),
  points=c(1, 1, 3), verify=TRUE, ...) {
  habMat <- x$habMat
  trapMat <- x$trapMat
  bbox <- attr(x, "boundingbox")
  ncols <- ncol(habMat)
  nrows <- nrow(habMat)
  MASS::eqscplot(1,1, xlim=c(1,nrows+1), ylim=c(1,ncols+1), type='n',
    bty='n', xaxt='n', yaxt='n', xlab="Easting", ylab="Northing")
  axis(1, at=c(1, nrows+1), labels=bbox[1:2, 1])
  axis(2, at=c(1, ncols+1), labels=bbox[2:3, 2])
  rect(1,1, nrows+1, ncols+1, col='grey95')
  # Plot pixel centres, centre is 0.5 units greater than SW corner
  rdex <- row(habMat)
  cdex <- col(habMat)
  points(x=rdex+0.5, y=cdex+0.5, pch=points[habMat+1], col=colors[habMat+1])
  points(trapMat, pch=points[3], col=colors[3], xpd=TRUE)
  
  if(verify) {
    # Check locations of traps
    trapcells <- floor(trapMat)
    ok <- numeric(nrow(trapcells))
    for(i in 1:nrow(trapcells))
      ok[i] <- habMat[trapcells[i,1], trapcells[i,2]]
    if(!all(ok == 1)) {
      cat("The following traps appear to be in bad habitat:", which(!ok), "\n")
      cat("They are circled in the plot.\n")
      cat("If on the edge, this is probably due to rasterization of the habitat polygon.\n")
    }
    if(!all(ok == 1))
      points(trapMat[!ok, ], col=colors[3], cex=2, xpd=TRUE)
  }
}
