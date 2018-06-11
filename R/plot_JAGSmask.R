# Function to plot a JAGSmask object

plot.JAGSmask <- function(x, colors=c("grey", "white", "yellow"),  verify=TRUE, ...) {
  if(is.null(x$coreMat)) {
    toPlot <- x$habMat
  } else {
    toPlot <- x$habMat + x$coreMat
  }
  bbox <- attr(x, "boundingbox")
  xlabels <- pretty(bbox[1:2, 1], n=5)
  xpos <- (xlabels - bbox[1, 1]) / diff(bbox[1:2, 1]) * nrow(x$habMat) + 1
  ylabels <- pretty(bbox[2:3, 2], n=5)
  ypos <- (ylabels - bbox[2, 2]) / diff(bbox[2:3, 2]) * ncol(x$habMat) + 1
  
  image(x=1:x$upperLimit[1],
        y=1:x$upperLimit[2],
        z=toPlot,
        ann=FALSE, axes=FALSE, col=colors, ...)
  title(xlab="Easting", ylab="Nothing") 
  axis(1, at=xpos, labels = xlabels)
  axis(2, at=ypos, labels = ylabels)
  box()
  points(x$trapMat, pch=3, col='red', xpd=TRUE)

  if(verify) {
    # Check locations of traps
    trapcells <- floor(x$trapMat)
    ok <- numeric(nrow(trapcells))
    for(i in 1:nrow(trapcells))
      ok[i] <- x$habMat[trapcells[i,1], trapcells[i,2]]
    if(!all(ok == 1)) {
      cat("The following traps appear to be in bad habitat:\n", which(!ok), "\n")
      cat("They are circled in the plot.\n")
      cat("If on the edge, this is probably due to rasterization of the habitat polygon.\n")
    }
    if(!all(ok == 1))
      points(x=x$trapMat[!ok, 1], y=x$trapMat[!ok, 2], col='red', cex=2, xpd=TRUE)
  }
}
