
# Function to plot activity centres

plotACs <- function(
    which=NA,     # which ACs to plot (don't usually want to do all in one plot)
    ACs,          # iters x animals x 2 array, as produced by convertOutput; NA out phantoms
    traps,        # 2-column matrix or data frame with trap coordinates
    Y,            # animals x traps matrix with capture histories; rownames assumed to be animal IDs
    hab,          # spatialPolygons object with the extent of the habitat
    howMany=3000, # number of points to plot for each animal
    show.labels=TRUE # whether to label plot with animal IDs
  )  {

  # Reduce number of iterations
  if(dim(ACs)[1] > howMany) {
    keep <- seq(1, dim(ACs)[1], length = howMany)
    ACs <- ACs[keep,,]
  }
  # Get posterior means for locations
  x <- colMeans(ACs[, ,1], na.rm=TRUE)
  y <- colMeans(ACs[, ,2], na.rm=TRUE)

  # Recover animal IDs if possible
  ncap <- nrow(Y) # number of animals captured
  M <- dim(ACs)[2] # total, incl. uncaptures
  animalIDs <- sprintf("id%03d", 1:M)
  if(!is.null(rownames(Y)))
    animalIDs[1:nrow(Y)] <- rownames(Y)

  # which to plot
  if(any(is.na(which)))
    which <- 1:ncap

  # Get capture locations
  captLocList <- vector('list', ncap)
  for(i in 1:ncap) {
    captTraps <- which(Y[i, ] > 0) # Which traps caught the animal
    captLocList[[i]] <- traps[captTraps, , drop=FALSE] # Locations of the traps
  }

  # do the plot
  plot(hab)
  points(traps, pch=3, col='red')
  colors <- palette()[-1]
  colno <- 1
  for(i in which) {
    col <- colors[colno]
    points(ACs[, i, ], cex=0.1, col=adjustcolor(col, 0.3))
    if(i <= ncap)
      points(captLocList[[i]], pch=21, col='black', bg=col, cex=1.2)
    colno <- colno+1
    if(colno > length(colors))
    colno <- 1
  }
  if(show.labels)
    plotrix::boxed.labels(x[which], y[which], labels=animalIDs[which])
  invisible(0)
}

