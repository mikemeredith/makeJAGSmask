
# Function to plot activity centres

plotACs <- function(
    which=NA,     # which ACs to plot (don't usually want to do all in one plot)
    ACs,          # iters x animals x 2 array, as produced by convertOutput; NA out phantoms
    traps,        # 2-column matrix or data frame with trap coordinates
    Y,            # animals x traps matrix with capture histories; rownames assumed to be animal IDs
    hab,          # spatialPolygons object with the extent of the habitat
    howMany=3000, # number of points to plot for each animal
    show.labels=TRUE, # whether to label plot with animal IDs
    rad=50,       # amount of jitter to add to capture locations
    link=TRUE,    # if TRUE, link capture locations with dotted line
    colors        # vector of colors to use for plotting
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
  M <- dim(ACs)[2] # total, incl. uncaptures
  animalIDs <- sprintf("id%03d", 1:M)
  captLocList <- NULL
  if(!missing(Y)) {
    ncap <- nrow(Y) # number of animals captured
    if(!is.null(rownames(Y)))
      animalIDs[1:nrow(Y)] <- rownames(Y)
    # which to plot
    if(any(is.na(which)))
      which <- 1:ncap

    # Get capture locations
    if(!missing(traps)) {
      captLocList <- vector('list', ncap)
      for(i in 1:ncap) {
        captTraps <- which(Y[i, ] > 0) # Which traps caught the animal
        tmp <- traps[captTraps, , drop=FALSE] # Locations of the traps
        jitangle <- runif(nrow(tmp), -pi, pi)
        jit <- cbind(rad*sin(jitangle), rad*cos(jitangle))
        captLocList[[i]] <- tmp + jit
      }
    }
  }
  # if(any(is.na(which)))
    # which <- 1:M

  # do the plot
  # -----------
  MASS::eqscplot(x, y, type='n', xlim=range(ACs[, , 1], na.rm=TRUE),
    ylim=range(ACs[, , 2], na.rm=TRUE),
    ann=FALSE, axes=FALSE)
  if(!missing(hab)) {
    if(inherits(hab, "SpatialPolygons")) {
    sp::plot(hab, add=TRUE)
    } else if((is.matrix(hab) || is.data.frame(hab)) && ncol(hab) == 2) {
      lines(hab)
    }
  }
  if(!missing(traps))
    points(traps, pch=3, col='red')
  if(missing(colors))
    colors <- palette()[-1]
  colno <- 1
  for(i in which) {
    col <- colors[colno]
    points(ACs[, i, ], cex=0.1, col=adjustcolor(col, 0.3))
    if(!is.null(captLocList) && i <= ncap){
      points(captLocList[[i]], pch=21, col='black', bg=col, cex=1.2)
      if(link && nrow(captLocList[[i]]) > 1)
        lines(captLocList[[i]], col=col)
    }
    colno <- colno+1
    if(colno > length(colors))
    colno <- 1
  }
  if(show.labels)
    plotrix::boxed.labels(x[which], y[which], labels=animalIDs[which])

  # data frame to return
  # --------------------
  niter <- dim(ACs)[1]
  tmp <- matrix(ACs[, which, ], ncol=2)
  IDs <- make.unique(animalIDs[which])
  out <- data.frame(ID=factor(rep(IDs, each=niter)), x=tmp[,1], y=tmp[,2])
  out <- out[complete.cases(out), ]
  return(invisible(out))
}

