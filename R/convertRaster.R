# Convert a habitat raster and trap coordinates for use with JAGS
# Output: list with habitat matrix, new traps coords as a matrix,
#   extent, pixel width, and area in original units;
#   attributes: original bounding box as a matrix, false origin, and pixel width.

convertRaster <- function(raster, traps, plot=TRUE) {

  # Sanity checks
  if(!inherits(raster, "Raster"))
    stop("'", deparse(substitute(raster)), "' is not a valid 'raster' object.")
  raster <- raster::trim(raster)
  if(!inherits(traps, "data.frame"))
    stop("'", deparse(substitute(traps)), "' is not a valid 'data.frame' object.")
  # Get point spacing, which will be our pixelWidth
  pixWidth <- xres(raster)
  stopifnot(all.equal(pixWidth, yres(raster)))

  bbox <- matrix(extent(raster)[c(1,2,2,1,3,3,4,4)], 4, 2)
  origin <- bbox[1, ] - pixWidth

  # Generate habMat
  r <- raster[[1]]
  values(r) <- !is.na(values(r))
  tmp <- raster::as.matrix(r) * 1
  habMat <- t(tmp[nrow(tmp):1, ])

  # Convert raster or stack to a list of matrices # new 2019-07-2
  nl <- nlayers(raster)
  covs <- vector('list', nl)
  if(nl == 1) {
    names(covs) <- "covMat"
  } else {
    names(covs) <- names(raster)
  }
  for(i in 1:nl) {
    tmp <- raster::as.matrix(raster[[i]])
    tmp[is.na(tmp)] <- 0
    covs[[i]] <- t(tmp[nrow(tmp):1, ])
  }
  # str(covs)
  
  # Convert trap coordinates to the new units:
  newtraps <- sweep(traps, 2, origin) / pixWidth

  out <- c(covs, list(habMat = habMat,
              trapMat = as.matrix(newtraps),
              upperLimit = c(x=nrow(habMat)+1, y=ncol(habMat)+1),
              pixelWidth = pixWidth,
              area = sum(habMat) * pixWidth^2))
  attr(out, "boundingbox") <- bbox
  attr(out, "origin") <- origin
  attr(out, "pixelWidth") <- pixWidth
  class(out) <- "JAGSmask"

  if(plot)
    plot.JAGSmask(out)

  return(out)
}


if(FALSE) {

str(tmp <- convertRaster(stack, traps, plot=TRUE))
str(tmp <- convertRaster(patchR, traps, plot=TRUE))



}

