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
  stopifnot(pixWidth == yres(raster))

  bbox <- matrix(extent(raster)[c(1,2,2,1,3,3,4,4)], 4, 2)
  origin <- bbox[1, ] - pixWidth

  # Generate habMat
  r <- raster
  values(r) <- !is.na(values(raster))
  tmp <- raster::as.matrix(r) * 1
  habMat <- t(tmp[nrow(tmp):1, ])

  # Convert raster to a matrix
  tmp <- raster::as.matrix(raster)
  tmp[is.na(tmp)] <- 0
  covMat <- t(tmp[nrow(tmp):1, ])

  # Convert trap coordinates to the new units:
  newtraps <- sweep(traps, 2, origin) / pixWidth

  out <- list(habMat = habMat,
              covMat = covMat,
              trapMat = as.matrix(newtraps),
              upperLimit = c(x=nrow(habMat)+1, y=ncol(habMat)+1),
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




