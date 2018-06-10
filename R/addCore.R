# Create a new mask object including a specified subset of good-habitat pixels

addCore <- function(JAGSmask, type = c("perimeter", "traps", "polygon"),
    buffer = 0, poly=NULL, cell.overlap = c("centre","any","all"), plot=TRUE)  {

  if(!inherits(JAGSmask, "JAGSmask"))
    stop("JAGSmask is not a valid 'JAGSmask' object.")

  type <- match.arg(type)
  olap <- match.arg(cell.overlap)

  pixCoord <- cbind(x=as.vector(row(JAGSmask$habMat))+0.5,
                    y=as.vector(col(JAGSmask$habMat))+0.5)
  pixOriginal <- convertOutput(pixCoord, JAGSmask)
  if(olap == "any" || olap == "all") {
    corns <- cbind(c(-0.5,-0.5,+0.5,+0.5), c(-0.5,+0.5,-0.5,+0.5))
    corners <- array(0, c(nrow(pixCoord), 4, 2))
    for(i in 1:4)
      corners[,i,] <- sweep(pixCoord, 1:2, corns[i, ], "+")
    cornOriginal <- convertOutput(corners, JAGSmask)
  }

  if(type == "traps") {
    if(buffer == 0)
      warning("type='traps' with buffer = 0 will select zero pixels!")
    bufferPix <- buffer / pixelWidth(JAGSmask)
    if(olap == "centre") {
      isCore <- secr::distancetotrap(pixCoord, JAGSmask$trapMat) <= bufferPix
    } else {
      isCornerCore <- matrix(FALSE, nrow(pixCoord), 4)
      for(i in 1:4) {
        isCornerCore[,i] <- secr::distancetotrap(corners[,i,], JAGSmask$trapMat) <= bufferPix
      }
      sumcorn <- rowSums(isCornerCore)
      if(olap=='any') {
        isCore <- sumcorn > 0
      } else {
        isCore <- sumcorn == 4
      }
    }
  }

  if(type == "perimeter") {
    tmp <- JAGSmask$trapMat[chull(JAGSmask$trapMat), ]
    poly <- convertOutput(tmp, JAGSmask)
    type <- "polygon"
  }

  if(type == "polygon") {
    if(is.null(poly))
      stop("'poly' must be supplied for type='polygon'")
    if(!inherits(poly, "SpatialPolygons"))
      poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(poly)), 1)))
    if(buffer > 0)
      poly <- rgeos::gBuffer(poly, width = buffer)
    if(olap == "centre") {
      isCore <- secr::pointsInPolygon(pixOriginal, poly)
    } else {
      isCornerCore <- matrix(FALSE, nrow(pixCoord), 4)
      for(i in 1:4) {
        isCornerCore[,i] <- secr::pointsInPolygon(cornOriginal[,i,], poly)
      }
      sumcorn <- rowSums(isCornerCore)
      if(olap=='any') {
        isCore <- sumcorn > 0
      } else {
        isCore <- sumcorn == 4
      }
    }
  }

  JAGSmask$coreMat <- JAGSmask$habMat * isCore
  if(plot)
    plot(JAGSmask)
  attr(JAGSmask, "poly") <- poly
  return(JAGSmask)
}
