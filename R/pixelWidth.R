# Extract pixel width from a JAGSmask object.

pixelWidth <- function(JAGSmask) {
  pixWidth <- JAGSmask$pixelWidth
  if(is.null(pixWidth))
    pixWidth <- attr(JAGSmask, "pixelWidth")
  return(pixWidth)
}




