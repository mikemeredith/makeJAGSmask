# Function to print a JAGSmask object

print.JAGSmask <- function(x, ...) {
  cat("An x of class 'JAGSmask'\n")
  size <- dim(x$habMat)
  ntraps <- nrow(x$trapMat)
  cat("The habitat mask has", size[1], "rows and", size[2], "columns.\n")
  cat("The trap matrix has coordinates for", ntraps, "traps.\n")
  cat("Original bounding box is:\n")
  print(attr(x, "boundingbox"))
}
