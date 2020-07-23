
setwd("D:/Github/makeJAGSmask_package") # my desktop
setwd("C:/Github/makeJAGSmask_package") # my laptop
dir()

# Spelling check
library(spelling)
update_wordlist(pkg = "makeJAGSmask", confirm = TRUE)
out <- spell_check_package(pkg = "makeJAGSmask")

# Misc checks
library(tools)
checkTnF("makeJAGSmask")

# Install dependencies
install.packages(c("abind", "plotrix", "raster",
    "secr", "sp", "rgeos"))

# For package development
system("R CMD INSTALL makeJAGSmask") # Use this for a "dev" install.
devtools::load_all("makeJAGSmask")

# Create the makeJAGSmask package
# ==========================
unlink(list.files(pattern="Rplots.pdf", recursive=TRUE))
system("R CMD build makeJAGSmask")  # Produces the .tar.gz
pkg <- "makeJAGSmask_0.1.1.9001.tar.gz"  # <-- fix version number here

# Pick one to check:
## on desktop
system(paste("R CMD check ", pkg))
system(paste("R CMD check ", pkg, "--as-cran"))  # as-cran now runs donttest
## on laptop
system(paste("R CMD check ", pkg, "--no-manual"))
system(paste("R CMD check ", pkg, "--as-cran --no-manual"))

# Pick one to install
system(paste("R CMD INSTALL ", pkg))            # install only
system(paste("R CMD INSTALL ", pkg, "--build")) # install and produce the .zip binary

library(testthat)
test_package("makeJAGSmask", reporter=ProgressReporter)

# Try it out:
library(makeJAGSmask)
?makeJAGSmask
