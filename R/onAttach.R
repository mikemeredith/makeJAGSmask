

.onAttach <- function(libname, pkgname) {
  version <- try(packageVersion('makeJAGSmask'), silent=TRUE)
  if(!inherits(version, "try-error"))
    packageStartupMessage("This is makeJAGSmask ", version,
      ". For overview type ?makeJAGSmask; for changes do news(p='makeJAGSmask').")
}
