# #run when package loaded
#
#
 .onAttach <- function(libname, pkgname) {
   packageStartupMessage("Welcome to resmatch, this package matches resumes to job postings on indeed.com")
}
