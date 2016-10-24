# #run when package loaded
#
#
 .onAttach <- function(libname, pkgname) {
   packageStartupMessage("Welcome to resmatch, this package matches resumes to job postings on indeed.com")
  #create the connection to the postgres database
  # save the password that we can "hide" it as best as we can by collapsing it
  pw <- 'MSA2016'

  # loads the PostgreSQL driver
  drv <- try(dbDriver("PostgreSQL"))

  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <<- try(dbConnect(drv, dbname = "indeed_db",
                             host = "localhost", port = 5432,
                             user = "postgres", password = pw))
  number_pulls <<- 0

}
