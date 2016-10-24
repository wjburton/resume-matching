##### Deploying Application

# To update to newest resmatch;
install.packages("C:\\Users\\krjones9\\Desktop\\resmatch", repos = NULL, type = "source")
library(resmatch)
#devtools::load_all("resmatch")


# # Trying to connect to shinyapps.io
# install.packages('rsconnect')
# rsconnect::setAccountInfo(name='kaseyriver11', token='FD1D354700E5367B0AE489E334F05262', secret='mHk43PcRvGfmkgCsTYg5YjmBZV7paeDbxftE6REx')
# library(rsconnect)
# 
# devtools::install_github("wjburton/resmatch", auth_token = "b1ab1368a801cb4fb159d4a91504a74eec06dd11")
# devtools::install_github("krjones9/resmatch")
# 
# rsconnect::deployApp("C:\\Users\\kaseyriver11\\Google Drive\\scrape_jobs\\shiny_resume")

### - - - 
### Education experience 
### - - -

# library(tm)
# library(stringr)
# 
# text <- database$text
# # Basic Information; 
# Name <- str_extract(resume3, "\\w*.+" )
# Location <- str_extract(resume3,"\\w+, \\w+ \\d+")
# Email <- str_extract(resume3, "\\w+@.+")
# Number <- str_extract(resume3, ".*\\d{3}.+\\d{3}.\\d{4}")
# # Education;
# UnderGrad <- str_extract(resume, "(\\wssociate of|\\wachelor of) .+\n")
# Graduate <- str_extract(resume, "(\\waster of|\\woctor of) .+\\n")
# 
# underGrad <- str_extract(resume,"(\\bachelor.*?degree|ba.bs|(\\sba\\s|\\sbs\\s).*?(\\sdegree\\s|\\sin\\s)|b\\.s?.\\s|b\\.a?.\\s)")
# graduate <- str_extract(resume, "(master.+|advanced degree|graduate degree|m\\.s?.\\s)")
# phd <- str_extract(resume, "(phd|ph.d)")
















