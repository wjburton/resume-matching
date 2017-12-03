
# Check if we have the packages needed!
need_packages <- c("shiny")
new.packages <- need_packages[!(need_packages %in% installed.packages()[,"Package"])]
# If we need any, install them
if(length(new.packages)) install.packages(new.packages)

#

#devtools::load_all("C:\\Users\\kaseyriver11\\Google Drive\\scrape_jobs\\resmatch")
library(shiny)
library(tm)
library(SnowballC)
library(rvest)
library(dplyr)
library(stringr)
library(httr)
library(randomForest)
library(BH)
library(devtools)
library(shinyBS)
library(plotly)
library(resmatch)
library(XML)
library(rCharts)



resume <- "
Example

EDUCATION
Western Carolina University						Cullowhee, NC
Master of Science in Applied Mathematics				May 2014
.	GPA: 4.0
Western Carolina University						Cullowhee, NC
Bachelor of Science in Mathematics					May 2013
Bachelor of Science in Education in Mathematics			May 2013
.	GPA: 3.95, summa cum laude
.	Honors College

Actuarial Exam P, Society of Actuaries - Score of 8/10		January 2014

WORK EXPERIENCE
Booz Allen Hamilton							Washington, DC
Sr. Consultant								January 2015-Current
.	Implemented best practices, standardization, and macros in the Office of Immigration Statistics  code for their quarterly reports to Congress
.	Created an Shiny application to help pricing analyst understand market trends and pricing strategies. while raising the company's chances of winning new bids
.	Worked on creating new methodologies for Earned Value Management to help the Air Force better predict the success of their feature projects
.	Implemented D3 visualizations int Shiny applications for the Marines, Air Force, Inova Health, and internal projects
Allegis Global Solutions						Hanover, MD
Invoicing Associate							August 2014-January 2015
.	Consolidated and summarized weekly invoices for a large number of programs
.	Accurately reconciled payments for invoices and created request for funds to generate supplier payments
.	Worked within Excel and Access to maintain customer databases and records
WCU-Math Tutoring Center						Cullowhee, NC
Mathematics Tutor/Graduate Assistant					August 2013-May 2014
.	Tutored individual students in undergraduate math and cross-curricular classes
.	Lead large group problem-solving sessions in preparation for exams and finals
Smokey Mountain High School					Sylva, NC
Teaching Intern				 				January-May 2013
.	Taught AP Calculus and Pre-Calculus courses over a 50 day internship
.	Helped 9 of 10 students pass their AP tests
WCU-Residential Living						Cullowhee, NC
Residential Assistant							January 2012-May 2013
.	Kept accurate and organized student records and forms
.	Planned and implemented educational and social programs
COMPUTER SKILLS
.	Base Certification, work in on a daily basis

AWARDS & SCHOLARSHIPS
.	Graduate Creative Project Award, December 2013
.	Teaching Fellows Scholarship (merit based), 2010-2013
.	Dean's Outstanding Scholar Award, May 2013
.	Senior Mathematics Award, May 2013
.	Freshman Mathematics Award	, May 2011

COMPLETED RESEARCH
.	'Taxicab Geometry: More than a Change in Distance' - won university-wide graduate creative project award

CONFERENCES & CONTEST
.	Presenter, MAA Southeastern Section Conference, Tennessee Tech, March 2014
.	Presenter, Graduate Symposium, WCU, April 2014
.	Participant, Mathematical Contest in Modeling, WCU, February 2012
.	Participant, Putnam Mathematical Contest, WCU, December 2011, 2012
";










#### PLEASE DELETE;
call_indeed_api <- function(search_term = "analyst", location =  "",start_point="", lim = 5){

  api <- 'http://api.indeed.com/ads/apisearch?publisher=5795537109414523'

  xml_file <- GET(api, query = list(q = search_term,
                                    v = 2,
                                    l = location,
                                    filter = 0,
                                    limit = lim,
                                    start = start_point))

  xml_file <- httr::content(xml_file)

  xml_file %>%
    html_nodes('expired') %>%
    html_text() -> expired

  xml_file %>%
    html_nodes('country') %>%
    html_text() -> country

  xml_file %>%
    html_nodes('state') %>%
    html_text() -> state

  xml_file %>%
    html_nodes('city') %>%
    html_text() -> city

  xml_file %>%
    html_nodes('company') %>%
    html_text() -> company

  xml_file %>%
    html_nodes('jobtitle') %>%
    html_text() -> jobtitle

  xml_file %>%
    html_nodes('url') %>%
    html_text() -> href

  xml_file %>%
    html_nodes('date') %>%
    html_text() -> dates

  xml_file %>%
    html_nodes('jobkey') %>%
    html_text() -> jobkey

  indeed_data <- data.frame(expired, country, state,
                            city, company, jobtitle,
                            href, dates, jobkey, stringsAsFactors = F)

  return(indeed_data)

}


convert_href_to_text_indeed <- function(search_term = "analyst", location =  "", start_point = "", lim = 5){

  lim <- ifelse(lim <2,2,lim) #must take atleast 2 jobs

  indeed_df <- call_indeed_api(search_term = search_term,  location = location,
                               start_point = start_point, lim  = lim)
  href_vect <- indeed_df$href

  text_df <- NULL
  for(href in href_vect){
    row <- clean_indeed_text(href)
    text_df <- rbind(text_df,row)
  }

  indeed_df <- cbind(indeed_df, text_df)
  return(indeed_df)
}

clean_indeed_text <- function(href){
  remove <- "\\||~|!|@|#|%|\\^|&|\\*|\\(|\\)|\\{|\\}|_|\\+|\\:|\\|<|>|\\?|,|;|'|\\[|\\]|="
  html <- read_html(href)
  html %>%
    html_nodes('#job_summary') %>%
    html_text()-> text

  text <- gsub("\\n|\\r|\\t", "", text) #remove all \n \r \t
  text <- gsub(remove, " ", text) #remove certain characters as defined in remove
  text <- gsub('[\\.\\/\\$\\-]', " ", text) #remove whitespace and replace with a single space
  text <- gsub('([a-z])([A-Z])','\\1 \\2', text) #add space between lcase and ucase letters
  text <- gsub('[^a-zA-Z0-9 $]', "", text)
  text <- gsub("([a-zA-Z])([0-9])", "\\1 \\2", text)
  text <- gsub('\\s+', " ", text) # convert multiple whitespaces to a single whitespace
  text <- tolower(text) #make everything lowercase
  text <- unlist((str_split(text, " ")))
  text <- text[!(text %in% stopwords)]
  text <- suppressWarnings(text[is.na(as.numeric(text))])
  key_items <- extract_key_terms(text)
  text <- paste(text, collapse = ' ')
  df <- data.frame('text' = text, 'key_terms' = key_items, stringsAsFactors = F)
  return(df)
}

create_text_corpus <- function(text, text_to_remove=c("")){
  r1 <- Corpus(VectorSource(text))                    # Make Corpus
  r1 <- tm_map(r1, removePunctuation)                 # Remove puncuation.
  r1 <- tm_map(r1, removeNumbers)                     # Remove numbers.
  r1 <- tm_map(r1, tolower)                           # Make lower
  r1 <- tm_map(r1, removeWords, stopwords)            # remove stopwords
  r1 <- tm_map(r1, removeWords, text_to_remove)       # additional words removed
  r1 <- tm_map(r1, stripWhitespace)                   # strip white space
  r1 <- tm_map(r1, PlainTextDocument) ;               # Needed after you convert to lowercase
  r1 <- tm_map(r1, stemDocument)                      # stem doc, uses Snowballc package
  r1
}



