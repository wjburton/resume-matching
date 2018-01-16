
#indeed specific functions


#' indeed posting to data frame function
#'
#' @description This function combines specific functions that work on job postings on
#' indeed.com. It calls the indeed.com api for job information and extracts text off the job posting
#' @param search_term = search term for api
#' @param location = location for api
#' @param start_point = job number starting point for the api
#' @param lim = the max number of hrefs to visit
#' @return a dataframe containing href, useful text and keywords. If api is used
#' then it returns a dataframe containing href, useful text, keywords, and also all other api info
#' @export

pull_new_job_data <- function(search_term = "analyst", location =  "", start_point = "", lim = 5, add_to_db = FALSE){
  #grab job info using indeed database
  indeed_df <- call_indeed_api(search_term = search_term,  location = location,
                               start_point = start_point, lim  = lim)

  #go to each job posting and extract job description using the clean text funcion
  href_vect <- indeed_df$href
  text_df <- lapply(href_vect, function(href) clean_text(href = href))
  text_df <- do.call(rbind,text_df)
  text_df$last_checked_date <- date()
  #combine the api data to description data
  indeed_df <- cbind(indeed_df, text_df)

  return(indeed_df)
}


#' URL to job data frame function
#'
#' @description This function combines all other functions to take the href of a job posting
#'and output all relevant information about a job
#' @param href_vector = a vector containing hrefs
#' @param lim = the max number of hrefs to visit
#' @param location = location for api
#' @param start_point = job number starting point for the api
#' @return a dataframe containing href, useful text and keywords. If api is used
#' then it returns a dataframe containing href, useful text, keywords, and also all other api info
convert_href_to_text <- function(href = NULL, location =  "", start_point= ""){

  print('Scraping website')
  #visit hrefs and extract raw text
  html_text_col <- NULL
  html_text <- as.character(read_html(href))
  print('Grabbing useful text')
  #write to file
  text_df <- NULL
  html_text
  text <- suppressWarnings(extract_useful_text(html_text))
  text_df <- rbind(text_df, text)

  print('converting api info and text into dataframe')
  job_info <- cbind(href_vector, text_df, stringsAsFactors = F)
  return(job_info)
}

#'extract jobs on coursera
#'
#' @description This scrapes the jobs on coursera based on what is missing from a resume
#' @param query = a string of skills separated by a plus ex. statistics+linear alg
#' @return a dataframe containing href, useful text and keywords. If api is used
#' then it returns a dataframe containing href, useful text, keywords, and also all other api info
#' @export
coursera_scrape <- function(query){
  url <- paste0('https://www.coursera.org/courses?languages=en&query=',query)
  page <- read_html(url)
  class_names <- page %>%
                html_nodes('.headline-1-text') %>%
                rvest::html_text()

  hrefs <- page %>%
           html_nodes('a.rc-OfferingCard.nostyle') %>%
           html_attr('href')
  hrefs <- paste0('https://www.coursera.org', hrefs)

  class_df <- data.frame(classes = class_names, link = hrefs)
  return(class_df)
}




