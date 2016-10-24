
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
  if(search_term == ""){
    search_term <- 'analyst'
  }
  lim <- ifelse(lim <2,2,lim) #must take atleast 2 jobs

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

  #if desired, add the newly pulled data to the postgres database
  if(add_to_db == TRUE){
    postgres_append(new_df = indeed_df, append = TRUE)
  }

  return(indeed_df)
}



