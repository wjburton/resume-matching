
#' Make data request to indeed.com api
#'
#' @param search_term = the term you want to search for within indeed
#' @param location = location of jobs to search
#' @param start_point = how far to look back
#' @param lim = max number of jobs to return.. max = 25
#' @return a dataframe containing information about specific jobs that were pulled
#' with the api request: expired, country, state, city, company, title, href, dates, jobkey
#' @export
call_indeed_api <- function(search_term = "analyst", location =  "",start_point="", lim = 5){

  api <- 'http://api.indeed.com/ads/apisearch?publisher=5795537109414523'

  #make api request
  xml_file <- GET(api, query = list(q = search_term,
                               v = 2,
                               l = location,
                               filter = 1,
                               limit = 20,
                               start = start_point))

  xml_file <- httr::content(xml_file)


  # parse through content to extract useful information
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
    html_text() -> job_title

  xml_file %>%
    html_nodes('url') %>%
    html_text() -> href

  xml_file %>%
    html_nodes('date') %>%
    html_text() -> date

  xml_file %>%
    html_nodes('jobkey') %>%
    html_text() -> job_key

  summary_df <- NULL
  for(i in 1:length(href)){
    new_row <- clean_text(href = href[i])
    summary_df <- rbind(summary_df, new_row)
  }

  search_term <- rep(search_term, length(job_key))

  location <- rep(location,length(job_key))

  #take useful content and form a database
  indeed_data <- data.frame(search_term, 'search_location' = location, expired, country, state,
                            city, company, job_title,
                            href, date, job_key,summary_df, stringsAsFactors = F)
  return(indeed_data)

}





