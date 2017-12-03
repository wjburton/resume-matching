



#' Scores key terms between resumes and job postings
#' @param job_key_terms = a vector of strings containing key terms found within the job posting
#' @param resume_key_terms = a string containing key terms found within the job posting
#' @return a vector: Scores for each resume-job pair
#' @export

score_key_terms <- function(job_key_terms, resume_key_terms){

    job_key_terms <- strsplit(job_key_terms, split = " ") %>% unlist
    job_key_freq <- log(table(job_key_terms) + 1)

    resume_key_terms <- unlist(strsplit(resume_key_terms, split = " "))

     if(length(job_key_terms)[1] != 0){
       total <- sum(job_key_freq)
       matched_terms <- job_key_freq[names(job_key_freq) %in% resume_key_terms]
       filter_total <- sum(matched_terms)
       score <- (filter_total/total)
      }else{
        score <- NA
    }

    return(score)
}



#' Combines scoring functions to create final scores between resumes and job postings
#' @param resume = raw resume
#' @param job_db = a sample of the job dataaframe in posgres and new api pulls
#' @return a vector: Final scores for each resume-job pair
#' @export

create_score_df <- function(resume_df, resume_exp = 2, job_df){

  #calculate experience diff
  experience_diff <- job_df$years_exp - resume_exp

  #run the exp diff through the gompertz function to return score weight
  experience_weight <- gompertz(ifelse(is.na(experience_diff),0,experience_diff))

  #create corpus of job info
  job_text_corpus <- create_text_corpus(job_df$text)
  job_key_terms <- job_df$key_terms

  #score the different jobs using cosine similarity and key terms metric
  #weight the function by the experience weight and output all the scores in a dataframe
  tdm <- tm::TermDocumentMatrix(tm::Corpus(tm::VectorSource(c(resume_df$text, job_df$text)))) %>% as.matrix
  matching_score <- lsa::cosine(tdm)[1,2]
  key_terms_sim <- score_key_terms(job_key_terms = job_df$key_terms,
                                   resume_key_terms = resume_df$key_terms)
  final_scores <- ifelse(is.na(key_terms_sim), matching_score, matching_score + key_terms_sim/2)
  final_scores <- final_scores * experience_weight

  unique_job_key_terms <- job_df$key_terms %>% strsplit(' ') %>% unlist %>%  unique
  unique_resume_key_terms <- resume_df$key_terms %>% strsplit(' ') %>% unlist %>% unique

  exp_needed <- ifelse((job_df$years_exp - resume_exp) > 0, job_df$years_exp - resume_exp, 0)
  exp_needed <- paste(exp_needed, 'years')
  missing_terms <- paste(unique_job_key_terms[!(unique_job_key_terms %in% unique_resume_key_terms)], collapse = ' ')
  score_df <- data.frame(final_scores =paste0(round(final_scores,2)*100, '% Match'), resume_key_terms = paste(unique_resume_key_terms, collapse = ' '),
                         job_key_terms = paste(unique_job_key_terms, collapse = ' '), exp_needed = exp_needed,
                         missing_key_terms = missing_terms)

  return(score_df)
}


#' Combines scoring functions to create final scores between resumes and job postings
#' @param resume = raw resume
#' @param work_exp = work_experience of person
#' @param search_term = term to search job postings for
#' @param location = location to search job postings for
#' @param db_lim = limit on number of rows to query
#' @param indeed_lim = limit on number of indeed.com jobs to pull and score (max 25)
#' @return a dataframe needed for the app
#' @export

generate_scores <- function(resume, href, work_exp){
  clean_job_df <- clean_text(href = href)
  clean_resume_df <- clean_text(text = resume)
  scored_df <- create_score_df(resume_df = clean_resume_df, resume_exp = work_exp,
                               job_df = clean_job_df)
  return(scored_df)
}




#50% of the score determines whether an individual is qualified or not based on key terms



#
#  resume <- "	OVERVIEW
#
# Highly self-motivated and passionate individual who takes great pride in work delivered with:
#   •	Advanced modeling experience: Built models in multiple industries to drive business decisions
# •	Project management experience:
#   •	Diverse Industry Knowledge: Manufacturing, Healthcare, Consulting, Analytics
#
# EDUCATION
#
# North Carolina State University
# M.S. Analytics              May 2017                   B.S. Industrial Engineering              January 2016
# Practicum: Sponsor-approved content              Health Systems Engineering Certificate Recipient
# Sponsor-approved content                                Minor: Statistics
#
# TECHNICAL SKILLS
#
# Languages:	Advanced: R		       Intermediate: SAS, SQL, Python, VBA, Git
# Software: 	Advanced: Simio, Excel
#
# PROFESSIONAL EXPERIENCE
#
# Elder Research                                                                                                                           Raleigh, NC Data Scientist                                                                                                    January, 2016 — June, 2016
# •	Applied statistical/machine learning for the New York center of excellence to identify fraud and abuse within the unemployment insurance program
# •	Built R packages to validate unemployment insurance data, check model accuracy, and identify concept drift
#
# Premier Inc.                                                                                                                              Charlotte, NC
# Project Manager Intern                                                                                    May, 2015 — August, 2015
# •	Developed churn model to identify hospitals at risk of dropping software contracts
# •	Consulted Conway Medical Center on how to improve patient workflow in their pediatric clinic and achieve “patient centered medical home” status
#
#
#
# Highland Industries Inc.                                                                                                            Cheraw, SC
# Co-op Tech II		     June, 2013 — June, 2014
# •	Wrote VBA programs to analyze and monitor chemical consumption, and forecast chemical inventory levels
# •	Presented projects bi-weekly to 30 company managers
#
# ACADEMIC PROJECTS
#
# Duke Hospital - Senior Design                                                                        January, 2015 — May, 2015
# •	Simulated current-state conditions to identify physician rescheduling bottlenecks. The Implemented changes reduced process variation by  92%
#
# Volvo Group – Logistics For Improvement                                                    May, 2014 — August, 2014
# •	Worked in a group of four to develop Volvo Truck Sun Visor designs that improved packing efficiency by 25%
# Sports Analytics Club: NFL Play Prediction				       May, 2015 — August, 2015
# •	Created logistic regression model to predict NFL play and presented the model and results at Joint Statistical Meetings 2015
# •	Model was featured on Newsweek, Discovery News, US News, Wired, ScienceDaily, etc.
# •	Attended super bowl media day to discuss the model and the trend of analytics in football
# •	Featured as a “rising star” on ozy.com"
