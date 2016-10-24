
#' Calculates cos_sim between a resume, and job postings
#' @param resume_text_corpus = corpus of the resume
#' @param job_text_corpus = corpus of the job postings
#' @return the cosine similarity scores between
#' @export
#'
#'

cos_sim <- function(resume_text_corpus,job_text_corpus){
    # Calculate Term frequencies for jobs and the resume
    combined_tf <- as.matrix(TermDocumentMatrix(c(resume_text_corpus, job_text_corpus)))

    # only keep terms that are contained in both the query (resume), and in at least one document( job posts)
    combined_tf <- combined_tf[combined_tf[,1] > 0,] #remove terms not in query
    combined_tf <- combined_tf[apply(combined_tf[,-1],1,sum) > 0,] #remove terms not in documents

    #calculate term frequency for the query (resume)
    resume_tf <- combined_tf[,1]
    resume_log_tf <- 1 + log10(resume_tf)

    #calculate term frequency for the document (job posts)
    job_tf <- combined_tf[,-1]
    job_log_tf <- ifelse(is.infinite(log10(job_tf)),0,1 + log10(job_tf))

    #calculate idf
    doc_ocurrence <- as.matrix(apply(job_tf,1, function(x) length(which(x > 0))))
    idf <- log10((length(job_text_corpus)/doc_ocurrence ))

    #calculate query (resume) tf*idf
    resume_tf_idf <- resume_log_tf * idf

    #calculate document (job posts) tf*idf
    job_tf_idf <- apply(job_log_tf, 2, function(x) x * idf)

    #calculate query (resume) unit vector
    resume_tf_idf_unit <- resume_tf_idf/sqrt(sum(resume_tf_idf^2))

    #calculate document (job posts) tf*idf unit vector
    job_tf_idf_unit <- apply(job_tf_idf,2,function(x) x/sqrt(sum(x^2)))

    #find the dot product of the two unit vectoer
    scores <-  apply(job_tf_idf_unit,2, function(x) crossprod(resume_tf_idf_unit, x))
    scores[is.na(scores)] <- 0
    return(scores)
}




#' Scores key terms between resumes and job postings
#' @param job_key_terms = a vector of strings containing key terms found within the job posting
#' @param resume_key_terms = a string containing key terms found within the job posting
#' @return a vector: Scores for each resume-job pair
#' @export

score_key_terms <- function(job_key_terms, resume_key_terms){

    job_key_terms <- strsplit(job_key_terms, split = " ")
    job_key_freq <- sapply(job_key_terms,function(x) log(table(x) + 1))

    resume_key_terms <- unlist(strsplit(resume_key_terms, split = " "))

    scores <- sapply(job_key_freq, function(key_terms) {
        if(length(key_terms)[1] != 0){
            total <- sum(key_terms)
            matched_terms <- key_terms[names(key_terms) %in% resume_key_terms]
            filter_total <- sum(matched_terms)
            return(filter_total/total)
        }else NA
    })
    return(scores)

}



#' Combines scoring functions to create final scores between resumes and job postings
#' @param resume = raw resume
#' @param job_db = a sample of the job dataaframe in posgres and new api pulls
#' @return a vector: Final scores for each resume-job pair
#' @export

create_score_df <- function(resume, resume_exp = 2, job_db){

    #calculate experience diff
    experience_diff <- job_db$years_exp - resume_exp

    #run the exp diff through the gompertz function to return score weight
    experience_weight <- gompertz(ifelse(is.na(experience_diff),0,experience_diff))

    #create corpus of job info
    job_text_corpus <- create_text_corpus(job_db$text)
    job_key_terms <- job_db$key_terms

    #put resume in corpus form
    resume <- clean_text(resume)
    resume_text_corpus <- create_text_corpus(resume$text)
    resume_key_terms <- resume$key_terms

    #score the different jobs using cosine similarity and key terms metric
    #weight the function by the experience weight and output all the scores in a dataframe
    cos_sim <- cos_sim(resume_text_corpus, job_text_corpus)
    key_terms_sim <- score_key_terms(job_key_terms, resume_key_terms)
    final_scores <- ifelse(is.na(key_terms_sim), cos_sim, cos_sim/2 + key_terms_sim/2)
    final_scores <- final_scores * experience_weight
    score_df <- data.frame(cos_sim, key_terms_sim, experience_weight, final_scores)

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

generate_scores <- function(resume, work_exp = 2, search_term = "", location = "",
                            db_lim = 10000, indeed_lim = 5,start_point = 1,
                            pull_from_db = TRUE, pull_from_indeed = TRUE){
  df1 <- NULL
  df2 <- NULL

  if(pull_from_db == TRUE){
    df1 <- query_database(search_term, location, db_lim)
    if(!is.null(df1)){
      df1 %>% select(-c(date, last_checked_date)) -> df1
    }
  }


  if(pull_from_indeed == TRUE){
    df2 <- try(pull_new_job_data(search_term, location, add_to_db = TRUE, lim = indeed_lim, start_point = start_point))
    if(!is.null(df2)){
      df2 %>% select(-c(date, last_checked_date)) -> df2
    }
  }

  job_db <- rbind(df1,df2)
  scored_df <- create_score_df(resume, work_exp, job_db)

  output_for_app <- cbind(job_db,scored_df)
  return(output_for_app)
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
