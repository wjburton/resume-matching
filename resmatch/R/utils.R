
#Utils

#'  Extract/clean text from indeed.com
#'
#' @description Use regular expressions to eliminate irrelevant html syntax/formatting. Also
#' extract keywords found in the keywords dataset and find max years experience. This can
#' be applied to a string of text (resume), or a href to a job posting.
#' @param href = link to webpage
#' @return dataframe containing three columns: text, key_terms, years_exp
#' @export
#'
clean_text <- function(text = NULL, href = NULL, for_query = FALSE){

  #this chunk is only needed on cleaning text called through the api
  if(!is.null(href)){
    html <- read_html(href)
    html %>%
      html_nodes('#job_summary') %>%
      html_text()-> text
  }

  #Remove things like symbols and extra spaces for a clean body of text
  remove <- "\\||~|!|\\+|@|#|%|\\^|\\*|\\(|\\)|\\{|\\}|_|\\+|\\:|\\|<|>|\\?|,|;|'|\\[|\\]|=|\\/|\\$|\\-"
  text <- gsub("\\n|\\r|\\t", " ", text) #remove all \n \r \t
  text <- gsub(remove, " ", text) #remove certain characters as defined in remove
  text2 <- text # do not remove;
  text <- gsub('[\\.]', "", text) #remove whitespace and replace with a single space
  text <- gsub('([a-z])([A-Z])','\\1 \\2', text) #add space between lcase and ucase letters
  text <- gsub('[^a-zA-Z0-9 $]', "", text)
  #text <- gsub("([a-zA-Z])([0-9])", "\\1 \\2", text)
  text <- gsub('\\s+', " ", text) # convert multiple whitespaces to a single whitespace
  text <- tolower(text) #make everything lowercase

  if(for_query == TRUE){
    return(text)
    break
  }

  internship <- is_internship(text) #is this posting an internship?
  experience <- extract_years_experience(text) #grab max yrs experience
  degree <- extract_highest_degree(text2) #extract highest degree required

  text <- unlist((str_split(text, " "))) #splits the body of text into individual words
  text <- text[!(text %in% stopwords)] #remove stopwords
  text <- suppressWarnings(text[is.na(as.numeric(text))]) #remove numbers

  key_items <- extract_key_terms(tolower(text), unique = FALSE) #grab keywords

  text <- paste(text, collapse = ' ') #collapse back into text string
  df <- data.frame('text' = text,
                   'key_terms' = key_items,
                   'years_exp' = experience,
                   'degree_preferred' = degree,
                   'internship' = internship,
                   stringsAsFactors = FALSE) #form text into dataframe
  return(df)
}







#' Uses tm_package to clean a resume or vector of documents.
#' @description  Creates a corpus of the text/vector. Removes punctuation, numbers, stopwords,
#' whitespace, and stems the document
#' @param text = a resume or vector of documents
#' @return a Vcorpus/Corpus ready to turn into a document term matrix
#' @export
#'
create_text_corpus <- function(text){
  r1 <- Corpus(VectorSource(text)) # Make Corpus
  r1 <- tm_map(r1, stemDocument)  # stem doc, uses Snowballc package
  return(r1)
}






#'extracts key terms found in the text
#'
#' @param text_vect = vector of words
#' @param unique = TRUE/fALSE
#' @param string = TRUE/fALSE
#' @return returns a string containing the keywords found separated by a space
#' @examples extract_key_terms(some_vector_of_words, unique = fALSE)
#' @export
#'
extract_key_terms <- function(text_vect, unique = TRUE){
  #look for key terms within the text documents that match key terms in
  # the key_terms data file
  if(unique==TRUE){
    key_terms <- unique(text_vect[text_vect %in% key_terms])
  }else{
    key_terms <- text_vect[text_vect %in% key_terms]
  }

  key_terms <- paste(key_terms, collapse = " ")
  return(key_terms)

}







#'extracts max number of years experience required
#'
#' @param text =  string of text
#' @return numeric value, the max number of years found
#' @export
#'
extract_years_experience <- function(text){
  experience <- str_match_all(text,'(\\d+) years.*?experience')
  years <- suppressWarnings(as.numeric(experience[[1]][,2]))
  years <- years[years <= 15]
  years <- ifelse(length(years) == 0, NA, years)
  return(years)
}






#'extracts highest level of degree obtained or required
#'
#' @param text =  string of text
#' @return numeric value. NA = no degree, 1 = bachelors, 2 = mastrs, 3 = phd
#' @export
#'
extract_highest_degree <- function(text){
    text<-tolower(text)
    underGrad <- str_extract(text,"( degree |\\bachelor.*?degree|ba.bs|(\\sba\\s|\\sbs\\s).*?(\\sdegree\\s|\\sin\\s)|b\\.s?.\\s|b\\.a?.\\s)")
    graduate <- str_extract(text, "(master.+|advanced degree|graduate degree|m\\.s?.\\s)")
    phd <- str_extract(text, "(phd|ph.d)")

    if(is.na(phd)){
        if(is.na(graduate)){
            if(is.na(underGrad)){
                value <- NA
            } else {value <- 1}
        } else{ value <- 2}
    } else {value <- 2}
    return(value)
}






#' Used to determine a weight for the scores based on work experience
#'
#' @param x = difference between years required experience and current experience
#' @return numeric value. NA = no degree, 1 = bachelors, 2 = mastrs, 3 = phd
#' @export
#'
gompertz <- function(x,a = 1 ,b = 8,c = .8){
       out <- -a*exp(-b*exp(-c*x))
       return(out + 1)
}







#' Determine if posting is an internship
#'
#' @param text = text string
#' @return overwrites the old rdata file with an updated version
#' (you can never remove existing terms, only add new ones)
#' @export
#'
is_internship <- function(text){
  ans <- str_detect(text,' intern |internship| interns ')
  ans <- ifelse(ans == TRUE, "true", "false")
  return(ans)
}







#' Adds key terms to the key_terms.data file
#'
#' @param key_terms_to_add = vector of terms to add
#' @return overwrites the old rdata file with an updated version
#' (you can never remove existing terms, only add new ones)
#' @export
#'
add_to_key_terms <- function(key_terms_to_add) {
  key_terms <- unique(c(key_terms, key_terms_to_add))
  save(key_terms, file = './data/key_terms.rdata')
  load('./data/key_terms.rdata')
}







#'Calculate concordant pairs
#'
#' @param binary = vector of outcomes (0 or 1) not qualified = 0 qualified = 1
#' @return score = vector of scores associated with the outcomes
#' @export
#'
pct_concordant <- function(binary, score){
  tied <- 0
  concordant <- 0
  discordant <- 0
  all_pairs <- combn(1:length(binary),2)
  apply(all_pairs,2, function(x){
   if(!zero_range(binary[x])){
     compare_out <- binary[x]
     compare_score <- score[x]
     add_c <- as.numeric(compare_score[compare_out == 1] == max(compare_score))
     add_d <- as.numeric(compare_score[compare_out == 1] != max(compare_score))
     concordant <<- concordant + add_c
     discordant <<- discordant + add_d
   }
  }
  )
  pct_concordant = (concordant) / (concordant + discordant)
  return(pct_concordant)
}








#'Quickly determines if two numbers in the vector
#'
#' @description This function is used to in the pct concordant function
#' @param x = vector of numbers
#' @return TRUE if numbers are equal FALSE if they are not
#'
#' @export
#'
zero_range <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (length(x) == 1) return(TRUE)
  x <- range(x) / mean(x)
  isTRUE(all.equal(x[1], x[2], tolerance = tol))
}











