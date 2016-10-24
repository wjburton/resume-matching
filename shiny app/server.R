
library(shinyjs)

shinyServer(function(input, output, session){
   
createLink <- function(val) {
    sprintf(paste0("<a href=", '"',val,'"', 'target="_blank" class="btn btn-primary"> Link </a>'),val)
} 
# createCopy <- function(val) {
#     sprintf(paste0(
#         "<button class='btn btn-primary' id=", '"demo"', 'value =', "'",val,"'",  'onclick="copyToClipboard(document.getElementById(', "'demo'",
#         ').value)">Copy</button>'))
# } 

output$recentTable1 <- renderDataTable({
    if (input$go == 0)
        return(NULL)
    return(isolate({
    # DO THE SEARCH;
    df <- values()
    
    # Did the user say filter out internships?
    if(input$filter1 == TRUE){
        df <- subset(df, (df$internship == "false"))
    }
    
    # What about on degree?
    if(input$filter2 == TRUE){
        res <- clean_text(input$resume10)$degree_preferred
        df <- subset(df, (df$degree_preferred == res ))
    }
    # Fix the dataframe for the user;
    df <- df[order(-df$final_scores),]
    df$location <- paste0(df$city, ", ", df$state)
    keep <- c("company", "job_title", "href",
              "key_terms", "years_exp", "final_scores", "location", "degree_preferred")
    df <- df[,colnames(df) %in% keep]
    
    #Grab Unique Key Terms
    char <- sapply(df$key_terms,function(x) unique(unlist(strsplit(x, " "))))
    char2 <- sapply(char, function(x) paste(x,collapse = " "))
    df$key_terms <- char2
    
    # Rename columns, and order the desired output;
    colnames(df) <- c("Company", "Job Title", "Link", "Technologies/Skills", 
                      "Years Experience", "Degree Preferred", "Score", "Location")
    #df$"Copy Job Description" <- createCopy(df$Text2)
    df <- df[,c(1,2,8,4,5,6,7,3)]
    DB <<- df
    #df <- df[,c(1,2,8,4,5,6,3)]
    df$Link <- createLink(df$Link)
    df
    }))
}, escape = FALSE) #, row.names = FALSE)
  

#### Having Problems getting this to show up; 
# output$topSkills <- renderPlotly({
#     if (input$go == 0)
#         return(NULL)
#     return(isolate({
#     count <- barInfo()
#     db <- values()
#     x <- list(title = "Skills/Technologies")
#     y <- list(title = "Frequency")
#     p <- plot_ly(x = count$a,y = count$Freq,
#                name = "SF Zoo",type = "bar") %>%
#       layout(title = paste0("Top 5 skills needed for ",  dim(db)[1], " analyzed positions."), xaxis = x, yaxis = y)
#     p
#     }))
# })
  
### Page 2 things ###
# observe({
#     updateTextInput(session, "resume11", value = input$resume10)
# })
# observe({
#     a <- res()
#     if (input$update == 1){
#         updateTextInput(session, "resume10", value = input$resume11)}
# })

# observe({
#     #query <- members[which(members$nr==input$Member),2]
#     test2 <- input$joblink
# })
# output$frame <- renderUI({
#     a <- "http://news.scibite.com/scibites/news.html?q=GENE$BCRA1"
#     my_test <- tags$iframe(src="https://www.google.com", height=500, width="100%")
#     print(my_test)
#     my_test
# })

### call reactives ###
    panel1.server.dir <- 'external.server/'
        source(paste0(panel1.server.dir, 'reactives.server.R'), local = T)
})


# withProgress(message = 'Running Analysis', value = 0, {
#   # Number of times we'll go through the loop
#   n <- 5
#   for (i in 1:n) {
#     # Each time through the loop, add another row of data. This is
#     # a stand-in for a long-running computation.
#     # Increment the progress bar, and update the detail text.
#     incProgress(1/n, detail = paste("Part ", i))
#     # Pause for 0.1 seconds to simulate a long computation.
#     Sys.sleep(0.1)
#   }
# })jobs



