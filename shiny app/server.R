
library(shinyjs)

shinyServer(function(input, output, session){

createLink <- function(val) {
    sprintf(paste0("<a href=", '"',val,'"', 'target="_blank" class="btn btn-primary"> Link </a>'),val)
}

output$resume_skills_tbl <- renderDataTable({
    if (input$go == 0)
        return(NULL)
    return(isolate({
    score_data <<- values()
    print(score_df)
    data.frame('Your Skills' = score_data$resume_key_terms %>% as.character() %>% strsplit(' ') %>% unlist)
    }))
}, escape = FALSE, options = list(paging = FALSE,
                                  searching = FALSE,
                                  ordering = FALSE,
                                  fixedHeader.footer = FALSE,
                                  footerCallback = FALSE,
                                  bInfo = FALSE)) #, row.names = FALSE)

output$job_skills_tbl <- renderDataTable({
  if (input$go == 0)
    return(NULL)
  return(isolate({
    data.frame('Skills Missing' = score_data$missing_key_terms %>% as.character() %>% strsplit(' ') %>% unlist)
  }))
}, escape = FALSE, options = list(paging = FALSE,
                                  searching = FALSE,
                                  ordering = FALSE,
                                  fixedHeader.footer = FALSE,
                                  footerCallback = FALSE,
                                  bInfo = FALSE))

output$job_exp <- renderDataTable({
  if (input$go == 0)
    return(NULL)
  return(isolate({
    data.frame('Experience Needed' = score_data$exp_needed)
  }))
}, escape = FALSE, options = list(paging = FALSE,
                                  searching = FALSE,
                                  ordering = FALSE,
                                  fixedHeader.footer = FALSE,
                                  footerCallback = FALSE,
                                  bInfo = FALSE))

output$score <- renderDataTable({
  if (input$go == 0)
    return(NULL)
  return(isolate({
    data.frame('Matching.Score' = score_data$final_scores)
  }))
}, escape = FALSE, options = list(paging = FALSE,
                                  searching = FALSE,
                                  ordering = FALSE,
                                  fixedHeader.footer = FALSE,
                                  footerCallback = FALSE,
                                  bInfo = FALSE))

output$coursera_table <- renderDataTable({
  if (input$go == 0)
    return(NULL)
  return(isolate({
    score_df <<- query_coursera()
    data.frame('Classes' = score_df$classes, 'Link' = score_df$link)
  }))
}, escape = FALSE)


### call reactives ###
    panel1.server.dir <- 'external.server/'
        source(paste0(panel1.server.dir, 'reactives.server.R'), local = T)
})



