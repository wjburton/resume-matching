
values <- eventReactive(input$go, {
    if (input$go == 0)
        return(NULL)
    return(isolate({
    score_data <<- generate_scores(resume = input$resume10, href = input$href, work_exp = input$experience)
    vvalue <<- dim(score_data)
    score_data
    }))
})

query_coursera <- eventReactive(input$go, {
  query <- stringr::str_replace_all(score_data$missing_key_terms, ' ', '+')
  coursera_scrape <<- coursera_scrape(query = query)
  coursera_scrape
})










