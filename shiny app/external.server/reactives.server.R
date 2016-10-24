
values <- eventReactive(input$go, {
    if (input$go == 0)
        return(NULL)
    return(isolate({
    # Call API results

    if(exists("number_pulls") == FALSE){number_pulls = 0}
    number_pulls <- number_pulls + 1
    if(number_pulls < 20){
        df <- generate_scores(input$resume10, work_exp = input$experience, search_term = input$info,
                          location = input$location, indeed_lim = 10, pull_from_indeed = FALSE)
    }else {
         df <- generate_scores(input$resume10, work_exp = input$experience, search_term = input$info,
                               location = input$location, indeed_lim = 10, pull_from_indeed = FALSE)}
    vvalue <<- dim(df)
    df
    }))
})

output$value <- renderUI({
    if (input$go == 0)
        return(NULL)
    return(isolate({
    value1 <- vvalue[1]
    a <- paste0(value1," ", input$info, " jobs analyzed.")
    a}))
})

# barInfo <- reactive({
#     db <- values()
#     a <- strsplit(db$key_terms, split = " ")
#     a <- lapply(a, function(x) unique(x))
#     a <- unlist(a)
#     count <- table(a)
#     count <- count[order(-count)]
#     count <- as.data.frame(count[1:5])
#     count
# })

# res <- reactive({
#     res <- input$resume11
# })

# userInput <- reactive({
#     Description <- input$info
#     Location <- input$location
#     Years_Exp <- input$experience
#     Desired_Salary <- input$salary
#     Resume <- input$salary
#     lineinfo <- as.data.frame(cbind(Description, Location, Years_Exp, Desired_Salary, Resume))
# })








