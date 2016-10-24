# Good luck!

shinyUI(fluidPage( theme = "bootstrap.css",
    #tags$head(tags$script(src="function.js")),
    
navbarPage("Matching Resumes to Indeed Job Postings: An R Shiny Application", 
    tabPanel("Job Lookup and Scoring", 
             
    fluidPage(
        fluidRow(
           tags$br(),
           column(1),
           column(3,textInput("info", "Job Details:", value = "Data Scientist")), 
                                 #placeholder = "job title, description, or company name")),
           column(3,textInput("location", "Location:", value = "Washington, DC")),
                                #placeholder = "Ex: Franklinville, NC")),
           column(3,sliderInput("experience", "Years of Related Experience:", 
                                min=0, max=40, value=0))
           )
    ),
    
    fluidPage(

        fluidRow(
            column(1),
            column(1,
                   actionButton("showResume", "Enter Resume:", width = "120%")),
            column(1,
                   actionButton("go", "Get Results:", width="120%")),
            column(1),
            column(3, tags$h4("Filter (remove):"),
                   checkboxInput('filter1', 'Internships', value=TRUE),
                   checkboxInput('filter2', 'Jobs with lower degree requested'))
            ),
        tags$br()
    ),
     
   fluidPage(
     fluidRow(
       column(1),
       column(10,tags$h3(uiOutput("value")), dataTableOutput("recentTable1"))
       #column(4,plotlyOutput("topSkills"))
     )),
    
    ## ----
    ### The Modal information
    ## ----
    
    #### Resume Entry
    bsModal("modelExmple", "Resume Entry:", "showResume", size = "large",
        fluidRow(
            column(8,
                   tags$h4("Enter plain text resume below:"),
                   tags$textarea(id="resume10", rows=20, cols=100, resume))
            ))
    ),
    #### Resume Instructions
    # bsModal("modal1", "Application Instructions:", "instructions", size = "large",
    #         tags$div("Here are some instructions.. ", tags$br())
    #   ),
   
   ##
   # Page 2!!!!!!!
   ##
   
   tabPanel("Instructions:",
        fluidPage(
            fluidRow(
                column(2),
                column(8,
                       tags$div(
                           tags$h1("Application Instructions and Explanation"), tags$br()),
                       tags$div(h4("Goal:")),
                       tags$div(
"The goal of this application is to improve on the way current job search engines work. 
Instead of being limited to searching a job title and location, this application takes into 
acocunt the user's resume, years of related job experience, and technical skills. 
Currently the application only works for people searching for a data focused position." 
                       ),
                       tags$h4("Resume Instructions:"),
                       tags$div(
"Resume must be pasted into resume input as text. Please do not include personal information or
references, as the extra text may adjust the scoring algorithm."
                        ),
                        tags$h4("Jod Details Instructions:"),
                        tags$div("Type a job title, job description, or company name"),
                        tags$h4("Location Instructions:"),
                        tags$div("Format: 'City, State'. Can be left blank to search entire database."),
                        tags$h4("Filter Explanation:"),
                        tags$div("Some job postings are for internships. The internship filter removes 
                                 these from view. The degree filter will remove jobs that request a
                                 degree that is lower than the highest degree on our resume.")
                        ))))
    # tabPanel("Update Resume",
    # fluidRow(
    #     column(6,
    #            tags$h4("update your resume..."),
    #            tags$textarea(id="resume11", rows=25, cols=70, "resume11")),
    #     column(6,
    #            tags$h4("to match this job posting..."),
    #            tags$textarea(id="jobPosting", rows=25, cols=70, "Paste Job Posting Here"))
    #            #htmlOutput("frame"))
    # ),
    #     actionButton("update", "Update Resume:", width="150px")
    #  )
)))





