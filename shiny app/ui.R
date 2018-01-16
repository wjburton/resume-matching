
shinyUI(fluidPage( theme = "bootstrap.css",
    #tags$head(tags$script(src="function.js")),

navbarPage("Path to your dream job: An R Shiny Application",
    tabPanel("Job Scoring, and Skills Matching",

        fluidRow(
           tags$br(),
           column(offset = 1, width = 1, actionButton("showResume", "Enter Resume:  ", width = "140%")),
           column(offset = 1, width = 3, sliderInput("experience", "Years of Related Experience:", min=0, max=40, value=0)),
           column(offset = 1, width = 3, textInput("href", "Job Link:", value = "paste link here")),
           column(width = 1, actionButton("go", "Get Results:", width="140%")),
        fixedRow(
          tags$br()
        ),
        fixedRow(
          column(offset = 2, width = 2,tags$h2(uiOutput("value")), dataTableOutput("resume_skills_tbl")),
          column(width = 2, tags$h2(uiOutput("value2")), dataTableOutput("job_skills_tbl")),
          column(width = 2,tags$h2(uiOutput("value3")), dataTableOutput("job_exp")),
          column(width = 2,tags$h2(uiOutput("value4")), dataTableOutput("score"))
          ),
        dataTableOutput("coursera_table")
    ),

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





