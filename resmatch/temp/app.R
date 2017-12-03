#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(titlePanel("Some Iframe"),
                mainPanel(fluidRow(
                  tags$iframe(seamless="seamless",src="https://www.coursera.org/browse/statistics?languages=en", height=800, width=1400)
                )
                )
)

server <- function(input, output) {}
shinyApp(ui, server)
