library(shiny)
library(shinydashboard)
header <- dashboardHeader(title = "Uploading Files")
sidebar <- dashboardSidebar(
  fileInput("file1", "Choose CSV File",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")),
  tags$hr(),
  checkboxInput("header", "Header", TRUE),
  radioButtons("sep", "Separator",
               choices = c(Comma = ",",
                           Semicolon = ";",
                           Tab = "\t"),
               selected = ","),
  radioButtons("quote", "Quote",
               choices = c(None = "",
                           "Double Quote" = '"',
                           "Single Quote" = "'"),
               selected = '"'),
  
  # Horizontal line ----
  tags$hr(),
  radioButtons("disp", "Display",
               choices = c(Head = "head",
                           All = "all"),
               selected = "head")
)
body <- dashboardBody(tableOutput("contents"))
ui <- dashboardPage(header, sidebar, body)
server <- function(input, output) {
  
  output$contents <- renderTable({
    
    req(input$file1)
    
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
}
shinyApp(ui,server)

