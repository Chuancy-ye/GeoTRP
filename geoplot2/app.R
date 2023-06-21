#geoplot2 demo
library(shiny)
library(shinydashboard)
library(readxl)
library(plot3D)
library(rgl)
#file1 <- read_excel("C:/Users/Chauncy ye/AnacondaProjects/KMeans_AP聚类 2.0/cluster_analysis/zhuliang3000.xlsx")

file1 <- read_excel("zhuliang3000.xlsx")
header <- dashboardHeader(title = "3Dplot for Carbonate")
sidebar <- dashboardSidebar(
  
  
  uiOutput('vx',width = "400px"),
  br(),
  # Horizontal line ----
  tags$hr(),
  
  sliderInput("size1", "Pointsize",min = 0, max = 10, value = 2.5,step =0.1),
  radioButtons("disp", "Displaydata",
               choices = c(Head = "head",
                           All = "all",
                           demo = " demo"),
               
               selected = "head"),
  radioButtons("disp2", "Displaystyle",
               choices = c(
                 all ="all",   
                 withoutBOX = "BOX",
                 withoutAXES = "AXES"),
               selected = "all"),
  submitButton(text = "Apply Changes", icon = NULL, width = NULL),
  br()
)

body <- dashboardBody(helpText( a("Click Here to back", href="http://182.92.182.119/geoplot")),
                      tableOutput("contents"),
                      verbatimTextOutput("value"),
                      rglwidgetOutput('rglplot'),
                      br(),
                      p("Made using",
                        a(href = "http://shiny.rstudio.com/", target = "_blank", "Shiny"),
                        paste0(as.character(packageVersion("shiny")), ", and"),
                        a(href = "https://www.r-project.org/", target = "_blank", "R"),
                        paste0(as.character(getRversion()),".")
                      ),
                      p(" Revised by Xiangying Ye in August 2020")
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output) {
  
  varis <- reactive({
    
    af <- colnames(file1)
  })
  
  output$vx <- renderUI({
    
    checkboxGroupInput("inp", "Choose 3 columns of the data",choices=1:length(varis()),
                       selected = c(4,2,3))
  })
  
  
  output$contents <- renderTable({
    
    
    df <- file1
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
    
  })
  output$rglplot <- renderRglwidget({
    
    req(input$inp)
    req(varis())
    zhuliang3000 <- file1
    a <- zhuliang3000[as.numeric(input$inp[1])]
    b <- zhuliang3000[as.numeric(input$inp[2])]
    c <- zhuliang3000[as.numeric(input$inp[3])]
    #df = data.frame(CAO,NA2O,AL2O3)
    af = data.frame(a,b,c)
    b1 = zhuliang3000[1]
    c1 = as.matrix(b1)
    if (input$disp2 == "BOX" ){
      plot3d(af, col= as.numeric(c1+3),size=input$size1,box =FALSE, axes = TRUE)
    }else if(input$disp2 == "AXES"){
      plot3d(af, col= as.numeric(c1+3),size=input$size1,box =TRUE, axes = FALSE)
    }else{
      plot3d(af, col= as.numeric(c1+3),size=input$size1,box =TRUE, axes = TRUE)
      
    }
    par3d(family = "sans", cex = 1.3,"activeSubscene")
    rglwidget()
  })
  
  output$value <- renderText({ 
    
    req(input$inp)
    df <- file1
    
    paste("You chose", 
          colnames(df)[as.numeric(input$inp[1])],
          colnames(df)[as.numeric(input$inp[2])],
          colnames(df)[as.numeric(input$inp[3])])
    
    
    
  }
  )
  
}

shinyApp(ui,server)