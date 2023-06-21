library(shiny)

ui <- fixedPage(
       
     
     sidebarPanel(   
          
    ),        
            
          
     mainPanel(
         
         plotOutput('pl', width='70%', height='501px'), 
         
         br(),         
         h3(  textOutput("min_max")),
         h3(  textOutput("T_P")  ) 
                     
         )
       
    )
