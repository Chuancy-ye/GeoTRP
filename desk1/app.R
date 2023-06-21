

library(shiny)

library(CHNOSZ)
ui <- fluidPage(
  navbarPage(a(href="http://134.175.117.194/", target="_blank", tags$b("GeoCVP")),
             
             
             
             tabsetPanel(
               tabPanel("Diagram",
                        br(),
                        sidebarLayout(
                          sidebarPanel(
                            
                            ##sliderInput("range", h3("滑动改变 H2S 浓度 "),min = -4, max = -1.5, value = c(-4,-1.5)) ,
                            ##sliderInput("range", h3("Slider to change the  H2 "),min = -6, max = -1, value = c(-6,-1)) , 
                            ## sliderInput("range3", h3("改变系统反应温度 "),min = 0, max = 300, value = 300) , 
                            ## sliderInput("range4", h3("改变系统反应压力  "),min = 0, max = 800, value = 800) 
                            
                            
                            sliderInput("temperature", tags$small("t,°C"), 25, 1000, 300, 25),
                            sliderInput("pressure", tags$small("P, bar (0 for Psat)"),  0, 5000, 800, 100),
                            sliderInput("y.range", tags$small("log H2S"), -12, -1, c(-11,-2), 1),
                            sliderInput("x.range", tags$small("log H2"), -6, -1, c(-6, -1),1)
                          ),
                          mainPanel(
                            plotOutput("chnosz.diagram", height = "500px", width="500px"),
                            textOutput("name")
                          )
                        )
               ),
               tabPanel("Species references",
                        br() ,
                        h1("coming soon")
               ),
               
               tabPanel("Species properties",
                        br(),
                        fluidRow(
                          h1("coming soon")
                        )
               ),
               tabPanel("code",
                        h1("coming soon"))
             )
  )
)

# Define server logic
server <- function(input, output) {
  output$chnosz.diagram <- renderPlot({
    
    ##  basis("CHNOS+")
    ## species(ispecies <- info(c("glycinium", "glycine", "glycinate")))
    ## a <- affinity(pH=c(input$range[1], input$range[2]))
    ## e <- equilibrate(a)
    ## d <- diagram(e, alpha=TRUE, lwd=1)
    
    add.obigt("SUPCRT92") 
    basis(c("Fe", "H2", "H2S", "H2O"), c("cr", "aq", "aq", "liq")) 
    species(c("hematite", "pyrite", "pyrrhotite", "magnetite")) 
    a <- affinity(H2S = c(input$ y.range[1], input$ y.range[2]), H2 = c(input$ x.range[1],input$ x.range[2]), T = input$ temperature, P = input$ pressure) 
    diagram(a, fill = "terrain") 
  })
  output$name <-renderText({
    paste('temperature is ',input$temperature,"°C","pressure is",input$pressure,"bar")
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
