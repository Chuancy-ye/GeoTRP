library(shiny)
library(CHNOSZ)
ui <- fixedPage(
   
   headerPanel("测试代码"),
   h4("反应条件改变窗口"),
   sidebarPanel(   
      sliderInput("range", h3("滑动改变 H2S 浓度 "),min = -4, max = -1.5, value = c(-4,-1.5)) ,
      ##sliderInput("range", h3("Slider to change the  H2 "),min = -6, max = -1, value = c(-6,-1)) , 
      sliderInput("range3", h3("改变系统反应温度 "),min = 0, max = 300, value = 300) , 
      sliderInput("range4", h3("改变系统反应压力  "),min = 0, max = 800, value = 800) 
   ),        
   
   
   mainPanel(
      titlePanel(span("Fe-O-H-S反应体系",style="color:red")),
      plotOutput('pl', width='70%', height='501px'), 
      h4(span("pyrrhotite=磁黄铁矿，pyrrhotite=赤铁矿，pyrite=黄铁矿，hematite=磁铁矿", style = "color:blue")) , 
      br(),         
      h3(  textOutput("min_max")),
      h3(  textOutput("T_P")  ) 
      
   )
   
)

server <- function(input, output) {

  output$pl <- renderPlot({

  ##  basis("CHNOS+")
   ## species(ispecies <- info(c("glycinium", "glycine", "glycinate")))
   ## a <- affinity(pH=c(input$range[1], input$range[2]))
   ## e <- equilibrate(a)
   ## d <- diagram(e, alpha=TRUE, lwd=1)
       
     add.obigt("SUPCRT92") 
     basis(c("Fe", "H2", "H2S", "H2O"), c("cr", "aq", "aq", "liq")) 
     species(c("hematite", "pyrite", "pyrrhotite", "magnetite")) 
     a <- affinity(H2S = c(input$ range[1], input$ range[2]), H2 = c(-6, -1), T = input$ range3, P = input$ range4) 
     diagram(a, fill = "terrain") 
      
                
   
                      
  })
  
  output$min_max <- renderText({ 
   paste("你改变的 loga(h2s)值范围=" ,
         input$range[1], "to", input$range[2] )
     })
     
   output$T_P <- renderText({ 
      
   paste( " 你选择的温度为" ,
         input$range3 ,"摄氏度", ",压力为",input$range4,"bar")
      })      
} 
