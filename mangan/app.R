library(shiny)

library(CHNOSZ)
ui <- fluidPage(
  navbarPage(a(href="http://134.175.117.194/", target="_blank", tags$b("GeoCVP")),
             
             tabsetPanel(
               tabPanel("Diagram",
                        br(),
                        sidebarLayout(
                          sidebarPanel( 
                            sliderInput("temperature", tags$small("t,°C"), -100, 100, 25,1),
                            sliderInput("pressure", tags$small("P, bar"),  0.5,2, 1,0.1),
                            sliderInput("y.range", tags$small("Eh"), -0.8, 1, c(-0.8,1), 0.1),
                            sliderInput("x.range", tags$small("pH"), 0, 14, c(1, 14),0.1)
                          ),
                          mainPanel(
                            plotOutput("chnosz.diagram", height = "500px", width="500px"),
                            textOutput("name")
                          ))),
               tabPanel("Species references",
                        br() ,
                        h5("Hydrothermal solubility of rhodochrosite, Mn ( II) speciation, and equilibrium constants
                             ——OLAF WOLFRAM and RALF E. KRUPP(1996)")
                        ),
               tabPanel("Species properties",
                        br(),
                        fluidRow(
                          h1("coming soon"))
                        ),
               tabPanel("code",
                        h5(
                          sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
                choices = c("code", "datebase", "diagram")),
    downloadButton('downloadData', 'Download')
    
  )))
                        )
             )
              )

# Define server logic
server <- function(input, output) {
 output$chnosz.diagram <- renderPlot({
res <-300
T <- input$temperature
P <- input$pressure
blend <- TRUE
basis(c("Mn+2","H2O","H2S","CO3-2","Cl-","H+","e-"))

basis(c("H2S"),c(-3))

iaq <- retrieve("Mn", c("S","C","O","H","Cl"),"aq")
icr <- retrieve("Mn", c("S","C","O","H","Cl"), "cr")
species(c(iaq, icr))
species(1:length(iaq), -10)
bases <- c("H2S","SO4-2", "HSO3-")

m <- mosaic(bases, pH = c(input$x.range[1],input$x.range[2], res), Eh = c(input$y.range[1],input$y.range[2], res), T = T,P = 1,blend = blend)
fill <- rev(heat.colors(nrow(species())))
fill[which(species()$state == "cr")] <- "slateblue3"
m$A.species$species$name <- gsub(",alpha", "  ", m$A.species$species$name)
diagram(m$A.species, fill = fill)
dprop <- describe.property(c("T", "P"), c(T, P))
legend("bottomleft", legend = dprop, bty = "n")
t1 <- quote("Mn-S-C-Cl-O-H")
t2 <- "Unchecked phase diagram 2019.9.8"
mtitle(as.expression(c(t1, t2)), cex = 0.95)
    }
 )
 
     }

# Run the application 
shinyApp(ui = ui, server = server)
