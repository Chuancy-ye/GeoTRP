# app.R: a simple mockup for inputting diagram type and options
# v2.0 (20191006): initial version
#Tin
library(shiny)
library(CHNOSZ)
ui <- fluidPage(
 

  navbarPage(a(href="http://134.175.117.194/", tags$div("GeoTVP",style="font-family: 'Lobster', cursive;
  font-weight: 500;color: #ad1d28;")),
             
    tabsetPanel( 
    tabPanel("Diagram",
    br(),
    sidebarLayout(
    sidebarPanel(
    helpText("Geochemical Thermodynamics using R, CHNOSZ, and Shiny"),
    selectInput(inputId ="Tin", "Tin", list("base" = "Sn", "other" = "add")),
   
    sliderInput("temperature", HTML("Temperature (&deg;C)"),
                min = 15, max = 300, value = 25, step = 5),
    sliderInput("concentration", HTML("concentration (Sn2+)"),
                min = -6, max = -1, value = -4, step = 1),
    checkboxInput("addS", "add S", FALSE),
    checkboxInput("addC", "add C", FALSE),
    
    hr(),
    submitButton(text = "Apply Changes", icon = NULL, width = NULL)
    
  ),
  
  mainPanel(
    p(plotOutput("chnosz.diagram", height = "500px", width="500px")),
    textOutput("text1"),
    textOutput("text2"),
    hr(),
    p("Made using",
      a(href = "http://chnosz.net", target = "_blank", "CHNOSZ"),
      paste0(as.character(packageVersion("CHNOSZ")), ","),
      a(href = "http://shiny.rstudio.com/", target = "_blank", "Shiny"),
      paste0(as.character(packageVersion("shiny")), ", and"),
      a(href = "https://www.r-project.org/", target = "_blank", "R"),
      paste0(as.character(getRversion()), ".")
    )
    )
  
 )
),

    tabPanel("Species references",
    br() ,
    tableOutput("references")),

   tabPanel("Species properties",
            HTML("This table lists values of &Delta;G&deg;<sub><i>f</i></sub> in J/mol"),
            tableOutput("deltaG")
   ),
   tabPanel("code",
    sidebarPanel(
    selectInput("dataset", "Choose a dataset:", 
    choices = c("code", "datebase", "diagram")),
    downloadButton('downloadData', 'Download')
    )))))



function1 <- function(input,temp,conc,return.ispecies =FALSE){
  
  res <-500
  T=temp
  P <- 1
  c <- conc
  if(input$addC&input$addS)
  {
    pH <- c(0, 14, 500)
    Eh <- c(-1, 1, 500)
    T=25
    P=1
    basis(c("Sn", "SO4-2", "H2O", "H+", "e-", "CO3-2"))
    basis("SO4-2", c(-6))
    basis("CO3-2", c(0))
    species(c("Sn+2"),c)
    iaq <- retrieve("Sn",c("O","H","C","S"),"aq")
    icr <- retrieve("Sn", c("O", "H","C","S"), "cr")
    species( c(iaq,icr))
    if(return.ispecies)return(species()$ispecies)
    # two sets of changing basis species:
    # speciate SO4-2, HSO4-, HS-, H2S as a function of Eh and pH
    # speciate CO3-2, HCO3-, CO2 as a function of pH
    bases <- c("SO4-2", "HSO4-", "HS-", "H2S")
    bases2 <- c("CO3-2", "HCO3-", "CO2")
    # calculate affinities using the relative abundances of different basis species
    # (using default blend = TRUE)
    # note curved lines, particularly at the boundaries with siderite
    m1 <- mosaic(bases, bases2, pH = pH, Eh = Eh, T = T,P = 1)
    # make a diagram and add water stability lines
    diagram(m1$A.species, lwd = 2)
    water.lines(m1$A.species, col = "seagreen", lwd = 1.5)
    # show lines for Mn(aq) = 10^-4 M
    species(c("Sn+2"),c)
    m2 <- mosaic(bases, bases2, pH = pH, Eh = Eh, T =T,P = 1)
    diagram(m2$A.species, add = TRUE, names = NULL)
    # overlay the sulfur and carbonate basis species predominance fields
    d <- diagram(m1$A.bases, add = TRUE, col = "red", col.names = "red", lty = 3, limit.water = FALSE)
    d <- diagram(m1$A.bases2, add = TRUE, col = "blue", names = NULL, lty = 3, limit.water = FALSE)
    text(d$namesx, -0.8, as.expression(sapply(m1$A.bases2$species$name, expr.species)), col = "blue")
    # add legend and title
    dP <- describe.property(c("T", "P"), c(T, 1))
    legend("top", dP, bty = "n")
    dS <- expression(sum(S)*"(aq)" == 10^-6~italic(m))
    dC <- expression(sum(C)*"(aq)" == 1~italic(m))
    legend("topright", c(dS, dC), bty = "n")
    t1<-"Sn-C-S-O-H"
    t2 <- "Unchecked phase diagram 2019.9.12"
    mtitle(as.expression(c(t1,t2)), cex = 0.95)
    
    # reset the database, as it was changed in this example
    reset()
  }
  else if(input$addC){
    basis(c("SnO2","H2O","H+","e-","CO2","H2S"))
    basis("CO2",c(-3))
    iaq <- retrieve("Sn",c("O","H","C"),"aq")
    icr <- retrieve("Sn", c("O", "H","C"), "cr")
    species(c("Sn+2"),c)
    species( c(iaq,icr))
    if(return.ispecies)return(species()$ispecies)
    # set activities of aqueous species
    species(1:length(iaq), -6)
    bases <-c("CO2","CO3-2","HCO3-")
    m <- mosaic(bases,pH = c(0, 14,res), Eh = c(-0.8, 1.2,res),T=T,P=P)
    # adjust colors and names
    fill <- rev(heat.colors(nrow(species())))
    fill[which(species()$state == "cr")] <- "slategray3"
    m$A.species$species$name <- gsub(",alpha", "", m$A.species$species$name)
    # make the plot!
    diagram(m$A.species, fill = fill)
    dprop <- describe.property(c("T", "P"), c(T, P))
    #add legend and title
    legend("bottomleft", legend = dprop, bty = "n")
    t1<-"Sn-C-O-H"
    t2 <- "Unchecked phase diagram 2019.9.12"
    mtitle(as.expression(c(t1,t2)), cex = 0.95)
    reset()
  }else if (input$addS){
    basis(c("SnO2","H2O","H+","e-","CO2","H2S"))
    basis("H2S",c(-3))
    species(c("Sn+2"),c)
    iaq <- retrieve("Sn",c("O","H","S"),"aq")
    icr <- retrieve("Sn", c("O", "H","S"), "cr")
    species( c(iaq,icr))
    if(return.ispecies)return(species()$ispecies)
    # set activities of aqueous species
    species(1:length(iaq), -6)
    bases <-c("H2S","HS-","SO4-2","HSO4-")
    m <- mosaic(bases,pH = c(0, 14,res), Eh = c(-0.8, 1.2,res),T=T,P=1)
    fill <- rev(heat.colors(nrow(species())))
    fill[which(species()$state == "cr")] <- "slategray3"
    m$A.species$species$name <- gsub(",alpha", "", m$A.species$species$name)
    # make the plot!
    diagram(m$A.species, fill = fill)
    dprop <- describe.property(c("T", "P"), c(T, P))
    legend("bottomleft", legend = dprop, bty = "n")
    t1<-"Sn-S-O-H"
    t2 <- "Unchecked phase diagram 2019.9.12"
    mtitle(as.expression(c(t1,t2)), cex = 0.95)
    
    reset()
  }else{
    
    #an Eh-pH diagram for Cu-bearing aqueous species
    basis(c("SnO2","H2O","H+","e-"))
    species(c("Sn+2"),c)
    iaq <- retrieve("Sn",c("O","H"),"aq")
 ##  icr <- retrieve("Sn",c("O","H"),"cr")
    species(c(iaq))
    if(return.ispecies)return(species()$ispecies)
    a <- affinity(pH = c(0, 14,res), Eh = c(-0.8, 1.2,res),T=T,P=P)
    diagram(a,fill="terrain")
    
    dprop <- describe.property(c("T", "P"), c(T, P))
    legend("bottomleft", legend = dprop, bty = "n")
    t1<-"Sn-O-H"
    t2 <- "Unchecked phase diagram 2019.9.12"
    mtitle(as.expression(c(t1,t2)), cex = 0.95)
    reset()
}
  
}

server <- function(input,output){
  
  output$text1 <- renderText({
    text <- "-O-H"
    if(input$addC)
    {text <- paste0("-C", text)
    } 
    if(input$addS) text <- paste0("-S", text)
   
    paste0("The system is Sn", text, ".")
  })
  output$text2 <- renderText(paste0("The temperature is ", input$temperature, " °C."
                             ,"Logarithm of activity is 10^ " , input$concentration,"."))

  
  output$references <- renderTable({
    args <- list(input = input,temp =input$temperature,conc= input$concentration, return.ispecies = TRUE)
    ispecies <- do.call(function1, args)
    thermo.refs(ispecies)
  })
  
  output$deltaG <- renderTable({
    args <- list(input = input,temp =input$temperature,conc= input$concentration, return.ispecies = TRUE)
    ispecies <- do.call(function1, args)
    E.units("J")
    sout <- subcrt(ispecies, property = "G", T = input$temperature)$out
    deltaG <- data.frame(species = names(sout), Delta_G = unlist(sout))
    deltaG
  })
  output$chnosz.diagram <- renderPlot({
  
    args <- list(input = input,temp =input$temperature,conc= input$concentration)
    
   
   do.call(function1, args)
  
  })
}

shinyApp(ui = ui,server = server)
