library(shiny)
library(shinyBS)
library(imager)
library(colocr)
#library(dplyr)
library(qpcR)

source('img_func-MOD.R')
source('qpcr_func.R')

library(shiny)

if(!require(shinysense)){
  devtools::install_github("Aciole-David/shinysense")
  #library(shinysense)
}

library(shinyscreenshot)
library(tidyverse)
library(shinyjs)
if(!require(r2d3)){
  devtools::install_github("rstudio/r2d3")
  library(r2d3)
}
library(xfun)


# Define UI for application that draws a histogram
ui <-  fluidPage(
  h3("EasyOmics qLAMP", span("", style = "font-weight: 300"),
  style = "color: #000000; text-align: center;background-color:#00AD87;padding: 10px"),
  column(6,p("A tool for realtime colorimetric quantitative LAMP analysis from timelapse images of reaction chambers."),
         br()),
  ## Main
  fluidRow( ),
  # Tabs
  tabsetPanel(
    tabPanel('Captures',
           column(width = 3, h3("Camera"),
           shinyviewr_UI("myCamera", height = '100%'),
           numericInput(inputId = "NUMCAP", label = "# of captures", value = 5),
           numericInput(inputId = "INTECAP", label = "Capture delay [seconds]", value = 1),
           verbatimTextOutput("status"),
           actionButton("setconfirm","Confirm changes"),
           actionButton("resetdefault","Reset to default"),
           textOutput("answer"),
    ),
    column(width = 4, offset = -1,  h3('Last capture'),
           imageOutput("snapshot", width = 720, height = 480),
           imageOutput("myImage")
    )
             
             
             ),
    ## Select ROI
    tabPanel('Image Processing',
             sidebarLayout(
               sidebarPanel(
                 tags$br(),
                 
                 
                 fileInput('image1', 'UPLOAD IMAGES', multiple = TRUE),
                 bsTooltip('image1',
                           'Select and upload the chamber images.',
                           'right', options = list(container = "body")),
                 tags$hr(),
                 
                 tags$p('Adjust parameters to remove the background,'),
                 sliderInput('threshold', 'Threshold', 1, 99, 29, 1),
                 bsTooltip('threshold',
                           'Choose a threshold for excluding the background,',
                           'right', options = list(container = "body")),
                 
                 
                 sliderInput('shrink', 'Shrink', 1, 10, 1, 1),
                 bsTooltip('shrink',
                           'Shrink the selected area by eroding the bounderies around it.',
                           'right', options = list(container = "body")),
                 
                 sliderInput('grow', 'Grow', 1, 10, 1, 1),
                 bsTooltip('grow',
                           'Grow the selected area by dilating the bounderies around it.',
                           'right', options = list(container = "body"))),
               mainPanel(
                 plotOutput("image_plot", height = "3000px"),
                 tags$h4('Hue value table'),
                 tableOutput('table')
                 
               ))),
    
    ## Realamp 
    
    tabPanel('Data Processing',
             sidebarLayout(
               sidebarPanel(
                 tags$br(),
                 radioButtons('curve', 'MODE OF ANALYSIS', choices = c("Standard Calibration Curve","Quantification of Real Sample")),
                 tags$hr(),
                 
                 numericInput('dil', 'Lowest concentration', 1,min = 0, max = NA, step = NA ),
                 numericInput('dil_factor', 'Dilution factor', 10,min = 0, max = NA, step = NA),
                 tags$hr(),
                 
                 
                 fileInput('StdCurve', 'Upload the Calibration Curve',
                           multiple = FALSE)),
               
               mainPanel(
                 fluidRow(
                   plotOutput('graph_plot', height = "700px"),
                   tableOutput('table2'),
                   tags$br(),
                   
                   downloadButton("qLAMP.csv", "Save as Calibration Curve")
                 ))))
    
    
    
    
  ))


# Define server
server <- function(input, output, session) {
  # intiate interactive values
  values <- reactiveValues()
  
  # load images
  img1 <- reactive({
    image_load(input$image1$datapath[order(input$image1$name, decreasing = FALSE)])
    
  })
  
  ## calculate the pixset
  px <- reactive({
    roi_select(img1(),
               threshold = input$threshold,
               shrink = input$shrink,
               grow = input$grow)
  })
  
  ## calculate correlation
  b1 <- reactive({
    roi_check(px())  
  })
  
  # Output Views
  ## Select ROI
  
  # plots
  output$image_plot <- renderPlot({
    req(input$image1)
    n <- length(input$image1$name)/2
    par(mfrow=c(n+1,4), mar = rep(1, 4))
    roi_show(px())
  })
  
  s1 <- reactive({
    read.csv(input$StdCurve$datapath)
  })
  dil1 <- reactive({
    c(input$dil,
      input$dil*input$dil_factor^1,
      input$dil*input$dil_factor^2,
      input$dil*input$dil_factor^3,
      input$dil*input$dil_factor^4,
      input$dil*input$dil_factor^5)
  })
  
  ## sigmoid plot
  output$graph_plot <- renderPlot({
    req(input$image1)
    par(mfrow=c(2,1))
    rlamp(df = b1(), standardcurve = input$curve, df2 = s1(), dil2 = dil1(), thr_2 = input$thr_1, thr_01 = input$thr_0)
    
  })
  ## G/R ratio table
  output$table <- renderTable({
    req(input$image1)
    b1()
  })
  output$table2 <- renderTable({
    req(input$image1)
    rlamp(df = b1(), standardcurve = input$curve, df2 = s1(), dil2 = dil1(), thr_2 = input$thr_1, thr_01 = input$thr_0)
  })
  # download plot
  
  # download table
  output$qLAMP.csv <- downloadHandler(
    #specify the filename
    filename = function() {
      paste("qLAMP", ".csv", sep = "")
    },
    
    content = function(file) {
      req(input$image1)
      write.csv(b1(), file, row.names = FALSE)
    })
  
reset.default<-function() {  
    ##set parameters in main.js to default values
    gsub_file(file=system.file("r2d3/viewr/main.js",package = "shinysense"),
              "NumeroDeCapturas = .*,","NumeroDeCapturas = 5,")
    gsub_file(file=system.file("r2d3/viewr/main.js",package = "shinysense"),
              "IntervaloDeCaptura = .*;","IntervaloDeCaptura = 1000;")
    }
  
  ## update parameters when input change
  reset.vars<-function(){
    num.cap=({input$NUMCAP})
    gsub_file(file=system.file("r2d3/viewr/main.js",package = "shinysense"),
              "NumeroDeCapturas = .*,",paste0("NumeroDeCapturas = ",num.cap,","))
    inte.cap.i=input$INTECAP #input interval
    inte.cap=inte.cap.i*1000 #multiply to get input in milisseconds
    gsub_file(file=system.file("r2d3/viewr/main.js",package = "shinysense"),
              "IntervaloDeCaptura = .*;",paste0("IntervaloDeCaptura = ",inte.cap,";"))
  }
  
  status <- reactiveVal()
  
  out <- eventReactive(input$setconfirm,{
    status("Values updated")
    reset.vars()
    session$reload()
    sprintf(paste0(input$NUMCAP, " captures ;",input$INTECAP," seconds delay"))
    
  })
  
  observeEvent({list(input$NUMCAP,input$INTECAP)},
               {status("Please confirm parameters and wait \npage load before any capture")})
  
  output$answer <- renderText({out()})
  output$status <- renderText({status()})
  
  ##start with blank capture to allow several captures
  ##without this, captures stop prematurely
  screenshot(id = "snapshot", filename = "blank", download = F, server_dir = ".")
  
  #server side call of the viewr module
  myCamera <- callModule(
    shinyviewr, "myCamera",
    output_width = 500, 
    output_height = 400 )
  
  #picture taken
  output$snapshot <- renderPlot({
    par(mar = c(0, 0, 0, 0))
    plot(as.raster(myCamera()))
    a=sprintf(format(Sys.time(), '%Y-%m-%d_%H-%M-%S'))
    
    ##save to user download folder
    #screenshot(id = "snapshot", filename =
    #             paste("beromics-capture-",a, sep = ""),timer = .5)
    
    ##save to server side folder
    screenshot(id = "snapshot", filename =
                 paste("beromics-capture-",a, sep = ""),timer = .5,
               server_dir = "./", download = F)
    
  })
  
   observeEvent(input$resetdefault, {
     reset.default()
     updateNumericInput(session, "NUMCAP", value = 5)
     updateNumericInput(session, "INTECAP", value = 1)
   })
  
  
  }

# Run the application
shinyApp(ui = ui, server = server)
