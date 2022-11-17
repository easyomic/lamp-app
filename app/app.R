## app-test-save-image-capture
#save as png
#do not plot in screen
#use back camera as default
#no screenshot
#javascrip loop inside app.R, not in main.js
#main.js add shutter attr id, to look for button id
#get button from mycamera div + button id from shadowDOM

library(shiny)
library(shinyBS)
library(imager)
library(colocr)
#library(dplyr)
library(qpcR)

source('source-function.R')
source('img_func-MOD.R')
source('qpcr_func.R')

library(shiny)

# if(!require(shinysense)){
#   devtools::install_github("Aciole-David/shinysense")
#   #library(shinysense)
# }

library(shinyscreenshot)
library(tidyverse)
library(shinyjs)
if(!require(r2d3)){
  devtools::install_github("rstudio/r2d3")
  library(r2d3)
}
library(xfun)
library("gridGraphics") #to rotate plots
#library(shinylogs)

library(TeachingDemos)

# Define UI for application that draws a histogram
ui <-  fluidPage(
  useShinyjs(),
  #use_tracking(), #needed by shinylogs
  h5("EasyOmics qLAMP", span("", style = "color:red;font-weight: 200"),
     style = "color: #000000; text-align: center;background-color:#a476cf;padding: 10px"),
  column(12,h6("A tool for realtime colorimetric quantitative LAMP analysis from timelapse images of reaction chambers."),
  ),
  ## Main
  fluidRow( ),
  # Tabs
  tabsetPanel(
    tabPanel('Captures',
             column(width = 6, align="center",
                    shinyviewr_UI("myCamera", height = '100%'),
                    #tags$hr(),
                    splitLayout(
                      numericInput(inputId="NUMCAP",width = '45%',
                                   label="Total captures",value=1),
                      numericInput(inputId="INTECAP",width='45%',
                                   label="Delay [seconds]",value = 1)
                    ),
                    verbatimTextOutput("status"),
                    splitLayout(
                      actionButton("setconfirm","Confirm"),
                      actionButton("resetdefault","Reset"),
                      actionButton("capture","Capture!")
                    ),
                    textOutput("answer"),
                    #automatic download link
                    downloadLink("downloadData",label=""),
                    #automatic download logic
                    tags$head(tags$script(HTML('
                             Shiny.addCustomMessageHandler("jsCode",
                             function(message) {
                             eval(message.value);
                             });'
                    ))),
             ),
             splitLayout(
             column(width = 6, offset = 0, align="center",
                    imageOutput("snapshot", width = 450, height = 600)
                    #plotOutput("snapshot", width = 600, height = 480)
                    #uiOutput("snapshot", width = 344, height = 300)
             ))
             
             
    ),
    ## Select ROI
    tabPanel('Image Processing',
             sidebarLayout(
               sidebarPanel(
                 tags$br(),
                 
                 
                 fileInput('image1', 'UPLOAD IMAGES', multiple = TRUE,
                           capture='environment'),
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
                   #plotOutput('empty_plot', width = '30%', height = '150px'),
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
    #dev.off()
    #plot(1:10)
    # TeachingDemos::subplot(
    #   plot(1:10, col=5, pch='.', cex.axis=0.00001,xaxt='n', yaxt='n')+
    #     legend("topleft",
    #        legend =c('Amostras','NC', '#2', '#3', '#4', '#5', '#6', '#7'),
    #        pch=16, pt.cex=1, cex=1, bty='n',
    #        col = c('#FFFFFFFF', 'black','#df536b', '#61d04f', '#2297e6', '#28e2e5', '#cd0bbc', '#f5c710')),
    #   x=grconvertX(c(0.85,1), from='npc'), #position and size
    #   y=grconvertY(c(0.5,1), from='npc'), #position and size
    #   type='fig', pars=list( mar=c(0,0,0,0)+0.1) )
    
  })
  
  # ## Make dummy legend to curve plot
  # output$empty_plot <- renderPlot({
  #   par(mar=c(0,0,0,0))
  #   plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='ylab',xlab='xlab', xlim=0:1, ylim=0:1)
  #   legend("topleft",
  #          legend =c('Amostras','NC', '#2', '#3', '#4', '#5', '#6', '#7'),
  #          pch=16, pt.cex=1, cex=1, bty='n',
  #          col = c('#FFFFFFFF', 'black','#df536b', '#61d04f', '#2297e6', '#28e2e5', '#cd0bbc', '#f5c710'))
  #   mtext("Amostras", at=.05, cex=1)
  # })
  
  
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
  
  # reset.default<-function() {  
  #     ##set parameters in main-test-back-camera.js to default values
  #     gsub_file("main-test-back-camera.js",
  #               "NumeroDeCapturas = .*,","NumeroDeCapturas = 5,")
  #     gsub_file("main-test-back-camera.js",
  #               "IntervaloDeCaptura = .*;","IntervaloDeCaptura = 1000;")
  #     }
  
  ## update parameters when input change
  # reset.vars<-function(){
  #   num.cap=({input$NUMCAP})
  #   #     gsub_file("main-test-back-camera.js",
  #   #               "NumeroDeCapturas = .*,",paste0("NumeroDeCapturas = ",num.cap,","))
  #   inte.cap.i=input$INTECAP #input interval
  #   inte.cap=inte.cap.i*1000 #multiply to get input in milisseconds
  #   #     gsub_file("main-test-back-camera.js",
  #   #               "IntervaloDeCaptura = .*;",paste0("IntervaloDeCaptura = ",inte.cap,";"))
  # }
  
  status <- reactiveVal()
  
  out <- eventReactive(input$setconfirm,{
    status("Values updated")
    #reset.vars()
    #session$reload()
    sprintf(paste0(input$NUMCAP, " captures; ",input$INTECAP," seconds delay"))
    
  })
  
  observeEvent({list(input$NUMCAP,input$INTECAP)},
               {status("Confirm parameters before any capture")})
  
  output$answer <- renderText({out()})
  output$status <- renderText({status()})
  
  ##start with blank capture to allow several captures
  ##without this, captures stop prematurely
  screenshot(id = "snapshot", filename = "blank", download = F, server_dir = ".")
  
  #server side call of the viewr module
  myCamera <- callModule(
    shinyviewr, "myCamera",
    output_width = 450,
    output_height = 600)
  
  #picture taken
  observeEvent(myCamera(), {
    photo <- myCamera() 
    a=sprintf(format(Sys.time(), '%Y-%m-%d_%H-%M-%S'))
    capturename=paste("beeromics-capture-",a, sep = "")
    #png(filename=capturename,
    #   width = 720, height = 205)
    #par(mar=c(0,0,0,0))
    #plot(as.raster(photo)) # plot photo
    #dev.off()
    
    
    output$snapshot <- renderPlot({
      par(mar=c(0,0,0,0))
      plot(photo)
      screenshot(id = 'snapshot', filename = capturename,
                 download = T, server_dir = ".", timer = 1.2)
    })
  })  
  
  #automatic download
  # observeEvent(input$my_own_trigger, {
  #   output$downloadData<<-downloadHandler(filename = capturename,content = function(file)file.copy(file0,file) )
  #   jsinject <- "setTimeout(function(){window.open($('#downloadData').attr('href'))}, 100);"
  #   session$sendCustomMessage(type = 'jsCode', list(value = jsinject))    
  # })
  
  
  observeEvent(input$resetdefault, {
    #reset.default()
    updateNumericInput(session, "NUMCAP", value = 5)
    updateNumericInput(session, "INTECAP", value = 1)
  })
  
  
  #runjs("var today = new Date(); alert(today);")
  
  ##simulate click and repeat in a loop
  simulateclick<-function(){
    runjs(paste0("console.log('",status(),"'); var confirmation = '",status(),"'; if (confirmation === 'Values updated') { var i = 1; function myLoop() { setTimeout(function() { console.log(\'cheese\'+i); document.getElementById(\'myCamera-shinyviewr\').shadowRoot.querySelector(\'#shutterbutton\').click(); i++; if (i <=",input$NUMCAP,") { myLoop(); } },", input$INTECAP*1000,") } myLoop() } ; "))
    a=sprintf(format(Sys.time(), '%Y-%m-%d_%H-%M-%S'))
    captname=paste("beromics-capture-",a, sep = "")
    
  }
  
  #simulate click
  observeEvent(input$capture, {
    simulateclick()
    timestamp()
  })
  
  ## SHINYLOGS
  # Just take a look at what is generated
  #track_usage(what = "error",
  #           storage_mode = store_json(path = "logs/"))
  # track_usage(
  #   storage_mode = store_custom(FUN = function(logs){
  #     str(logs, max.level=3)
  #   })
  # )
 
  runjs('const myshadowRoot = document.getElementById(\'myCamera-shinyviewr\').shadowRoot;')
}
# Run the application
shinyApp(ui = ui, server = server)
