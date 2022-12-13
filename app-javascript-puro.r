## app-test-save-image-capture
#save as png
#do not plot in screen
#use back camera as default
#no screenshot
#javascrip loop inside app.R, not in main.js
#main.js add shutter attr id, to look for button id
#get button from mycamera div + button id from shadowDOM
library(shinyFiles)


library(shiny)
library(shinyBS)
library(imager)
library(colocr)
#library(dplyr)
library(qpcR)

#source('source-function.R')
source('functions/img_func-MOD.R')
source('functions/qpcr_func.R')

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
    tabPanel('Capturas',
             column(width = 6, align="center",
                    splitLayout(
                      numericInput(inputId="NUMCAP",width = '60%',
                                   label="Total captures",value=1),
                      numericInput(inputId="INTECAP",width='60%',
                                   label="Delay [seconds]",value = 1)
                    ),
                    verbatimTextOutput("status"),
                    splitLayout(
                      actionButton('startbutton','Start!'),
                      actionButton("setconfirm","Confirm"),
                      actionButton("resetdefault","Reset"),
                      actionButton("capturebutton","Capture!"),
                    ),
                    actionButton('hiddencapturebutton','hc', width = 0),
                    textOutput("answer"),
                    HTML(
                      '
    <div style="width: 100%">
    
    <input type="range" id="zoom-slider" min="0" , max="0">
    <div style="display:inline-block;vertical-align:center;">
    <div>Zoom: </div>
    </div>
    <div style="display:inline-block;">
    <output id="zoom-slider-value"></output>
    </div>
        
        </div>
        <p><button hidden id="start" onclick="start()">start camera!</button></p>
        <div>
        <div style="float:left;margin-right:5px;">
        <p><video id="video-tag" width=100% height=10% autoplay=true/></p>
        </div>
        <div style="float:left">
        <p><img id="image-tag" width=100%> </img></p>                             
        </div>
        </div>
        
        '
                    ),
                    tags$script(
                      HTML(
                        "
      const constraints =  { 'video': { width: 500, facingMode: \"environment\" }};
var videoTag = document.getElementById('video-tag');
var zoomSlider = document.getElementById('zoom-slider');
var zoomSliderValue = document.getElementById('zoom-slider-value');
var imageTag = document.getElementById('image-tag');
var imageCapturer;

function start() {
  navigator.mediaDevices.getUserMedia(constraints)
    .then(gotMedia)
    .catch(e => { console.error('getUserMedia() failed: ', e); });
}

function gotMedia(mediastream) {
  videoTag.srcObject = mediastream;
  document.getElementById('start').disabled = true;
  
  var videoTrack = mediastream.getVideoTracks()[0];
  imageCapturer = new ImageCapture(videoTrack);

  // Timeout needed in Chrome, see https://crbug.com/711524
  setTimeout(() => {
    const capabilities = videoTrack.getCapabilities()
    // Check whether zoom is supported or not.
    if (!capabilities.zoom) {
      return;
    }
    
    zoomSlider.min = capabilities.zoom.min;
    zoomSlider.max = capabilities.zoom.max;
    zoomSlider.step = capabilities.zoom.step;

    zoomSlider.value = zoomSliderValue.value = videoTrack.getSettings().zoom;
    zoomSliderValue.value = zoomSlider.value;
    
    zoomSlider.oninput = function() {
      zoomSliderValue.value = zoomSlider.value;
      videoTrack.applyConstraints({advanced : [{zoom: zoomSlider.value}] });
    }
  }, 500);
  
}

function takePhoto() {
  imageCapturer.takePhoto()
    .then((blob) => {
      console.log('Photo taken: ' + blob.type + ', ' + blob.size + 'B')
      imageTag.src = URL.createObjectURL(blob);
      
      //get blob content
      var reader = new FileReader();
      reader.onload = function() {
      console.log(reader.result);
      }
      reader.readAsDataURL(blob);
      //end get blob content
    })
    .catch((err) => { 
      console.error('takePhoto() failed: ', e);
    });
      
}
    // ### JPEG ###
    
    var width = 500; 
    var height = 500; 
    // Set the Width and Height
    
    var canvas = document.createElement('canvas');
    // Dynamically Create a Canvas Element
    
    canvas.width  = width;
    // Set the width of the Canvas
    
    canvas.height = height;
    // Set the height of the Canvas
    
    var ctx = canvas.getContext('2d');
    // Get the 'context' of the canvas 
    
    var img = document.getElementById('image-tag'); 
    // The id of your image container
    
    ctx.drawImage(img,0,0,width,height);
    // Draw your image to the canvas
    
    var jpegFile = canvas.toDataURL('image/jpeg');
    // This will save your image as a
    //jpeg file in the base64 format.



//start()
//gotMedia(mediastream)
//takePhoto()
      "
                      )
                      
                    )
                    ,
             ),
             # splitLayout(
             #   column(width = 6, offset = 0, align="center",
             #          imageOutput("snapshot", width = 450, height = 580)
             #          #plotOutput("snapshot", width = 600, height = 480)
             #          #uiOutput("snapshot", width = 344, height = 300)
             #   ))
             
             
    ),
    ## Select ROI
    tabPanel('Imagens',
             sidebarLayout(
               sidebarPanel(
                 tags$br(),
                 
                 shinyDirButton("dir", "Input directory", "Upload"),
                 verbatimTextOutput("dir", placeholder = TRUE),
                 
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
    
    tabPanel('Dados',
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
  shinyjs::hide('hiddencapturebutton')
  
  
  
  # intiate interactive values
  values <- reactiveValues()
  
  
  listfl<<-list()
  
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
  
  #reset default zoom
  reset.default<-function() {
    ##set parameters in main-test-back-camera.js to default values
    #gsub_file("main.js",
    #         "zoomvalue = .*;","zoomvalue = 1;")
  }
  
  # update zoom when input change
  reset.vars<-function(){
    # zoom.value=({input$ZOOMINPUT})
    # gsub_file("main.js",
    #           "zoomvalue = .*;",paste0("zoomvalue = ",zoom.value,";"))
  }
  
  status <- reactiveVal()
  
  out <- eventReactive(input$setconfirm,{
    status("Values updated")
    reset.vars()
    #session$reload()
    sprintf(paste0(input$NUMCAP, " captures; ",
                   input$INTECAP," seconds delay; "
    ))
    
  })
  
  observeEvent({list(input$NUMCAP,input$INTECAP)},
               {status("Confirm parameters before any capture")})
  
  output$answer <- renderText({out()})
  output$status <- renderText({status()})
  
  ##start with blank capture to allow several captures
  ##without this, captures stop prematurely
  #screenshot(id = "snapshot", filename = "blank", download = F, server_dir = ".")
  
  #server side call of the viewr module
  # myCamera <- callModule(
  #   shinyviewr, "myCamera",
  #   output_width = 450,
  #   output_height = 580)
  # 
  #picture taken
  # observeEvent(myCamera(), {
  #   photo <- myCamera() 
  #   a=sprintf(format(Sys.time(), '%Y-%m-%d_%H-%M-%S'))
  #   capturename=paste("beeromics-capture-",a, sep = "")
  #   #png(filename=capturename,
  #   #   width = 720, height = 205)
  #   #par(mar=c(0,0,0,0))
  #   #plot(as.raster(photo)) # plot photo
  #   #dev.off()
  #   
  #   
  #   output$snapshot <- renderPlot({
  #     par(mar=c(0,0,0,0))
  #     plot(photo)
  #     screenshot(id = 'snapshot', filename = capturename,
  #                download = T, server_dir = ".", timer = 1.2)
  #   })
  # })
  # 
  observeEvent(input$resetdefault, {
    reset.default()
    updateNumericInput(session, "NUMCAP", value = 1)
    updateNumericInput(session, "INTECAP", value = 1)
    
    #listfl<<-list.files(path=paste0('./',capturesfolder), pattern="*.png",
     #                  all.files=FALSE,full.names=T,include.dirs = T)

    #listfl
    
    
    
  })
  
  
  #runjs("var today = new Date(); alert(today);")
  
  ##simulate click and repeat in a loop
  simulateclick<-function(){
    timestamp()
    runjs(paste0("console.log('",status(),"'); var confirmation = '",status(),"'; if (confirmation === 'Values updated') { var i = 1; function myLoop() { setTimeout(function() { console.log(\'cheese\'+i); document.getElementById(\'hiddencapturebutton\').click(); i++; if (i <=",input$NUMCAP,") { myLoop(); } },", input$INTECAP*1000,") } myLoop() } ; "))
    
    
    
  }
  
  #simulate click
  observeEvent(input$capturebutton, {
    
    #timestamp()
    if (status() == 'Values updated') {
    a=sprintf(format(Sys.time(), '%Y-%m-%d_%H-%M-%S'))
    dirname=paste("folder-",a, sep = "")
    dir.create(dirname)
    capturesfolder<<-(paste0(dirname))
    file.create(paste0(capturesfolder,'/paths.tsv'))
    
    testmyfolder<<-as.character(capturesfolder)
    simulateclick()
    print(input$shinyscreenshot)
    
    
    
    
      #listfl = append(listfl,paste0(input$shinyscreenshot))
      #View(listfl)
      # listfl2<-list.files(path=paste0(testmyfolder),
      #                     pattern="*.png", all.files=FALSE,
      #                     full.names=T, include.dirs = T)
      # View(listfl2)
      # print(listfl2)

      
      #file.create(paste0(capturesfolder,'/paths.tsv'))
      #write_tsv(x = as.data.frame(input$shinyscreenshot),
                #file = paste0(capturesfolder,'/paths.tsv'), append = T)
      #df=df[nrow(df) + 1,] = c(value1, value2, ...)

    
    
    }
  })
  
  observeEvent(input$hiddencapturebutton, {
    #if (status == 'Values updated') {
    runjs('takePhoto()')
    a=sprintf(format(Sys.time(), '%Y-%m-%d_%H-%M-%S'))
    captname=paste("easyomics-",a, sep = "")
    screenshot(filename = captname,timer = 1, selector = "#image-tag", scale = 4)
    screenshot(filename = captname,timer = 1, selector = "#image-tag", scale = 4, server_dir = capturesfolder, download = F)
    
    timestamp()
    Sys.sleep(15)
    listfl<<-list.files(path=as.character(paste0(capturesfolder)),
                          pattern="*.png", all.files=FALSE,
                          full.names=T, include.dirs = T)
      
    View(listfl)
    print(listfl)
    timestamp()
    
    
    
    
    # captpath=gsub(x=paste0(input$shinyscreenshot),
    #      pattern = '/easyomics-.*',
    #      replacement = '')
    
    
    
    
    #img2 <- image_load(listfl)
    

    #send blob content to R
    runjs("
    function sleep (time) {
    return new Promise((resolve) => setTimeout(resolve, time));
    }
    // Usage!
    sleep(500).then(() => {
    // Do something after the sleep!
    
      var elements = document.getElementsByTagName('img');
      var sendToR = [];
      for (var nr = 0; nr < elements.length; nr++) {
          var newElement1 = {};
          var newElement2 = {};
          //newElement1 = elements[nr].id;
          newElement2 = elements[nr].src;
          //sendToR.push(newElement1);
          sendToR.push(newElement2);
      }
      Shiny.onInputChange('myblob', sendToR)
      
    });
    ")
    #a=View(input$h1)
    
    observe({
      # print('myblob')
      # print(input$myblob)
      # print("captname")
      # print(captname)
      # print("dirname")
      # print(dirname)
      # print("capturesfolder")
      # print(capturesfolder)
      # print("screenshot-path")
      
      #write_tsv(x = as.data.frame(input$h1), file = "myblob.jpg")
    })
    #}
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
  observeEvent(input$startbutton, {
    runjs('start()')
  })
  
##SHINY SERVER LOGIC
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = '.'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw", "jpg", "jpeg", "png")
  )
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$dir)
  
  output$dir <- renderText({
    global$datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 req(is.list(input$dir))
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               })
  
}

# Run the application
shinyApp(ui = ui, server = server)
