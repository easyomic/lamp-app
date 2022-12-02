#javascript e html de captura embebido no app.r
#função zoom nativo da camera do aparelho
#sem distorção de captura
#compativel apenas com navegadores google chrome devido ao image capture:
# https://github.com/w3c/mediacapture-image/blob/gh-pages/implementation-status.md
#no opera nao funciona zoom, mas captura ok

library(shiny)
library(shinyjs)
library(shinyscreenshot)


ui <- fluidPage(
  useShinyjs(),
  #theme = shinythemes::shinytheme("flatly"),
  fluidRow(
    # column(width = 7, h3("Beeromics LAMPshot"),
    #        startWebcam(width = 800, height = 600, quality = 100),
    #        #snapshotButton(),
    #        #takeSnapshot(),
    #        actionButton("go", "Start capturing") ),
    column(width = 6, h3("testHTMLcode"),
           actionButton(inputId = "startbutton", label = 'Start!'),
           actionButton(inputId = "capturebutton", label = 'Capture!'),
           HTML(
             '
        <p><button hidden id="start" onclick="start()">start camera!</button></p>
        <div>
        <div style="float:left;margin-right:5px;">
        <p><video id="video-tag" width=100% height="180" autoplay=true/></p>
        </div>
        <div style="float:left">
        <p><img id="image-tag" width=100%> </img></p>                             
        </div>
        </div>
        <div style="width: 100%">
        <input type="range" id="zoom-slider" min="0", max="0">
        <output id="zoom-slider-value"></output>
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
    })
    .catch((err) => { 
      console.error('takePhoto() failed: ', e);
    });
}
//start()
//gotMedia(mediastream)
//takePhoto()
      "
             )
             
           )
           
    )
  ))
server <- function(input, output){
  observeEvent(input$startbutton, {
    runjs('start()')
  })
  observeEvent(input$capturebutton, {
    runjs('takePhoto()')
    screenshot(timer = 1, selector = "#image-tag", scale = 4)
  })
}
shinyApp(ui,server)
