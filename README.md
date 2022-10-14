# lamp-app

dependencies and stuff:

#R -e shiny::runapp('/root/app' host='0.0.0.0',port='3838')

#git clone https://github.com/easyomic/lamp-app.git

#cd lamp-app/

#docker build -t easyomics/shiny-lamp .

#docker images

#docker run -p 3838:3838 easyomics/shiny-lamp
#docker run -it -p 3838:3838 --name shiny-lamp  easyomics/shiny-lamp /bin/bash

#docker start shiny-lamp #if not running

docker exec -it shiny-lamp /bin/bash

####################################
#install.packages('shiny')

install.packages('shinyBS')

install.packages('imager')
#libxml2.so.2
#sudo apt-get install libxml2-dev

#libglpk.so.40
#sudo apt-get install -y libglpk-dev 

install.packages('colocr')
#libMagick++-6.Q16.so.8

install.packages('dplyr')

install.packages('qpcR')
#libGLU.so.1
#sudo apt-get install libglu1-mesa

###############
new dependencies to R install:

library(devtools)

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

#xdg-open
#sudo apt-get install xdg-utils



