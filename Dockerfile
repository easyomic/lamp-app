FROM rocker/shiny

RUN mkdir /root/app
COPY app /root/app

### Install dependencies
RUN apt-get update \
  && apt-get install -y --allow-unauthenticated --no-install-recommends \
  libxml2-dev libglpk-dev libglu1-mesa libmagick++-dev \
  && apt-get clean && apt-get autoclean && apt-get autoremove \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/

### Install new packages
#try install devtools alone 
RUN R -e "install.packages('devtools')"

RUN R -e "install.packages('shinylogs')"

RUN R -e "install.packages(pkgs=c('shinyBS','imager','colocr', 'dplyr', 'qpcR'), repos='https://cran.rstudio.com/')"

#try simplify devtools install
#RUN R -e "install.packages(pkgs=c('devtools', 'shinyscreenshot', 'tidyverse','shinyjs', 'xfun'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(pkgs=c('shinyscreenshot', 'tidyverse','shinyjs', 'xfun'), repos='https://cran.rstudio.com/')"

#RUN R -e "if(!require(shinysense)){ devtools::install_github("Aciole-David/shinysense") #library(shinysense) }"
RUN R -e "devtools::install_github('Aciole-David/shinysense')"

#RUN R -e "if(!require(r2d3)){ devtools::install_github("rstudio/r2d3") library(r2d3) }"
RUN R -e "devtools::install_github('rstudio/r2d3')"


CMD ["R", "-e", "shiny::runApp('/root/app', host='0.0.0.0', port=3838)"]
