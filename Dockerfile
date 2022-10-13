FROM rocker/shiny

RUN mkdir /root/app
COPY app /root/app

### Install dependencies
RUN apt-get update &&\
  apt-get install -y --allow-unauthenticated --no-install-recommends \
  libxml2-dev libglpk-dev libglu1-mesa libmagick++-dev \
  && apt-get clean && apt-get autoclean && apt-get autoremove \
  && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/

### Install new packages
RUN R -e "install.packages(pkgs=c('shinyBS','imager','colocr', 'dplyr', 'qpcR'), repos='https://cran.rstudio.com/')" 

CMD ["R", "-e", "shiny::runApp('/root/app', host='0.0.0.0', port=3838)"]
