FROM rocker/shiny

RUN mkdir /root/app
COPY app /root/app

### Install new packages
#RUN R -e "install.packages(pkgs=c('shiny','tidyverse','shinydashboard', 'xgboost', 'DT', 'plotly', 'tidymodels', 'ggridges', 'tidytext'), repos='https://cran.rstudio.com/')" 

CMD ["R", "-e", "shiny::runApp('/root/app', host='0.0.0.0', port=3838)"]
