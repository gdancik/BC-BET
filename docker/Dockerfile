# build from shiny directory

FROM openanalytics/r-base

MAINTAINER Garrett M. Dancik

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    libcurl4-gnutls-dev \
    libssl-dev \
    libsasl2-dev\
    libssh2-1-dev \
    libxml2-dev 
RUN  apt install -y --no-install-recommends software-properties-common dirmngr 
RUN  wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc 

RUN  add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/" 

RUN  apt-get clean \
    && rm -rf /var/lib/apt/lists/ 

# install required packages
#RUN R -e "install.packages(c('shiny','DT', 'ggplot2', 'mongolite', 'pheatmap', 'tidyr', 'survival', 'GGally', 'cowplot', 'dplyr', 'reshape', 'xlsx', 'shinycssloaders', 'shinyBS', 'shinydashboard', 'shinyjs'), repos='https://cloud.r-project.org/')" 

RUN add-apt-repository ppa:c2d4u.team/c2d4u4.0+
RUN apt install --no-install-recommends -y r-cran-tidyverse
RUN apt install --no-install-recommends -y r-cran-reshape
RUN apt install --no-install-recommends -y r-cran-xlsx
RUN apt install --no-install-recommends -y r-cran-mongolite
RUN apt install --no-install-recommends -y r-cran-pheatmap
RUN apt install --no-install-recommends -y r-cran-survival
RUN apt install --no-install-recommends -y r-cran-cowplot
RUN apt install --no-install-recommends -y r-cran-shiny
RUN apt install --no-install-recommends -y r-cran-shinydashboard
RUN apt install --no-install-recommends -y r-cran-shinyjs
RUN apt install --no-install-recommends -y r-cran-shinycssloaders

RUN R -e "install.packages('GGally', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('shinyBS', repos = 'https://cloud.r-project.org/')"
RUN R -e "install.packages('DT', repos = 'https://cloud.r-project.org/')"

RUN rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# copy the app to the image
RUN mkdir /root/BCBET
COPY . /root/BCBET

COPY Rprofile.site /usr/lib/R/etc

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/BCBET')"]

