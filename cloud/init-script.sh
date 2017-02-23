#! /bin/bash

# new rstudio psswd = rs num
apt update -y
apt install -y awscli libcurl4-openssl-dev libxml2-dev gdebi-core zsh git-core

#R -e "install.packages(c('caret'), repos='http://cran.rstudio.com/', dependencies = c('Depends','Imports'))"
#R -e "install.packages(c('tidyverse','glmnet','randomForest','xgboost','rvest','stringr','viridis','leaflet'), repos='http://cran.itam.mx/', dependencies = TRUE)"
