#! /bin/bash

# https://aws.amazon.com/blogs/big-data/running-r-on-aws://aws.amazon.com/blogs/big-data/running-r-on-aws/
# http://www.louisaslett.com/RStudio_AMI/
# *** http://strimas.com/r/rstudio-cloud-1/
# ami-6a52840a
# ssh -i aws/spark-test.pem ubuntu@ec2-52-42-104-245.us-west-2.compute.amazonaws.com
# https://www.r-bloggers.com/how-to-install-r-on-linux-ubuntu-16-04-xenial-xerus/

# choose vim
sudo update-alternatives --config editor

#install system deps
sudo apt update
sudo apt install -y libcurl4-openssl-dev libxml2-dev gdebi-core zsh git-core awscli parallel libudunits2-dev

sudo chsh -s `which zsh` ubuntu
sudo chsh -s `which zsh` rstudio
sudo passwd rstudio
# passwd = rs num
# once for each user:
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh

# change user with
su - rstudio

#export R_LIBS_USER="/home/rstudio/R/library"
#echo 'export R_LIBS_USER="/home/rstudio/R/library"' >> .zshrc
#mkdir -p $R_LIBS_USER

R -e "install.packages(c('caret'), repos='http://cran.rstudio.com/', dependencies = c('Depends','Imports'))"
R -e "install.packages(c('tidyverse','glmnet','randomForest','xgboost','rvest','stringr','viridis','leaflet'), repos='http://cran.itam.mx/', dependencies = TRUE)"

# try installing in parallel
#echo 'tidyverse,glmnet,randomForest,xgboost,rvest,stringr,viridis,leaflet,rgl,rgdal' | tr ',' '\n' | parallel -j4 'R -e "install.packages(\"{}\", repos = \"http://cran.itam.mx/\", dependencies = TRUE)"'


#############################
## wrong and already in the good image
###########################

#install R
sudo echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list
gpg --keyserver keyserver.ubuntu.com --recv-key E084DAB9
gpg -a --export E084DAB9 | sudo apt-key add -
sudo apt update
sudo apt install r-base r-base-dev

# create rstudio user
sudo mkdir /home/rstudio
sudo useradd --home-dir /home/rstudio rstudio
sudo chown rstudio:sudo /home/rstudio
sudo passwd rstudio
# psswd = rstudio
sudo chmod -R 0755 /home/rstudio
# add NOPASSWD option to rstudio user?
sudo visudo
sudo usermod -aG sudo rstudio
#sudo usermod -aG shiny rstudio
# add /bin/bash as login shell for rstudio
sudo vi /etc/passwd
su - rstudio
wget https://github.com/robbyrussell/oh-my-zsh/raw/master/tools/install.sh -O - | zsh
chsh -s `which zsh`

export R_LIBS_USER="/home/rstudio/R/library"
echo 'export R_LIBS_USER="/home/rstudio/R/library"' >> .zshrc
mkdir -p $R_LIBS_USER

#install RStudio-Server
R -e "install.packages('shiny', repos='http://cran.rstudio.com/')"
#wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
#sudo gdebi shiny-server-1.5.1.834-amd64.deb
wget https://download2.rstudio.org/rstudio-server-1.0.136-amd64.deb
sudo gdebi rstudio-server-1.0.136-amd64.deb



#echo username:password | chpasswd



