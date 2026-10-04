#!/bin/bash
set -e
apt-get update -y
apt-get install -y r-base r-base-dev libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libcairo2-dev

R -e '
pkgs <- c("shiny","readxl","dplyr","tidyr","stringr","lubridate","ggplot2","plotly","DT","openxlsx","rlang","Matrix","randomForest","xgboost","cluster","jsonlite","MASS")
install.packages(pkgs, repos="https://mirrors.ustc.edu.cn/CRAN/", Ncpus=4)
'