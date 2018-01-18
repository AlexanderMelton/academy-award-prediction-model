################################################################################
###               01_master_control - Oscar Prediction Model                 ###
################################################################################

#Set the working directory
wd <- "C:/Users/alex.melton/Documents/Side Projects/Oscar Model"
setwd(wd)

#Loading in packages
pkgs <- c("htmltab", "jsonlite", "rvest", "XML", "RCurl", "stringr", "randomForest",
          "plyr", "dplyr", "xml2", 'lubridate')
._ <- sapply(pkgs, library, character.only = TRUE, quietly = TRUE)

#Sourcing other files for the model 
source("02_retro_data_collection")
source("03_model_creation")
source("04_model_validation")
source("05_current_data_collection")
source("06_model_prediction")