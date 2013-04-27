## @knitr intro ####
# First we set working directory
setwd("/Users/vrangbaek/Dropbox/Studieophold/College_of_Europe/Master_Thesis")
# Read data
IQM_pro_data <- read.csv("Data/IQM_pro_data1.4.csv")
ls(IQM_pro_data)
# Loading all the necessary libraries
library(foreign)
library(reshape)
library(plm)
library(estout)
library(psych)

## @knitr datamanagement ####
source("CoE_thesis_repository/data_management.R",
       local = FALSE, echo = TRUE)

## @knitr datadesc ####
source("CoE_thesis_repository/datadesc.R",
       local = FALSE, echo = TRUE)

## @knitr analysis ####
source("CoE_thesis_repository/analysis.R",
       local = FALSE, echo = TRUE)





