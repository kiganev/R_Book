# Author: Kaloyan Ganev
# Code to accompany Chapter 1 of
# ``Applied Statistics with R''
# (in Bulgarian)

# Clear wortkspace
rm(list = ls())

# Clear plots
check_dev <- dev.list()
if(!is.null(check_dev)){
  dev.off(dev.list()["RStudioGD"])  
}

# Get location of current script
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)

# Set working directory to script location
setwd(fileloc)

# Remove fileloc variable
rm(fileloc, check_dev)

# Set locale to English 
Sys.setlocale("LC_ALL", "en_US.utf8")

getwd()

dir()

# Make sure you already created the folder you want to make
# default working directory
# Change "MyFolder" to the name you prefer
setwd("d:/MyFolder")

install.packages("tseries")

# The data contained in the file are just 1000 random numbers
# drawn from the standard normal distribution
foreign::read.dta("some_data.dta")

library(foreign)
read.dta("some_data.dta")

help("mean")
?mean

