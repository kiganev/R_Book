# Author: Kaloyan Ganev
# Code to accompany Chapter 9 of
# ``Applied Statistics with R''

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

library(tidyverse)
library(openxlsx)

sample(1:49, size = 6, replace = FALSE)

data1 <- read.xlsx("for_systematic_sampling.xlsx")

startobs <- sample(1:50, 1, replace = F)

smpl1 <- data1$x[startobs]

for(i in 1:99){
  smpl1 <- c(smpl1, data1$x[startobs + 7*i])
}
