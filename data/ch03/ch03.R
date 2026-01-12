# Author: Kaloyan Ganev
# Code to accompany Chapter 3 of
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

sd

formals(sd)

body(sd)

environment(sd)

names(methods:::.BasicFunsList)

# Creating functions
cyl_vol <- function(r,h){
  pi * r^2 * h
}

cyl_vol(2, 5)

root_cplx <- function(x, n = 2){
  x <- as.complex(x)
  x^(1/n)
}

# Environments
env1 <- new.env()

env1[["v1"]] <- c(1, 2, 3) # or
env1$v1 <- c(1, 2, 3)

env1$v1

ls(envir = env1)

as.environment(list(a = 1, b = 2))