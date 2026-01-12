# Author: Kaloyan Ganev
# Code to accompany Chapter 8 of
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
library(mvtnorm)

dbinom(0:10, 10, 0.6)

pbinom(3, 10, 0.5)

qbinom(0.75, 10, 0.6)

dgeom(14, 0.55)

pgeom(14, 0.55)

dhyper(20, 185, 15, 20)

dpois(10, 15)

ppois(10, 15)

dunif(25, 0, 50)

punif(10, 0, 50)

punif(45, 0, 50) - punif(40, 0, 50)

dnorm(1, mean = 2, sd = 5)

pnorm(2, mean = 1, sd = 2)

pnorm(3, 1, 2) - pnorm(-1, 1, 2)

pnorm(5, 1, 2) - pnorm(-3, 1, 2)

pnorm(7, 1, 2) - pnorm(-5, 1, 2)

qnorm(0.025)

qnorm(0.975)

dlogis(1, 2, 5)

plogis(2, 1, 2)

dexp(1, 5)

pexp(4, 1/4) - pexp(3, 1/4)

gamma(3.2)

dgamma(1, 3, 0.5)

pgamma(40, 10, 1/3)	

# Multivariate normal
sigmamat <- matrix(c(1, 0.6, 0.6, 1), nrow = 2)
xvec <- c(1, 2)
mus <-  c(0.05, -0.05)
dmvnorm(x = xvec, mean = mus, sigma = sigmamat)

sigmamat2 <- matrix(c(1, 0, 0, 1), nrow = 2)
rvec2 <- rmvnorm(n=10000, mean = mus, sigma = sigmamat2)
rvec2 <- as.data.frame(rvec2) %>% 
  rename(X1 = V1, X2 = V2)