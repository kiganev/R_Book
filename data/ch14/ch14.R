# Author: Kaloyan Ganev
# Code to accompany Chapter 14 of
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

low_skilled_income <- read.xlsx("low_skilled_income.xlsx")

head(low_skilled_income)

str(low_skilled_income)

mod1 <- lm(income ~ region, data = low_skilled_income)
summary(mod1)

twoway_data2 <- read.xlsx("twoway_anova_with_interaction.xlsx")

mod2 <- lm(wage ~ sector * gender, data = twoway_data2)
summary(mod2)

# ANCOVA regression
ancova_data <- read.xlsx("ancova_data.xlsx")

mod3 <- lm(log(wage) ~ age + I(age^2) + gender, data = ancova_data)
summary(mod3)

expm1(coef(mod3)[4])

# Piecewise linear regression
piecewise <- read.xlsx("piecewise.xlsx") %>%
  mutate(dum1 = ifelse(x <= 100, 0, 1)) 

mod4 <- lm(y ~ x + I(x - 100) : dum1, data = piecewise)
summary(mod4)