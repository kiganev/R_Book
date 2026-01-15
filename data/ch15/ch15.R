# Author: Kaloyan Ganev
# Code to accompany Chapter 15 of
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
library(eurostat)
library(mFilter)
library(openxlsx)
library(moments)
library(xtable)
library(AER)

# Logistic regression
old_movies <- read.xlsx("oldmovies.xlsx")

mod1 <- lm(likes ~ age, data = old_movies)
summary(mod1)

mod2 <- glm(likes ~ age, family = binomial(link="logit"), data = old_movies)
summary(mod2)

confint(mod2)

anova(mod2)

flu_data <- read.xlsx("flu_munic.xlsx")

mod3 <- glm(cases ~ temp + vacc_rate, 
            family = "poisson", data = flu_data)
summary(mod3)

exp(coef(mod3)[1])

anova(mod3)

disp_par <- deviance(mod3)/df.residual(mod3)

dispersiontest(mod3, alternative = "two.sided")

mod4 <- glm(cases ~ temp + vacc_rate, 
            family = "quasipoisson", 
            data = flu_data)
summary(mod4)

# Gompertz
gompertz_data <- read.xlsx("gompertz.xlsx")

mod5 <- nls(y ~ const + alpha * exp(-beta * exp(-gamma * t)),
            start = list(const = 20, alpha = 250, beta = 30, gamma = 0.01),
            data = gompertz_data)

summary(mod5)

cor(gompertz_data$y, fitted(mod1))