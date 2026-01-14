# Author: Kaloyan Ganev
# Code to accompany Chapter 12 of
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
library(gridExtra)
library(openxlsx)
library(knitr)
library(stargazer)
library(xtable)

# One-way ANOVA
data(iris)
head(iris)

oneway.test(Sepal.Length ~ Species, 
                          data = iris,
                          var.equal = T)

# Kruskal-Wallis
iris2 <- iris %>% 
  select(Sepal.Length, Species) %>% 
  mutate(idx = rep(1:50, 3)) %>% 
  pivot_wider(names_from = Species, values_from = Sepal.Length) %>% 
  select(-idx)

kruskal.test(iris2)

# Two-way ANOVA
twoway_data <- read.xlsx("twoway_anova.xlsx") %>% 
  pivot_longer(cols = -ferttype,
               names_to = "field", 
               values_to = "yield")

twoway_anova <- aov(yield ~ ferttype + field, data = twoway_data)
summary(twoway_anova)

# Two-way ANOVA with interaction
twoway_data2 <- read.xlsx("twoway_anova_with_interaction.xlsx") %>% 
  rename(sector = "Сектор", 
         gender = "Пол", 
         wage = "Заплата")

twoway_anova2 <- aov(wage ~ sector * gender, 
                     data = twoway_data2)

