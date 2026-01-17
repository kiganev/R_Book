# Author: Kaloyan Ganev
# Code to accompany Chapter 19 of
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
library(chron)
library(zoo)
library(xts)
library(lubridate)
library(eurostat)
library(quantmod)
library(dynlm)
library(openxlsx)
library(forecast)
library(mFilter)
library(tseries)
library(lmtest)
library(latex2exp)
library(gridExtra)
library(forecast)
library(uroot)

load("gdp_bg.RData")

train.ts <- window(gdp_bg.ts, start = c(1996, 1), end = c(2023, 4))
test.ts <- window(gdp_bg.ts, start = c(2024, 1), end = c(2025, 2))

hw3 <- HoltWinters(train.ts)

fc_hw3 <- forecast(hw3, h = 6)

class(fc_hw3)

fc_hw3$mean
fc_hw3$lower
fc_hw3$upper

fc_df <- data.frame(
  qtr = seq(from = as.Date("1996-01-01"), 
             to = as.Date("2025-04-01"),
             by = "quarters"),
  gdp = as.vector(gdp_bg.ts)
)

fc_df <- fc_df %>% 
  mutate(fc_mean = c(rep(NA, n() - 6), fc_hw3$mean)) %>%
  mutate(fc_lower = c(rep(NA, n() - 6), fc_hw3$lower[,2])) %>% 
  mutate(fc_upper = c(rep(NA, n() - 6), fc_hw3$upper[,2])) %>% 
  mutate(train = c(subset(gdp, qtr < as.Date("2024-01-01")), rep(NA, 6))) %>% 
  mutate(test = c(rep(NA, n() - 6), subset(gdp, qtr >= as.Date("2024-01-01"))))

theme_set(
  theme_minimal() +
    theme(
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 14),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16)
    )
)

fc_fc <- structure(fc_df$fc_mean, class = "forecast")

accuracy(fc_fc, fc_df$test)

# Forecast with ARIMA
train.ts <- ts(fc_df$train[!is.na(fc_df$train)], start = c(1996, 1), frequency = 4)
train.ts

hegy.test(train.ts)  

sarima_est <- auto.arima(train.ts)
summary(sarima_est)

sarima_fc <- forecast(sarima_est, h = 6)

fc_df <- fc_df %>% 
  mutate(fc2_mean = c(rep(NA, n() - 6), sarima_fc$mean),
         fc2_lower = c(rep(NA, n() - 6), sarima_fc$lower[,2]),
         fc2_upper = c(rep(NA, n() - 6), sarima_fc$upper[,2]))

fc2_fc <- structure(fc_df$fc2_mean, class = "forecast")
accuracy(fc2_fc, fc_df$test)