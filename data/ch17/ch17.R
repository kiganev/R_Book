# Author: Kaloyan Ganev
# Code to accompany Chapter 17 of
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

gdp_data_bg <- read.xlsx("gdp_data_bg.xlsx")

# Fit qubic trend
trend_eq <- dynlm(B1GQ ~ I(trend(B1GQ)^2) + I(trend(B1GQ)^3), 
                  data = gdp_data_bg)
summary(trend_eq)

# Moving averages
ma_df <- gdp_data_bg %>% 
  select(TIME_PERIOD, B1GQ)

ma_df <- ma_df %>% 
  mutate(ma1 = rollmean(B1GQ, k = 4, fill = NA, align = "right"))

ma_df <- ma_df %>% 
  mutate(ma2 = lead(rollmean(B1GQ, k = 4, 
                             fill = NA, 
                             align = "right")))

ma_df <- ma_df %>% 
  mutate(ma3 = rollmean(B1GQ, 
                        k = 5, 
                        fill = NA,
                        align = "center"))

ma_df <- ma_df %>% 
  mutate(ma4 = ma(B1GQ, order = 4, centre = T))

gdp_bg.ts <- ts(ma_df$B1GQ, start = c(1996, 1), freq = 4)

decomp_gdp <- decompose(gdp_bg.ts)

# Exponential smoothing 
hw1 <- HoltWinters(gdp_bg.ts, beta = F, gamma = F)
class(hw1)

hw1

hw2 <- HoltWinters(gdp_bg.ts, gamma = F)

hw3 <- HoltWinters(gdp_bg.ts)

hw3

# HP filter
gdp_data_bg_sca <- read.xlsx("gdp_data_bg_sca.xlsx")

gdp_bg_sca.xts <- xts(gdp_data_bg_sca$B1GQ,
                      order.by = gdp_data_bg_sca$TIME_PERIOD,
                      dimnames = list(NULL, "B1GQ"))



gdp_bg_hp <- hpfilter(log(gdp_bg_sca.xts$B1GQ), freq = 1600, type = "lambda")

gdp_bg_sca.xts$trend <- as.vector(gdp_bg_hp$trend)

gdp_bg_bk <- bkfilter(log(gdp_bg_sca.xts$B1GQ), pl = 6, pu = 32, nfix = 12)

gdp_bg_cf <- cffilter(log(gdp_bg_sca.xts$B1GQ), root = T, pl = 6, pu = 32)

# Seasonal
ma_df <- ma_df %>% 
  mutate(detr4 = B1GQ - ma4)

ma_df <- ma_df %>% 
  mutate(dumvar = as.factor(rep(c(1:4), 
                                length.out = length(ma_df$TIME_PERIOD))))

mod_detr4 <- lm(detr4 ~ dumvar, data = ma_df)
summary(mod_detr4)

ma_df <- ma_df %>% 
  mutate(seas4 = c(rep(NA, 2), fitted(mod_detr4), rep(NA, 2)))

seas_means <- ma_df %>% 
  group_by(dumvar) %>% 
  summarise(meanx = mean(detr4, na.rm = T)) %>% 
  mutate(meanx2 = meanx - mean(meanx))

ma_df <- ma_df %>% 
  mutate(irreg4 = detr4 - seas4)

hw3$fitted[,4]