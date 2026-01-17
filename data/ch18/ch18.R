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

# ARMA
acf_ma1 <- data.frame(idx = c(0:10),
                      acf_ma1 = ARMAacf(ar = 0, 
                                        ma = 0.8, 
                                        lag.max = 10,
                                        pacf = F))

set.seed(2023)
eps <- rnorm(300)
ma1_sim <- as.xts(arima.sim(list(ma = 0.8), n = 300, innov = eps))

ma4_sim <- as.xts(arima.sim(list(ma = c(-0.6,0.3,-0.5,0.5)), n = 300, innov = eps))

ar1_sim1 <- arima.sim(list(ar = 0.1),  n = 300, innov = eps)
ar1_sim2 <- arima.sim(list(ar = 0.5),  n = 300, innov = eps)
ar1_sim3 <- arima.sim(list(ar = 0.95),  n = 300, innov = eps)

eacf_ma1_sim <- Acf(ma1_sim, lag.max = 10, plot = F)
epacf_ma1_sim <- Pacf(ma1_sim, lag.max = 10, plot = F)

ma1_mod <- Arima(ma1_sim, order = c(0, 0, 1), include.mean = F)
summary(ma1_mod)

auto.arima(ma1_sim, max.p = 10, max.q = 10, ic = "bic")

y_df <- read.xlsx("y_series.xlsx") %>% 
  mutate(date = as.Date(date, origin = as.Date("1899-12-30")))

adf.test(y_df$y)

y_df <- y_df %>% 
  mutate(dy = c(NA,diff(y)))

adf.test(y_df$dy[-1])

arima_est <- auto.arima(y_df$y, d = 1, max.p = 10, max.q = 10)
summary(arima_est)

good_fit <- cor(y_df$y, fitted(arima_est))^2

bgtest(resid(arima_est) ~ 1, order = 12)

bptest(resid(arima_est)^2 ~ fitted(arima_est)^2)

jarque.bera.test(resid(arima_est))

# ARIMAX
arimax_data <- read.xlsx("arimax_data.xlsx") %>% 
  mutate(date = as.Date(date, origin = "1899-12-30"))

arimax1 <- auto.arima(arimax_data$y2, d = 1, xreg = arimax_data$x2)
summary(arimax1)

# SARIMA
load("y3.RData")

hegy.test(y3.ts)