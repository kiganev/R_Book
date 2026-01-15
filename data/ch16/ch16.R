# Author: Kaloyan Ganev
# Code to accompany Chapter 16 of
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

# ts
gdp_data <- get_eurostat("namq_10_gdp")

gdp_data_bg <- gdp_data %>% 
  filter(geo == "BG",
         unit == "CLV10_MEUR",
         s_adj == "NSA") %>% 
  select(TIME_PERIOD, na_item, values) %>% 
  pivot_wider(names_from = na_item, 
              values_from = values)

P3.ts <- ts(gdp_data_bg$P3, start = c(1996, 1), frequency = 4)
P3.ts

start(P3.ts)

end(P3.ts)

frequency(P3.ts)

deltat(P3.ts)

P3.ts.smpl <- window(P3.ts, 
                     start = c(2006, 1), 
                     end = c(2011, 3))
P3.ts.smpl

mts1 <- ts(gdp_data_bg[c("P3", "B1GQ")], start = c(1995, 1), frequency = 4)
mts1.smpl <- window(mts1, start = c(2008, 1), end = c(2010, 4))

mts1.smpl

class(mts1.smpl)

P51G.ts <- ts(gdp_data_bg$P51G, start = c(1995, 1), frequency = 4)
P51G.ts.smpl <- window(P51G.ts, start = c(2006, 4), end = c(2012, 2))

mts2 <- cbind(P3.ts.smpl, P51G.ts.smpl)
mts2

mts2a <- ts.union(P3.ts.smpl, P51G.ts.smpl)
mts2a

mts3 <- ts.intersect(P3.ts.smpl, P51G.ts.smpl)
mts3

P3.ts.smpl_lag <- stats::lag(P3.ts.smpl, -1)

mts4 <- ts.union(P3.ts.smpl, P3.ts.smpl_lag)
mts4

P3.ts.smpl_diff <- diff(P3.ts.smpl)
P3.ts.smpl_diff

P3.ts.smpl_diff2 <- diff(P3.ts.smpl, differences = 2)
P3.ts.smpl_diff2

P3.ts.smpl_diffs <- diff(P3.ts.smpl, lag = 4)
P3.ts.smpl_diffs

P3.ts.smpl_grate <- diff(log(P3.ts.smpl), lag = 4) * 100
P3.ts.smpl_grate

# zoo
m1 <- matrix(rnorm(480), nrow = 120)
colnames(m1) <- c("var1", "var2", "var3", "var4")
idx1 <- seq(from = as.Date("2001-01-01"), length.out = 120, by = "months")

zoo1 <- zoo(m1, order.by = idx1)

head(zoo1)

index(zoo1)
coredata(zoo1)

str(zoo1)
summary(zoo1)

zoo2 <- window(zoo1, 
               start = as.Date("2005-11-15"), 
               end = as.Date("2007-08-30"))

zoo1[,1]
zoo1$var1

head(dplyr::lag(zoo1))

zoo2a <- cbind(var1 = zoo1$var1, lag_var1 = stats::lag(zoo1$var1, -1))
head(zoo2a)

zoo2b <- merge(var1 = zoo1$var1, lag_var1 = stats::lag(zoo1$var1, -1))
head(zoo2b)

exp_imp_bg <- read.zoo("exp_imp_bg.csv",
                      index.column = 1,
                      FUN = as.Date,
                      sep = ",",
                      header = T)

exp_imp_bg

write.zoo(zoo1, "zoo1.csv", sep = ",")

index(exp_imp_bg) <- as.yearqtr(index(exp_imp_bg))
head(exp_imp_bg)

# xts
xts1 <- xts(gdp_data_bg[,2:3],
            order.by = gdp_data_bg$TIME_PERIOD,
            my_descr = "My first xts object!")

class(xts1)

xtsAttributes(xts1)

xts1[,1]

xts1["2010"]
xts1["2010-01"]
xts1["2010-01/2010-06"]

apply.yearly(xts1, mean)

apply.yearly(xts1, function(x) max(x) - min(x))

getSymbols("GOOG", src="yahoo")

endp1 <- endpoints(GOOG,"weeks")
period.apply(GOOG$GOOG.Open, INDEX=endp1, mean)

nyears(GOOG)

