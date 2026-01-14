# Author: Kaloyan Ganev
# Code to accompany Chapter 13 of
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
library(openxlsx)
library(forecast)
library(lmtest)
library(moments)

# Load the data
xy_data <- read.xlsx("xy_data.xlsx")

mod1 <- lm(y ~ x, data = xy_data)

print(mod1)

summary(mod1)

coef(mod1)
mod1$coefficients

fitted(mod1)
mod1$fitted.values

resid(mod1)
mod1$residuals

deviance(mod1)
df.residual(mod1)

sigmahat_mod1 <- sqrt(deviance(mod1)/df.residual(mod1))

coef(summary(mod1))[,2]

anova(mod1)

x_new <- as.data.frame(seq(1000, length.out = 10, by = 150))
colnames(x_new) <- "x"

y_predict <- predict(mod1, newdata = x_new)

yhat_confint <- predict(mod1, interval = "confidence")
yhat_predint <- predict(mod1, interval = "prediction")

xy_data <- xy_data %>% 
  mutate(z = 3 * x)

mod2 <- lm(y ~ x + z, data = xy_data)
summary(mod2)

x1x2y_data <- read.xlsx("x1x2y_data.xlsx")

mod3 <- lm(y ~ x1 + x2, data = x1x2y_data)
summary(mod3)

anova(mod3)

acf_eps <- acf(resid(mod3))
acf_eps

# Diagnostics
Box.test(resid(mod3), lag = 4, type = "Ljung-Box", fitdf = 2)

bgtest(mod3, order = 4, type = "Chisq")

gqtest(mod3, point = 0.4, fraction = 10, order.by = x1, alternative = "two.sided")

bptest(mod3)

jarque.test(resid(mod3))

ks.test(resid(mod3), "pnorm", mean = mean(resid(mod3)), sd = sd(resid(mod3)))

# No intercept
mod1a <- lm(y ~ 0 + x, data = xy_data)
mod1b <- lm(y ~ -1 + x, data = xy_data)

# Elasticity example
remun_sales <- read.xlsx("remun_sales_data.xlsx")
mod4 <- lm(log(remun) ~ log(sales), data = remun_sales)

mod4a <- lm(log(remun) ~ sales, data = remun_sales)
summary(mod4a)

expm1(coef(mod4a)[2]*1000)

# Polynomial regression
poly_data <- read.xlsx("poly_data.xlsx")

mod5 <- lm(y ~ x, data = poly_df)
summary(mod5)

mod6 <- lm(y ~ x + I(x^2) + I(x^3), data = poly_df)
summary(mod6)