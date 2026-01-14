# Author: Kaloyan Ganev
# Code to accompany Chapter 11 of
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
library(BSDA)
library(EnvStats)

# Generate population
pop <- rgamma(1000000, 3, 0.5)

# Draw sample
smpl <- sample(pop, 10000)

# Compute sample mean
smpl_mean <- mean(smpl)

# Compute confidence interval bounds
upper_bound <- smpl_mean + 1.96 * sqrt(12/10000)
lower_bound <- smpl_mean - 1.96 * sqrt(12/10000)

# Compute sample variance
smpl_var <- var(smpl)

# Compute confidence interval bounds assuming population variance unknown
upper_bound2 <- smpl_mean + 1.96 * sqrt(smpl_var/10000)
lower_bound2 <- smpl_mean - 1.96 * sqrt(smpl_var/10000)

# Draw a small sample
smpl_small <- sample(pop, 25)

# Compute sample mean for the small sample
smpl_small_mean <- mean(smpl_small)

# Find the 2.5 percentile for the t(24) distribution
t_quant <- qt(0.025, 24)

# Compute variance for the small sample
smpl_small_var <- var(smpl_small)

# Compute confidence bounds for small sample mean
upper_bound3 <- smpl_small_mean + t_quant * sqrt(smpl_small_var/25)
lower_bound3 <- smpl_small_mean - t_quant * sqrt(smpl_small_var/25)

# Compute 2.5 and 97.5 percentiles for chi^2(24)
chi_left <- qchisq(0.025, 24)
chi_right <- qchisq(0.975, 24)

# Compute confidence bounds for sample variance
lower_bound4 <- 24 * smpl_var / chi_right
upper_bound4 <- 24 * smpl_var / chi_left

# z-test
z.test(smpl, mu = 7, sigma.x = sqrt(12))

# t-test
t.test(smpl, mu = 7)

# One-sided
z.test(smpl, mu = 6.1, alternative = "greater", sigma.x = sqrt(12))
t.test(smpl, mu = 6.1, alternative = "greater")

# Two-sample t-test
pop2 <- rgamma(1000000, 3.5, 0.5)
smpl2 <- sample(pop2, 10000)

t.test(x = smpl, y = smpl2)

# Wilcoxon test
wilcox.test(smpl, mu = 7, alternative = "two.sided")

wilcox.test(smpl, smpl2, paired = T)

# Mann-Whitney
wilcox.test(smpl, smpl2, paired = F, alternative = "less")

# variance chi-squared test
varTest(smpl, sigma.squared = 12)

chisq_stat <- smpl_var / 12 * (length(smpl) - 1)
p_val_chisq_stat <- 2 * min(pchisq(chisq_stat, (length(smpl) - 1), lower.tail = T),
        pchisq(chisq_stat, (length(smpl) - 1), lower.tail = F))

# variance equality F-test
var.test(smpl2, smpl)

lb_f <- var(smpl2)/var(smpl) * qf(0.025, length(smpl) - 1, length(smpl2) - 1)
ub_f <- var(smpl2)/var(smpl) * qf(0.975, length(smpl) - 1, length(smpl2) - 1)

p_val_f_stat <- 2 * (min(pf(f_stat, length(smpl2) - 1, length(smpl) - 1),
                       pf(f_stat, length(smpl2) - 1, length(smpl) - 1, lower.tail = F)))

