# Author: Kaloyan Ganev
# Code to accompany Chapter 6 of
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

# Load necessary packages
library(tidyverse)
library(eurostat)
library(knitr)

# Query database and download GDP data
query <- search_eurostat("GDP")
na_data <- get_eurostat("nama_10_gdp")

head(na_data)

# Get dictionary on na_item
na_item_dict <- get_eurostat_dic("na_item")

# Subset data for Bulgaria only,
# save result to data frame
na_data_bg <- filter(na_data, geo == "BG")

# Bulgarian data + chain-linked volumes at 2010 prices, EUR mn
na_data_bg <- filter(na_data,
                     geo == "BG" & unit == "CLV10_MEUR")
na_data_bg <- filter(na_data,
                     geo == "BG", 
                     unit == "CLV10_MEUR")

# Select only desired columns (variables)
na_data_bg <- select(na_data_bg,
                     c(TIME_PERIOD, na_item, values))
na_data_bg <- select(na_data_bg,
                     -c(geo, unit))

# Convert to wide format
na_data_bg <- pivot_wider(na_data_bg, 
                          names_from = na_item, 
                          values_from = values)

# Return to long format
df_long <- pivot_longer(na_data_bg,
                        cols = -TIME_PERIOD,
                        names_to = "na_item",
                        values_to = "values")

# Select six variables
na_data_bg <- select(na_data_bg, c(TIME_PERIOD, B1GQ, P3, P51G, P6, P7))

# Create net exports variable
na_data_bg <- mutate(na_data_bg, NX = P6 - P7)

# Rename B1GQ to GDP
na_data_bg <- rename(na_data_bg, GDP = B1GQ)

# All above using the pipe
na_data_bg <- na_data %>% 
  filter(geo == "BG",
         unit == "CLV10_MEUR") %>% 
  select(-c(unit, geo)) %>% 
  pivot_wider(names_from = na_item, values_from = values) %>% 
  select(TIME_PERIOD, B1GQ, P3, P51G, P6, P7)

na_data_bg_head <- head(na_data_bg, n = 5)