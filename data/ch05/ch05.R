# Author: Kaloyan Ganev
# Code to accompany Chapter 5 of
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
library(openxlsx)
library(readxl)
library(foreign)
library(haven)
library(hexView)
library(DBI)
library(RSQLite)
library(RMariaDB)
library(RODBC)

# Read and write text/csv/tsv files
eu_data <- read.table("https://cohesiondata.ec.europa.eu/resource/vs2b-dct3.csv",
                         sep = ",", header = T)

eu_data <- read.csv("https://cohesiondata.ec.europa.eu/resource/vs2b-dct3.csv")

write.table(eu_data, "eu_data.tsv", sep = "\t")

# Read and write xls and xlsx files
xl_data <- read.xlsx("https://github.com/kiganev/R_Book/raw/refs/heads/main/data/ch05/example_file.xlsx")

write.xlsx(eu_data, "eu_data.xlsx")

file_to_write <- "multi_worksheet.xlsx"
output <- createWorkbook()
addWorksheet(output, "EU data")
addWorksheet(output, "Excel data")
writeData(output, sheet = "EU data", eu_data)
writeData(output, sheet = "Excel data", xl_data)
saveWorkbook(output, file_to_write, overwrite = T)

file_url <- "https://github.com/kiganev/R_Book/raw/refs/heads/main/data/ch05/example_file.xlsx"
download.file(file_url, "temp.xlsx", mode = "wb")
xl_data2 <- read_excel("temp.xlsx")


# SAS files
write_xpt(xl_data, "example.xport", version = 5)

file_url <- "https://github.com/kiganev/R_Book/raw/refs/heads/main/data/ch05/example.xport"
download.file(file_url, "sas_data.xport", mode = "wb")
sas_data <- read.xport("sas_data.xport")

write.foreign(xl_data2, "xldata.csv", "xldata.sas", package = "SAS")

# SPSS files
file_url <- "https://webfs.oecd.org/pisa2018/SPSS_SCH_QQQ.zip"
download.file(file_url, "SPSS_SCH_QQQ.zip")
unzip("SPSS_SCH_QQQ.zip")
spss_data <- read.spss("./SCH/CY07_MSU_SCH_QQQ.sav", to.data.frame = T)

# Stata files
stata_data <- read.dta("http://www.principlesofeconometrics.com/stata/airline.dta")

# EViews files
file_url <- "http://www.principlesofeconometrics.com/poe3/data/eviews/airline.wf1"
download.file(file_url, "airline.wf1")
eviews_data <- readEViews("airline.wf1")

# Weka files
data_weka <- read.arff("http://storm.cis.fordham.edu/~gweiss/data-mining/weka-data/weather.arff")
write.arff(data_weka, "weka_file.arff")

# Databases
drv <- dbDriver("SQLite")

file_url <- "https://www.sqlitetutorial.net/wp-content/uploads/2018/03/chinook.zip"
download.file(file_url, "chinook.zip")
unzip("chinook.zip")

db_path <- "./chinook.db"

sqlite_con <- dbConnect(drv, db_path)

dbListTables(sqlite_con)

sink(file = "dblisttables.txt")
dbListTables(sqlite_con)
sink(file = NULL)

dbListFields(sqlite_con, "artists")

sink(file = "dblistfields.txt")
dbListFields(sqlite_con, "artists")
sink(file = NULL)

sqlite_query <- "SELECT 
									trackid, 
									tracks.name AS Track,
									albums.title AS Album, 
									artists.name AS Artist
						FROM tracks 
						INNER JOIN albums ON albums.albumid = tracks.albumid
						INNER JOIN artists ON artists.artistid = albums.artistid
						WHERE artists.artistid = 22;"

sqlite_data <- dbGetQuery(sqlite_con, sqlite_query)

drv2 <- dbDriver("MariaDB")

genome_con <- dbConnect(drv2, dbname = "ensembl_compara_51",
                        user="anonymous", password="", host = "ensembldb.ensembl.org")

dbListTables(genome_con)
dbListFields(genome_con, "genome_db")

genome_data <- dbReadTable(genome_con, "genome_db")

odbc_con <- odbcConnect("sampledb", believeNRows = F, rows_at_time = 1)

sqlTables(odbc_con)

odbc_data <- sqlFetch(odbc_con, "albums")

odbc_data2 <- sqlQuery(odbc_con, sqlite_query)

