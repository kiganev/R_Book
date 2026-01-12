# Author: Kaloyan Ganev
# Code to accompany Chapter 2 of
# ``Applied Statistics with R''
# (in Bulgarian)

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

# R as a calculator
5 + 6 # Addition

11 - 3 # Subtraction

25 * 41 # Multiplication

99 / 31.6 # Division

2 ** 5

2 ^ 5

11 %/% 2

11 %% 2

5.5 / 0

-1.5 / Inf

0 / 0

Inf / Inf

Inf / 0

0 / Inf

sqrt(3.85)

sqrt(-1)

sqrt(-4.38 + 0i)

exp(1)

# Check type
typeof(12.581)

typeof(81.99 + 1.994i)

typeof("January 18, 2021")

is.numeric(1524)

is.character(3 + 2i)

is.raw(12.85)

# R objects and classes
x <- 6.8
6.8 -> x

assign("x", 6.8)

x <- c(3.5, 1.0, NA, 1.2)
y <- c("This", "is", "a", "vector", "!")
z <- c(1 + 2.8i, 1 - 2.8i)

x <- seq(1:10)

x <- 1:10

y <- seq(from = 1, to = 10, by = 0.1)

ones <- rep(1, times=100)

x <- c(1,2,3)
y <- rep(x,times=3)

print(y)
y

x <- 23.1

length(x)

lunch_prices <- c(3.30, 5.91, 2.75)

names(lunch_prices) <- c("Soup", "Main course", "Dessert")

lunch_prices

names(lunch_prices)

lunch_prices <- c(Soup = 3.30, "Main course" = 5.91, Dessert = 2.75)

LETTERS
letters
month.abb
month.name

em1 <- vector("numeric")
em2 <- vector("character")
em3 <- vector("logical")

length(em2)

nem1 <- vector("numeric", length = 20)

em1a <- numeric()
em2a <- character()
em3a <- logical()


# Math operations
v1 <- c(1, 2, 3)
v2 <- c(4, 5, 6)

v1 + 3.5

v1 * 2

v1 / 2

v1 - v2

v1 * v2

v1 ^ v2

v3 <- c(7, 8, 9, 10)

v4 <- v1 + v3
v4

TRUE + FALSE
TRUE * FALSE

# Type coercion
c(TRUE, 1.6)

c(5.1, "A frog in the pond.", 1 + 7.5i)

# Vector subsetting
lunch_prices[1]

lunch_prices[c(2, 3)]

lunch_prices[c(FALSE, TRUE, TRUE)]

lunch_prices[c("Main course", "Dessert")]

lunch_prices_alt <- c(3.10, 6.25, 2.90)

# Vector comparison
lunch_prices > lunch_prices_alt

# Factors
f1 <- factor(c("new", "second-hand", "new", "new", "second-hand"),
             levels = c("new", "second-hand"))

f2 <- factor(c("new", "second-hand", "new", "new", "second-hand"))

f3 <- factor(c("new"), levels = c("new", "second-hand"))

f4 <- factor(month.abb, levels = month.abb, order = T)

# Matrices
vec4m <- 1:100

mat1 <- matrix(vec4m, nrow = 10)

mat1 <- matrix(vec4m, ncol = 10)

mat2 <- matrix(vec4m, nrow = 10, byrow = TRUE)

mat3 <- cbind(lunch_prices, lunch_prices_alt)

zeros <- matrix(0, 5, 5)
ones <- matrix(1, 5, 5)
identity <- diag(5)

colnames(mat3) <- c("Prices, Restaurant 1", "Prices, Restaurant 2")

mat3 <- rbind(mat3, colSums(mat3))

rownames(mat3)[4] <- "Total"

# Subsetting matrices
mat3[4, 2]

mat3[c(1,3), 1]

mat3[,1]

mat3[2,]

mat3["Soup", "Prices, Restaurant 1"]

mat3[c(T, F, F, F), c(T,F)]

# Math operations with matrices
mat1 + 4.8
mat1 * 3
mat1 %% 3
mat1 ^ 2

mat1 + mat2
mat1 / mat2
mat1 %% mat2
mat1 ^ mat2

# Arrays
arr1 <- array(1:343, dim = c(7, 7, 7))

# Lists
list1 <- list(TRUE, "France", 21)

list1

names(list1) <- c("Active", "Country", "Age")

list2 <- list(Appearances = c(T,F,T,T,T), Attributes = list1)

# Subsetting lists
list3 <- list2[1]

class(list3)

vec_from_lst <- list2[[1]]

class(vec_from_lst)

vec_from_lst2 <- list2$Appearances

list2[[1]][1]
list2$Appearances[1]

list1[c(FALSE, TRUE, FALSE)]

# Extending lists
teams <- c("Real Madrid", "Barcelona", "Valencia")
list2$clubs <- teams

# Data frames
m4df <- matrix(1:9, nrow = 3)
df1 <- as.data.frame(m4df)

names(df1) <- c("One", "Two", "Three")
df1$Four <- c(10, 11, 12)
totals <- data.frame(matrix(c(6, 15, 24, 33), nrow = 1, ncol = 4))
names(totals) <- colnames(df1)
df2 <- rbind(df1, totals)

# Dates and times
Sys.Date()

class(Sys.Date())

date.var1 <- as.Date("2024-11-15")

date.var2 <- as.Date("2024/11/15")

Sys.getlocale()

Sys.setlocale("LC_ALL", "bg_BG.utf8")

as.Date("07/31/2024", format = "%m/%d/%Y")
as.Date("26 декември 2024 г.", format = "%d %B %Y г.")
as.Date("29мар2024", format = "%d%b%Y")

as.integer(as.Date("2017-01-21"))

as.Date(0)

as.Date(42658, origin = "1899-12-30")

as.Date("2024-11-12") - as.Date("1945-05-09")

difftime(as.Date("2024-11-12"), as.Date("1945-05-09"), unit = "weeks")

date_seq1 <- seq(as.Date("2024-10-10"), by = "days", length = 20)

date_seq2 <- seq(from = as.Date("2024-10-10"), to = as.Date("2025-10-10"),
                 by = "3 weeks")

# chron
dates1 <- dates(c("03/21/2002", "04/26/12", "01/11/14", "01/28/1915", "02/10/2016"))
dates1

dates1[5] - dates1[4]

times1 <- times(c("20:05:21", "19:29:59", "11:13:31", "10:29:14",
                  "06:48:02"))
times1

times1[2] - times1[4]

datestimes1 <- chron(dates = dates1, times = times1)
datestimes1

datestimes1[2] + 3.5

Sys.time()

posdate1 <- as.POSIXct("2024-10-15")
posdate2 <- as.POSIXct("2024/10/16")
posdate1
posdate2

posdate3 <- as.POSIXct("2024-10-15 15:45:37")
posdate3

posdate4 <- as.POSIXct("03 януари 2018 г., 5 ч., 13 мин., 45 сек.",
                       format = "%d %B %Y г., %H ч., %M мин., %S сек.",
                       tz = "CET")
posdate4

# lubridate
detach(package:chron, unload = T)
today()
now()

now(tzone = "America/Montreal")

lub_date1 <- ydm(20241403)

class(lub_date1)

lub_date2 <- ymd("Текущата дата е 2024 11 14")

lub_date3 <- dmy_hm("14/11-2024 17:21")
class(lub_date3)

lub_date4 <- yq("2024-Q4")
class(lub_date4)

per1 <- years(2) + minutes(35)
class(per1)

per2 <- period(years = 2, minutes = 35)
per2

dur1 <- dyears(1)
dur1

dur2 <- duration(years = 1)
dur2

lub_date5 <- now()
lub_date5
with_tz(lub_date5, tzone = "Japan")
force_tz(lub_date5, tzone = "Japan")

int1 <- now() %--% (now() + ddays(10))
int1

int_length(int1)