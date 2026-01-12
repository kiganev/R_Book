# Author: Kaloyan Ganev
# Code to accompany Chapter 4 of
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

# if
x <- sample(1:6, 1)
if(x <= 2){
  y <- 10
}

# if-else
if (x <= 2){
  y <- 10
} else {
  y <- 20
}

# Nested if-else
if (x <= 2){
  y <- 10
} else if (x > 2 && x <= 5){
  y <- 15
} else {
  y <- 20
}

# Single or double & and |
x <- c(TRUE, FALSE, TRUE)
y <- c(FALSE, FALSE, TRUE)

x & y
x | y

x[1] && y[1]
x[1] || y[1]  

# Negation
if (!(x > 10)){
  z <- 300
}

ifelse(c(1,0,1,0,0,1), "Heads", "Tails")

x <- 7
y <- 4
math_oper <- "exponentiate"
switch(math_oper,
       add = sum(x, y),
       subtract = x - y,
       multiply = x * y,
       divide = x / y,
       exponentiate = x ^ y,
       "Operation not defined")

switch(3, "Bachelor", "Master", "PhD")

# for loops
vecnums <- c(1,3,5,7,9,11)
for (i in vecnums){
  print(i%%3)
}

vec1 <- character()
for (i in 1:length(LETTERS)){
  vec1[i] <- paste(LETTERS[i],i, sep = "")
}

vec1

array1 <- array(dim = c(5,10,15))
for (i in 1:dim(array1)[1]){
  for (j in 1:dim(array1)[2]){
    for (k in 1:dim(array1)[3]){
      array1[i,j,k] <- i*j*k
    }
  }
}

x <- 0
while (x <= 33) {
  z <- sample(1:6, 1)
  x <- x + z
}

uname <- character()
while (length(uname) == 0){
  cat("Please enter your user name:")
  uname <- scan(what=character(),nmax=1,quiet=TRUE)
}

uname <- character()
while (length(uname) == 0){
  cat("Please enter your user name:")
  uname <- scan(what=character(),nmax=1,quiet=TRUE)
}

int1 <- 10000
int2 <- int1
div_by <- 2437
while (int1 > 0){
  if (int1%%div_by == 0){
    print(paste0("The largest integer between 0 and ", int2, 
                "divisible by ", div_by, " is ", int1, "."))
    break
  }
  int1 = int1 - 1
}

int3 <- 0
vec_odd <- integer()
for (i in c(1:100)){
  if(i%%2 == 0){
    next
  }
  vec_odd <- c(vec_odd,i)
}

name <- character()
repeat{
  cat("Enter your username:")
  uname <- scan(what=character(), nmax=1, quiet=TRUE)
  if(length(uname) != 0){
    break
  }
}

# apply family
m1 <- matrix(1:100, nrow = 10)
apply(m1, MARGIN = 1, mean)
apply(m1, MARGIN = 2, sum)
apply(m1, MARGIN = c(1,2), sqrt)

object1 <- list(arr1 = array(1:1000, dim = c(10,10,10)),
                vect1 = 1:10, mat1 = matrix(1:100, nrow = 10))
lapply(object1, sum)

sapply(object1, sum)

vapply(as.complex(-10:10), FUN = sqrt, FUN.VALUE = 1i)

vec1 <- seq(2, 70, length.out = 10)
vec2 <- seq(5, 60, length.out = 10)
mapply(min, vec1, vec2)

rapply(object1, mean)
rapply(object1, mean, how = "unlist")
rapply(object1, mean, how = "list")

object2 <- list(a1 = c(1:100), b1 = c(101:200), char1 = "Text")
rapply(object2, log, classes = "integer", how = "replace")

m1 = matrix(1:9, 3, 3)
idx1 = matrix(c(1,2,1,3,2,2,1,2,3), 3, 3)
tapply(m1, idx1, sum)

x <- 1:10
idx2 <- rep(c('A1', 'A2', 'A3'), length = 10)
idx3 <- rep(c('B1', 'B2' ,'B3', 'B4'), length = 10)
tapply(x, list(idx2, idx3), function(x) x^2)

y <- c(1:200)
factor1 <- factor(ifelse(y %% 2 == 0, "A", "B"))
sapply(split(y, factor1), mean)

mat1 <- matrix(1:12, nrow = 3)
sweep(mat1, 2, colMeans(mat1), "-")
sweep(mat1, 2, colSums(mat1), "/")

arr1 <-  array(1:18, dim = c(3,3,2))
sweep(arr1, 1, apply(arr1, 1, mean))
