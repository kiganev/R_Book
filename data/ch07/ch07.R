# Author: Kaloyan Ganev
# Code to accompany Chapter 7 of
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

nobs <- 100000
mean1 <- 5
sd1 <- 7
idx1 <- 1:nobs

vec_draws <- numeric()
vec_avgs <- numeric()
for (i in 1:nobs){
  draw <- rnorm(1, mean = mean1, sd = sd1)
  vec_draws[i] <- draw
  vec_avgs[i] <- mean(vec_draws)
}

df1 <- as.data.frame(cbind(idx1, vec_avgs, 
                           mean1 = rep(mean1, nobs)))

Pop <- as.data.frame(rchisq(10000000, 5))
colnames(Pop) <- "Pop"

true_mean <- mean(Pop$Pop)
true_var <- var(Pop$Pop)

Pop_hist <- ggplot(Pop, aes(x = Pop)) + 
  geom_histogram(bins = 3163,
                 col = "red",
                 fill = "red",
                 alpha = 0.5) + 
  theme_minimal()

Pop_hist

n <- 20000
m <- 10000

df1 <- as.data.frame(c(1:n))
colnames(df1) <- "idx"

for(i in 1:m){
  df1[[paste0("Smpl",i)]] <- sample(Pop$Pop, n)
}

X_bars <- colMeans(df1[,2:ncol(df1)])

Y_n <- sqrt(n) * (X_bars - true_mean)/ sqrt(true_var)
Y_n <- as.data.frame(Y_n)

Y_n_hist <- ggplot(Y_n, aes(x = Y_n)) + 
  geom_histogram(bins = 100,
                 col = "red", 
                 fill = "red",
                 alpha = 0.5) + 
  theme_minimal()

Y_n_hist

mean(Y_n$Y_n)
var(Y_n$Y_n)
