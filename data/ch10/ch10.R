# Author: Kaloyan Ganev
# Code to accompany Chapter 10 of
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

library(extrafont)
library(plotrix)
library(lattice)
library(tidyverse)
library(gridExtra)
library(latex2exp)
library(modeest)
library(moments)
library(DescTools)
library(openxlsx)

# Fonts
font_import()

loadfonts()

fonts()
names(pdfFonts())

# Generate some data
z <- rnorm(20)

plot(z)

dev.copy2pdf(file = "../../img/ch10/point.pdf", 
             width = 6, height = 4.5,
             device = cairo_pdf)

inc <- 1000 * runif(50)
cons <- 50 + 0.8 * inc + rnorm(50, mean = 0, sd = 50)

plot(inc, cons,
     col = "red",
     xlab = "Доход",
     ylab = "Потребление",
     main = "Зависимост между потреблението и дохода",
     pch = 20,
     cex = 2,
     cex.axis = 1.2,
     cex.lab = 1.2)

f1 <- factor(sample(c(1:3),length(inc),replace = T))

plot(inc, cons,
     col = f1,
     xlab = "Доход",
     ylab = "Потребление",
     main = "Зависимост между потреблението и дохода",
     pch = 20,
     cex = 2,
     cex.axis = 1.2,
     cex.lab = 1.2)

# Line graphs
plot(cons, type = "l",
     xlab = "Период",
     ylab = "Стойности",
     lty = 2,
     col = "darkseagreen",
     lwd = 2,
     ylim = c(min(cons, inc), max(cons, inc)),
     cex.axis = 1.2,
     cex.lab = 1.2)

lines(inc, 
      lty = "solid",
      col = "darkred")

# Plot math functions
fun1 <- function(x){
  3*x^3 + 2*x^2 - 7*x + 11
}

curve(fun1, -10, 10, 
      lwd = 2,
      col = "red",
      cex.axis = 1.2,
      cex.lab = 1.2)

abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

fun2 <- function(x){
  1000 * cos(x)
}

curve(fun2, -10, 10, 
      add = T, 
      lwd = 2,
      col = "blue")

# Pie charts
pie_data <- c(19, 24, 28, 17, 35)
pie_labels <- c("Ябълки", "Круши", "Череши", "Портокали", "Банани")

pie(pie_data, pie_labels,
    col = rainbow(length(pie_data)), 
    main = "Потребление на плодове",
    cex = 1.2)

pie3D(pie_data, 
      labels = pie_labels,
      explode = 0.2,
      radius = 1.8,
      main = "Потребление на плодове в 3D", 
      labelcex = 1.2, 
      labelcol = "black")

# Bar plots
barplot(inc, 
        border = "darkgreen", 
        col = "orange", 
        xlab = "", 
        ylab = "млн. лв.", 
        main = "Доход",
        cex.axis = 1.2)

barplot(inc, 
        horiz = T,
        border = "darkgreen", 
        col = "orange", 
        xlab = "млн. лв.", 
        ylab = "", 
        main = "Доход")

bar_data1 <- sample(30:50, 3)
bar_data2 <- sample(30:40, 3)

bar_data_matrix <- cbind(bar_data1, bar_data2)

par(mfrow = c(1, 2))

barplot(bar_data_matrix, 
        col = c("darkred", "darkorange", "yellow"),
        names.arg = c("Показател 1", "Показател 2"),
        beside = T,
        cex.axis = 1.2,
        cex.names = 1.2)

barplot(bar_data_matrix, 
        col = c("darkred", "darkorange", "yellow"),
        names.arg = c("Показател 1", "Показател 2"),
        beside = F,
        cex.axis = 1.2,
        cex.names = 1.2)

# Histograms
set.seed(2024)
z <- rnorm(500)
hist(z, 
     border = "darkblue", 
     col = "orange", 
     breaks = 20, 
     freq = T,
     main = "Хистограма на z",
     cex.axis = 1.2,
     cex.lab = 1.2)

dev.copy(file = "../../img/ch10/hist.pdf", width = 6, height = 4.5,
         device = cairo_pdf)
dev.off()

# Box plots
boxplot(z, 
        col = "#D85625",
        cex.axis = 1.2)

# Pairs plots 
iris.df <- as.data.frame(iris)
pairs(iris.df[,1:4],
      col = iris.df[,5],
      cex.axis = 1.2)

# 3D surface
z_func <- function(x,y){
  -7 * exp(-x^2 - y^2) * x * y
}
x <- seq(from = -3, to = 3, by = 0.1)
y <- seq(from = -3, to = 3, by = 0.1)
z <- outer(x, y, z_func)

persp(x, y, z,
      col="green",
      theta = 110,
      phi = 30,
      cex.lab = 1.2)

# Multiple graphs
par(mfrow = c(2,2))
plot(rnorm(100), type = "p")
plot(rnorm(100), type = "l")
plot(rnorm(100), type = "s")
plot(rnorm(100), type = "b")

pos_m <- matrix(c(1,1,2,3), nrow = 2)
layout(pos_m)
plot(rnorm(100), type = "p")
plot(rnorm(100), type = "l")
plot(rnorm(100), type = "s")

par(mfrow = c(1,1))

# Save to file
jpeg("linegraph1.jpg",
     width = 800,
     height = 600,
     units = "px")
plot(rnorm(100), type = "l")
dev.off()

png("linegraph1.png",
     width = 800,
     height = 600,
     units = "px")
plot(rnorm(100), type = "l")
dev.off()

pdf("linegraph1.pdf",
    width = 8,
    height = 6)
plot(rnorm(100), type = "l")
dev.off()

svg("linegraph1.svg",
    width = 8,
    height = 6)
plot(rnorm(100), type = "l")
dev.off()

plot(rnorm(100), type = "b")
dev.copy(png, 
         "bothgraph.png",
         width = 800,
         height = 600,
         units = "px")
dev.off()

dev.copy(svg,
         "bothgraph.svg",
         width = 8,
         height = 6)
dev.off()

dev.copy2pdf(file = "bothgraph.pdf",
             width = 8,
             height = 6)

# Lattice plots
df1 <- as.data.frame(cbind(inc, cons)) %>% 
  mutate(index = 1:length(inc))

xyplot(cons ~ inc, data = df1)

xyplot(cons ~ index,
       type = "l",
       data = df1)

barchart(cons ~ index, 
         horizontal = FALSE, 
         data = df1, 
         main="Потребление", 
         col = "lightblue",
         scales=list(x=list(at=seq(1, 50, 5))))

rnd1 <- rnorm(1000)
bwplot(rnd1, 
       col = "red", 
       fill = "green")

histogram(rnd1,
          col = "orange")

densityplot(rnd1, lwd = 2)

qqmath(rnd1)

# ggplot2
ggplot(data = df1)

ggplot(df1, aes(x = inc, y = cons))

ggplot(df1, aes(x = inc, y = cons)) + 
  geom_point()

ggplot(df1, aes(x = inc, y = cons)) + 
  geom_point(size = 4, col = "darkred", alpha = 0.5) + 
  xlab("Доход") + 
  ylab("Потребление") + 
  ggtitle("Зависимост между потреблението и дохода")

ggplot(df1, aes(x = inc, y = cons)) +
  geom_point(size = 1, col = "darkred", alpha = 0.5) +
  xlab("Доход") +
  ylab("Потребление") +
  ggtitle("theme_bw()") +
  theme_bw()

windowsFonts()

ggplot(df1, aes(x = inc, y = cons)) + 
  geom_point(size = 4, col = "darkred", alpha = 0.5) + 
  xlab("Доход") + 
  ylab("Потребление") + 
  ggtitle("Зависимост между потреблението и дохода") + 
  theme(axis.title.x = element_text(size=16, family = "Cambria"),
        axis.title.y = element_text(size=16, family = "Cambria"),
        axis.text.x = element_text(size=14, family = "Cambria"),
        axis.text.y = element_text(size=14, family = "Cambria"),
        plot.title = element_text(size=18, family = "Cambria"))

ggsave("../../img/ch10/scatter3_ggplot.pdf",
       width = 6,
       height = 4.5,
       unit = "in",
       device = cairo_pdf)

t1_bw <- ggplot(df1, aes(x = inc, y = cons)) + 
  geom_point(size = 1, col = "darkred", alpha = 0.5) + 
  xlab("Доход") + 
  ylab("Потребление") + 
  ggtitle("theme_bw()") + 
  theme_bw()

t2_dark <- ggplot(df1, aes(x = inc, y = cons)) + 
  geom_point(size = 1, col = "darkred", alpha = 0.5) + 
  xlab("Доход") + 
  ylab("Потребление") + 
  ggtitle("theme_dark()") + 
  theme_dark()

t3_light <- ggplot(df1, aes(x = inc, y = cons)) + 
  geom_point(size = 1, col = "darkred", alpha = 0.5) + 
  xlab("Доход") + 
  ylab("Потребление") + 
  ggtitle("theme_light()") + 
  theme_light()

t4_minimal <- ggplot(df1, aes(x = inc, y = cons)) + 
  geom_point(size = 1, col = "darkred", alpha = 0.5) + 
  xlab("Доход") + 
  ylab("Потребление") + 
  ggtitle("theme_minimal()") + 
  theme_minimal()

themes_all <- grid.arrange(t1_bw, t2_dark, t3_light, t4_minimal)

ggsave("../../img/ch10/themes_ggplot.pdf",
       themes_all,
       width = 6,
       height = 4.5,
       unit = "in",
       device = cairo_pdf)

theme_set(
  theme_minimal() +
    theme(
      axis.title = element_text(size = 16),
      axis.text  = element_text(size = 14),
      legend.text = element_text(size = 16),
      legend.title = element_text(size = 16)
    )
)

lines_gg <- ggplot(df1, aes(x = index)) + 
  geom_line(aes(y = inc), col = "red")

lines_gg

ggsave("../../img/ch10/lines_ggplot.pdf",
       lines_gg,
       width = 6,
       height = 4.5,
       unit = "in",
       device = cairo_pdf)

lines2_gg <- ggplot(df1, aes(x = index)) + 
  geom_line(aes(y = inc), col = "red") + 
  geom_line(aes(y = cons), col = "blue", linetype = "dashed")

lines2_gg

lines3_gg <- ggplot(df1, aes(x = index)) + 
  geom_line(aes(y = inc, col = "Доход")) + 
  geom_line(aes(y = cons, col = "Потребление"), linetype = "dashed") +
  scale_color_manual("", values = c("red", "blue")) +
  xlab("") + 
  ylab("") +
  theme(legend.position = "bottom")

lines3_gg

df2 <- data.frame(x = seq(-3, 3, by = 0.1))

fun1_gg <- ggplot(df2, aes(x = x)) + 
  geom_function(fun = fun1, col = "red") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed")

fun1_gg

df1 <- df1 %>% 
  mutate(country = f1)

bar_gg <- ggplot(df1, aes(x = country)) + 
  geom_bar(col = "orange", fill = "orange")

bar_gg

col_gg <- ggplot(df1, aes(x = index)) + 
  geom_col(aes(y = inc),
           col = "red", 
           fill = "orange")

col_gg

ggsave("../../img/ch10/col_ggplot.pdf",
       col_gg,
       width = 6,
       height = 4.5,
       unit = "in",
       device = cairo_pdf)

col2_gg <- ggplot(df1, aes(x = index)) + 
  geom_col(aes(y = inc), col = "red", fill = "orange") +
  coord_flip()

col2_gg

df3 <- data.frame(counts = pie_data, fruit = pie_labels)

pie_gg <- ggplot(df3, aes(x = "", y = counts, fill = fruit)) + 
  geom_col(col = "white") + 
  coord_polar("y", start = 0) + 
  scale_fill_manual("",
                    values = c("darkred", "darkblue", 
                               "orange", "darkgreen",
                               "yellow")) +
  theme_void() + 
  theme(legend.position = "bottom",
        legend.text  = element_text(size = 16))

pie_gg

hist_gg <- ggplot(iris.df, aes(x = Sepal.Length)) + 
  geom_histogram(bins = 13,
                 col = "orange",
                 fill = "darkgreen")

hist_gg

hist2_gg <- ggplot(iris.df, aes(x = Sepal.Length, after_stat(density))) + 
  geom_histogram(bins = 13,
                 col = "orange",
                 fill = "darkgreen")

hist2_gg

box_gg <- ggplot(iris.df, 
                 aes(x = Species, 
                     y = Petal.Length, 
                     fill=Species)) +
  geom_boxplot(alpha=0.5) + 
  scale_fill_manual("", values = c("darkred", "darkgreen", "orange")) + 
  xlab("") + 
  ylab("") +
  theme(legend.position = "bottom")

box_gg


smooth_gg <- ggplot(df1, aes(x = inc, y = cons)) + 
  geom_point(col = "darkblue", alpha = 0.2) +
  stat_smooth(method = lm, 
              formula = y ~ poly(x,2), 
              level = 0.95,
              col = "red")
  
smooth_gg

facets_gg <- ggplot(df1, aes(x = inc, y = cons, col = country)) + 
  geom_point(alpha = 0.5, size = 3) + 
  facet_wrap(~ country, nrow = 3) + 
  theme(legend.position = "bottom")

facets_gg

annot_gg <- ggplot(iris.df, 
                   aes(x = Sepal.Width, y = Sepal.Length, col = Species)) +
  geom_text(aes(x = Sepal.Width, 
                y = Sepal.Length, 
                label = Species, 
                color = Species)) + 
  theme(legend.position = "bottom")

annot_gg

annot2_gg <- ggplot(iris.df, 
                   aes(x = Sepal.Width, y = Sepal.Length, col = Species)) +
  geom_label(aes(x = Sepal.Width, 
                y = Sepal.Length, 
                label = Species, 
                color = Species)) + 
  theme(legend.position = "bottom")

annot2_gg

annot3_gg <- ggplot(df1, aes(x = inc, y = cons)) + 
  geom_point(col = "darkblue", alpha = 0.2) +
  stat_smooth(method = lm, 
              formula = y ~ poly(x,2), 
              level = 0.95,
              col = "red") + 
  annotate("text",
           x = 750, 
           y = 400, 
           label = "Полиномна регресия",
           size = 6,
           col = "blue")

annot3_gg

annot4_gg <- ggplot(df1, aes(x = index)) + 
  geom_line(aes(y = inc), col = "red") + 
  xlab("") +
  ylab("") +
  annotate("rect", 
           xmin = 40, xmax = 45, 
           ymin = 0, ymax = 1000,
           alpha = 0.2)

annot4_gg

# Descriptive stats
x_smpl <- c(1, 3, 14, 7, 18)
x_bar <- mean(x_smpl)

x_smpl2 <- c(1, 3, 14, 7, 18, NA)
x_bar2 <- mean(x_smpl2)

x_bar2 <- mean(x_smpl2, na.rm = T)

x_smpl3 <- c(1, 3, 14, 7, 18, 3, 7, 19, 2)
mlv(x_smpl3, method = "mfv")

sort(x_smpl3)
median(x_smpl3)

x_smpl4 <- c(1, 3, 14, 7, 18, 3, 7, 19, 2, 6)

sort(x_smpl4)
median(x_smpl4)

min(x_smpl4)

max(x_smpl4)

range(x_smpl4)

var(x_smpl)

sd(x_smpl)

moments::skewness(x_smpl)
DescTools::Skew(x_smpl, method = 1)

DescTools::Skew(x_smpl, method = 2)

modeest::skewness(x_smpl)
DescTools::Skew(x_smpl, method = 3)

moments::kurtosis(x_smpl)
DescTools::Kurt(x_smpl, method = 1) + 3

DescTools::Kurt(x_smpl, method = 2) + 3

DescTools::Kurt(x_smpl, method = 3) + 3

# Covariance and correlation
df5 <- read.xlsx("cov_cor_data.xlsx")

cov(df5$x, df5$y)

cov(df5)

cov(df5$x, df5$y, method = "spearman")

cov(df5$x, df5$y, method = "kendall")

cor(df5$x, df5$y)

cor(df5)

cor(df5$x, df5$y, method = "spearman")

cor(df5$x, df5$y, method = "kendall")