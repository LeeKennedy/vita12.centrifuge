# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

# Functions --------------------------------------------------------------

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

# Data Input -------------------------------------------------------------
vita <- read_excel("C:/Users/leekennedy/Desktop/VITA12 CENTRIFUGE TUBES TRIAL STUDY.xlsx", 
                   skip = 1)
colnames(vita)[1:4] <- c("Analyst", "Date", "Batch", "Sample")
vita <- vita[-1,c(-5,-8)]
vita[,5:6] <- sapply(vita[,5:6], as.numeric)

# Data Cleaning ----------------------------------------------------------


# Visualising