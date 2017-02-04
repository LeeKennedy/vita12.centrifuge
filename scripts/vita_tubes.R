# Clean Up environment ---------------------------------------------------
rm(list=ls())

# Packages ---------------------------------------------------------------
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(equivalence)

# Data Input -------------------------------------------------------------
vita <- read_excel("~/Documents/GitHub/vita12.centrifuge/data/VITA12 CENTRIFUGE TUBES TRIAL STUDY.xlsx", 
                   skip = 1)

# Data Cleaning ----------------------------------------------------------

vita <- vita[-1,c(-5,-8)]
colnames(vita)[1:6] <- c("Analyst", "Date", "Batch", "Sample", "Mojonnier", "Centrifuge")

vita[,5:6] <- sapply(vita[,5:6], as.numeric)
vita2 <- vita %>% 
        fill(Analyst, Date, Batch) %>% 
        na.omit(Sample)

# Visualising

vita_plot <- ggplot(vita2, aes(x=Mojonnier, y=Centrifuge)) +
        geom_point(shape=21, size=4, col="black", fill="cornflowerblue") +
        geom_abline(slope = 1, intercept = 0, lty=2, col="red")
vita_plot

# stats ------------------------------------------------------------------

t.test(vita2$Centrifuge, vita2$Mojonnier, paired = TRUE)
tost(vita2$Centrifuge, vita2$Mojonnier, epsilon = 250, paired = TRUE)
