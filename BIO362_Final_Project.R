#DSEE Final Project

library(readr)

#J
EJSCREEN <- read.csv("/Users/jamiefleshel/Desktop/F&M/Fall_2024/DSEE/Intro_to_R/Final_Project/EJSCREEN.csv")

#S
EJSCREEN <- read_csv("G:/My Drive/BIO - Final Proyect/EJSCREEN.csv")

library(dplyr)

PennsylvaniaData <- filter(EJSCREEN, ST_ABBREV == "PA")

