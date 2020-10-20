install.packages(c("remotes","pROC","naivebayes")) 
remotes::install_github("jtextor/bayesianNetworks")
library( bayesianNetworks )

install.packages("daggity")
library(daggity) 


rm(list = ls())
setwd("~/Desktop/Study/Bayesian_Networks/assignment/Bike-Sharing-Dataset")

#load .csv file 
resultTable <- read.csv("day.csv", header=TRUE)
resultTable

summary(resultTable)