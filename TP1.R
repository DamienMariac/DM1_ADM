rm(list = ls())

data <- read.csv("C:/Users/damie/Desktop/MASTER/ADM/tp/tp1/Datagenus.csv", sep=";")
data2 <- data[1 :1000,]

#Question 1

W=diag(1/1000,1000)