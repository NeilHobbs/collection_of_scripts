##CHIC402, Week2, Exercise 3

##Load required packages
library(tidyverse)

setwd("~/Lancaster/CHIC402/CHIC402 D2E3")

##Import dataset, name iris
iris = iris


iris.func <- function(a, b, Width) {
  return(a + b * Width)
}
MSE = function(a, b, Width, Length) {
  mean((Length - iris.func(a,b,Width))^2)
}

param_vals(-5, 5, 0.1)

mse_vals = c()

##attempt at nested loop nested loop
for(i in length(param_vals)){
  for(j in length(param_vals)){
mse_vals = MSE(param_vals[i], param_vals[j], iris$Petal.Width, iris$Petal.Length)
  }
}



