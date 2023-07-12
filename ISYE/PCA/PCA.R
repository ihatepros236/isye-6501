library(tidyverse)
setwd("C:/Users/Muhammad/ISYE/PCA")
library(pls)


set.seed(1000)

data <-  read.table("uscrime.txt", header = TRUE)

pcr_model <- pcr(Crime~., data= data, scale = TRUE, validation="CV")

summary(pcr_model)
validationplot(pcr_model)
pcr_model$coefficients

compnames(pcr_model, comps = 1:7, explvar = TRUE)
pcr_model$scores
