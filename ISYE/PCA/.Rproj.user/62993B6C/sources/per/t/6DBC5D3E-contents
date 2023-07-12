library(tidyverse)
setwd("C:/Users/Muhammad/ISYE/hw6")
crime_data<- read.table("uscrime.txt", header = TRUE)

#Regression model Dependent variable is Crime and indendent variables are M,So,Ed,P01....Time.
regression_model<- lm(Crime~., crime_data)
summary(regression_model)
M<- 14.0
So<- 0
Ed<-10.0
Po1<-12.0
Po2<- 15.5
LF<-0.640
M.F<-94.0
Pop<-150
NW<-1.1
U1<-0.120
U2<-3.6
Wealth<-3200
Ineq<-20.1
Prob<-0.04
Time<-39.0

pred_grid <- expand.grid(M = M, So = So, Ed=Ed, Po1=Po1, Po2=Po2,LF=LF, M.F = M.F, Pop = Pop, NW= NW, U1= U1, U2=U2, Wealth=Wealth, Ineq=Ineq,Prob=Prob, Time=Time)

prediction= predict(regression_model, newdata = pred_grid)
prediction

#I wouldn't rely on this model at all because data is too small and adjusted R is too small too. We could use less predictors even then 47 observations are just too low.
regression_model$coefficients
