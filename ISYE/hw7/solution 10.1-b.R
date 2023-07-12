# -------------------- Code for Question 10.1-b -----------------------------
# Clear environment
setwd("C:/Users/Muhammad/ISYE/hw7")
rm(list = ls())


# Setting the random number generator seed so that our results are reproducible
# (Your solution doesn't need this, but it's usually good practice to do)

set.seed(1)


# ---------------------------- Data manipulation -------------------------------------

# First, Read in the data
#

data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#
# optional check to make sure the data is read correctly
#



library(randomForest)

set.seed(1)



# Grow the random tree and set the number of predictors that 
# want to consider at each split of the tree (numpred)

numpred <- 4
rf.data <- randomForest(Crime~., data = data, mtry = numpred, importance = TRUE)
rf.data

## Call:
## randomForest(formula = Crime ~ ., data = data, mtry = numpred,      importance = TRUE) 
## Type of random forest: regression
## Number of trees: 500
## No. of variables tried at each split: 4
## 
## Mean of squared residuals: 82952
## % Var explained: 43.3

# Note that the argument "ntree" can be used to set the amount
# of trees that are grown.



# Calculate SSres of the random forest model

yhat.rf <- predict(rf.data)
SSres <- sum((yhat.rf-data$Crime)^2)



# Plot of actual vs. predicted crime values

plot(data$Crime, yhat.rf)
abline(0,1)



# Plot residuals

plot(data$Crime, scale(yhat.rf - data$Crime))
abline(0,0)



# Calculate SStot and R-squared of this model

SStot <- sum((data$Crime - mean(data$Crime))^2)
R2 <- 1 - SSres/SStot
R2

## 0.433

# Now, let's try leave-one-out cross-validation:

SSE <- 0

for (i in 1:nrow(data)) {
  rd <- randomForest(Crime~., data = data[-i,], mtry = numpred, importance = TRUE)
  SSE = SSE + (predict(rd,newdata=data[i,]) - data[i,16])^2
}
1 - SSE/SStot

## 0.423

# So this model looks better than the previous alternatives.
# And, the random forest procedure really has avoided a lot of 
# the overfitting, just like it's advertised to do.
#
# Remember from Q2a that we could find a decent regression model
# for the tree branch Po1 < 7.65, but not for Po1 > 7.65.
#
# Let's see how this random forest model does on those two 
# branches of data.

SSE1 <- 0
SSE2 <- 0
SStot1 <- 0
SStot2 <- 0

for (i in 1:nrow(data)) {
  rd <- randomForest(Crime~., data = data[-i,], mtry = numpred, importance = TRUE)
  if (data[i,]$Po1 < 7.65) {
    SSE1 = SSE1 + (predict(rd,newdata=data[i,]) - data[i,16])^2
    SStot1 = SStot1 + (data[i,16] - mean(data[,16]))^2
  } else {
    SSE2 = SSE2 + (predict(rd,newdata=data[i,]) - data[i,16])^2
    SStot2 = SStot2 + (data[i,16] - mean(data[,16]))^2
  }
}

1 - SSE1/SStot1

## 0.633

1 - SSE2/SStot2

## 0.338

# So, the model works better for Po1 < 7.65 than it doe for Po1 > 7.65,
# just as we found in the regression tree approach.
# And unlike the regression tree apporach, it does give decent
# predictive value even for Po1 > 7.65.


# But there is a real disadvantage too.
# We can't see a real model, because there are many different trees.
# But we can see which variables are most important in the 
# branching overall.
#
# Find two measures of variable importance. We can see that Po1 seems
# to be the most important variable for predictions.
#
# %IncMSE is the amount that the MSE of predictions increases
# if the variable is randomly chosen instead of using its actual value --
# in other words, how important it is for prediction.
#
# IncNodePurity measures how much splitting on it improves the
# "purity" (the similarity of the data points in each leaf)
#
# In both metrics, higher values mean it's more important.

importance(rf.data)

##        %IncMSE IncNodePurity
## M        1.576        202861
## So       2.944         29885
## Ed       4.480        254869
## Po1     12.585       1276186
## Po2      9.872        999054
## LF       3.008        292552
## M.F      0.699        254157
## Pop      1.293        368766
## NW       7.669        537053
## U1       0.843        141118
## U2       2.062        194464
## Wealth   4.396        763293
## Ineq     2.112        229308
## Prob     7.546        689413
## Time     2.435        196348

# It's not surprising, given what we observed in Q2a, where
# Po1 was the primary branching variable.

# Plots of these importance measures

varImpPlot(rf.data)



