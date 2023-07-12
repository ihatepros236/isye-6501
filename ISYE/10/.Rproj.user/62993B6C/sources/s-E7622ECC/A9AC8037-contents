library(MASS)
library(glmnet)

setwd("C:/Users/Muhammad/ISYE/hw8")
data <- read.table("uscrime.txt", stringsAsFactors = FALSE, header = TRUE)

#Q11.1 1:

reg1 <- lm(Crime ~., data = data)

#Training for stepwise regression, validation through AIC 
stepwise <- stepAIC(reg1, direction = "both", trace = FALSE)

#Best model using stepwise regression using both backward and forward selection
summary(stepwise)

#Data too small otherwise I would split the data into training, validation and testing set

#sacling data
data_scaled <- scale(data, center = TRUE, scale = TRUE)
x<- data_scaled[,1:15]
y<- data_scaled[,16]

# Q11.1.2
fit_lasso <- glmnet(as.matrix(x), as.matrix(y), family="mgaussian", alpha = 1)
summary(fit_lasso)
plot(fit_lasso)

cv.glm <- cv.glmnet(as.matrix(x), as.matrix(y), alpha=1)
plot(cv.glm)
#best value of lambda:
best_lambda <- cv.glm$lambda.min
best_lambda

