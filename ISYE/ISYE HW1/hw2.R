
#Question2.2

library(kernlab)
library(kknn)


setwd("C:/Users/Muhammad/ISYE")

data<-read.table("credit_card_data-headers.txt", header = TRUE)

#ksvm model
model <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]), C=0.001388191625, scaled =TRUE, kernel="vanilladot", type = "C-svc")

#coeeficients of our svm
a <- colSums(model@xmatrix[[1]] * model@coef[[1]])
a
#intercept
a0<- model@b
a0
#Assigning number of correct predictions to pred
pred<- predict(model,data[,1:10])
Accuracy_of_model <- sum(pred==data[,11])/nrow(data)
Accuracy_of_model
# Our model Accuracy is 86.39%

#Q2.2.3
  
accuracy_function = function(x){
  
  prediction<- matrix(nrow=nrow(data),ncol=1) 
  for (i in 1:nrow(data)){
    knn_model=kknn(R1~A1+A2+A3+A8+A9+A10+A11+A12+A14+A15,data[-i,],data[i,],k=x, scale = T)
    # Rounding off by 50% threshold 
    prediction[i,1] = as.integer(fitted(knn_model)+0.5) 
  }
  accuracy = sum(prediction[,1] == data[,11]) / nrow(data)
  return(accuracy)
}

K_test <- matrix(ncol=1, nrow=27)
#Usually value of K is root of number of sample size, hence K would be 25 or 26, to confirm lets loop all the number till 27
for (i in 1:27){
  K_test[i,1] = accuracy_function(i) 
}
ind <- which.max(K_test)
ind
K_test[ind,1]
#I expected value of K to be approximately around 25 but that isn't the case. K is 12 in this case, as accuracy maximizes to 85.32%

index_vector<- matrix(1:27,ncol=1,nrow=27)
plot(index_vector[,1],K_test[,1])

accuracy_function(57)
