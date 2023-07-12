

library(tidyverse)
library(caret)
library(kernlab)
library(knitr)

setwd("C:/Users/Muhammad/ISYE/hw3")
data<-read.table("credit_card_data-headers.txt", header = TRUE)
data<-as.data.frame(data)

#Q3.1.a
set.seed(100)
#splitting data for training/validation and testing. 80% training data set
indxTrain <- createDataPartition(y = data$R1,p = 0.8,list = FALSE)
training <- data[indxTrain,]
testing <- data[-indxTrain,]

#Pre-processing data i.e scaling for Knn
trainX <- training[,names(training) != "R1"]
Proccessed_Values <- preProcess(x = trainX,method = c("center", "scale"))

#I am choosing k-fold value of 5 beacause our dataset is too small

control <- trainControl(method="cv",number=5)
#training data set
knnFit <- train(as.factor(R1) ~ ., data = training, method = "knn", trControl = control, preProcess = c("center","scale"), tuneLength = 50)

#finding the value of kappa where accuracy maximizes;
index_where_max<-which.max(knnFit$results$Accuracy)
knnFit$results$Accuracy[index_where_max]
k=knnFit$results$k[index_where_max]
k
plot(knnFit)

cat("For final model Accuracy =", knnFit$results$Accuracy[index_where_max], "and k =",k)

#Testing our model on test dataset
knnPredict <- predict(knnFit, newdata = testing)
con<-confusionMatrix(knnPredict, as.factor(testing$R1))
con
overall_accuracy<-con$overall["Accuracy"]
cat("Accuracy on test data set =", overall_accuracy*100)

#Q3.1.b

#Sampling and splitting data for training and testing

index_sample<- sample(1:nrow(data), as.integer(0.75*nrow(data)))
train_data <- data[index_sample,]

#Assigning validation and test data
valid_test_data <- data[-index_sample,]
index_sample2<- sample(1:nrow(valid_test_data), as.integer(0.50*nrow(valid_test_data)))
validation_data<-valid_test_data[index_sample2,]
testing_data<- valid_test_data[-index_sample2,]

#c_value <- seq(1, 100, by=5)
c_value= seq(.1,1,by=0.1)
accuracy1= rep(1,10)
#looping for accuracy for svm model for different c value with increment of 0.1, tried 1 to 100 by 5 increment already
for (i in 1:10) {
  
  #training model on training data
  svm_model = ksvm(as.matrix(train_data[,1:10]),as.factor(train_data[,11]), type = "C-svc",kernel = "vanilladot",C = c_value[i],scaled=TRUE) 
  #testing it on validation data
  prediction1 = predict(svm_model,validation_data[,1:10])
  accuracy1[i] = (sum(prediction1 == validation_data$R1) / nrow(validation_data))*100
}

#which model maximizes 
max_ind<-which.max(accuracy1)
max(accuracy1)
c_max<-c_value[max_ind]
cat("C value which maximizes accuracy =",c_max,"\n")

#retraining model on training dataset with parameter c=0.1, 
#(in this case it really doesn't matter because we got same accuracy with the c values in last sequence I tried.
#however just to show that I know generally we have to rerun our model on training set with the parameter we found for 
#maximizing accuracy in validation data set)

svm_model_test = ksvm(as.matrix(train_data[,1:10]),as.factor(train_data[,11]), type = "C-svc",kernel = "vanilladot",C =0.1,scaled=TRUE) 

Accuracy_on_test = sum(predict(svm_model_test,testing_data[,1:10])==testing_data[,11])/nrow(testing_data)
cat("Accuracy on test data set =", Accuracy_on_test)


#Q4.2
library(datasets)
data(iris)
names(iris) <- tolower(names(iris))
iris_category <- unique(iris[,5])

#Scaling all data set
preproc2 <- preProcess(iris[,1:4], method=c("range"))

normalized_data <- predict(preproc2, iris[,1:4])

model_kmean<-kmeans(normalized_data[,1:4], 3, nstart = 20)
model_kmean


#There is not really a way to tell accuracy  since clustering is unsupervised learning
#We could look at the compactness which is 76.7% or how similar to members within a group are
#We can look at purity but I don't think that is expected here
#More over anything over 3 dimensions would be really hard to determine manually
#We could loop over multiple values of K but since we already know that there are 3 classes it's not necessary,
#however we could gain some extra information but we only have 150 observation so that isn't a good idea since
#it will over-fit and accuracy will probably go down  

# We could visualize for different combination of attributes and their graphs and see how two attributes
# are placed in a category depending on their values, but as we can see after just 3 dimensions we have
#difficulty visualizing, hence usually we could just look at individual cluster means for group and try
#understand intuitively or we could look at visualize 2 or 3 features of interest at a time to study further.

#i.e:

plot(normalized_data[c(1,2)], col=model_kmean$cluster)
plot(iris[c(1,2)], col=iris_category)

#I think for something like 













