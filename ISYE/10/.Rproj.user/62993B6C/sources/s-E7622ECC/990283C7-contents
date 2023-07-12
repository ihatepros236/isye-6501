
setwd("C:/Users/Muhammad/ISYE/10")
library(dplyr)
library(tidyr)
data <-read.table("breast-cancer-wisconsin.data.txt", stringsAsFactor = FALSE, header = F, sep = ",", na.strings="?")

#Naming columns 
colnames(data) <- c("ID", "Clump_Thickness", "Uniform_Cell_Size", "Uniform_Cell_Shape",
                  "Marg_Adhesion", "Single_Epith_Cell_Size", "Bare_Nuclei", "Bland_Chromatin",
                  "Normal_Nucleoli", "Mitoses", "Class")

#for checking number of  unique classes
#unique(data$Class)
#converting Class variable to categorical data  or factors
data$Class <- as.factor(data$Class)
summary(data)
#missing values in Bare_Nuclei variable

#creating a temporary variable for mean imputation

data1 <- data

#Q14.1.1
#Mean imputation
data1$Bare_Nuclei[is.na(data$Bare_Nuclei)] <- mean(data1$Bare_Nuclei, na.rm = TRUE)
mean(data1$Bare_Nuclei)

#mide Imputation:

#Creating a commonly used mode function (used a generic function recommended)
data2<-data

Mode <- function(x) {                               
  u_var <- unique(x)
  mode <- u_var[which.max(tabulate(match(x, u_var)))]
  mode
}

data2$Bare_Nuclei[is.na(data2$Bare_Nuclei)] <- Mode(data2$Bare_Nuclei[!is.na(data2$Bare_Nuclei)])                 
summary(data2$Bare_Nuclei)

#Q14.1.2
#Regression imputation 
data3 <- data

#removing all data with na
data3.1 <-drop_na(data3)

#Linear Model for predicting Bare nuclie value for Na value
model <- lm(Bare_Nuclei~Clump_Thickness+Uniform_Cell_Size+Uniform_Cell_Shape+Marg_Adhesion+Single_Epith_Cell_Size+Bland_Chromatin+Normal_Nucleoli+Mitoses
            ,data=data3.1 )


predicted_Na_value <- predict(model,newdata=data3[which(is.na(data3$Bare_Nuclei),),]) 


data_3.1a <- data3
#replacing Na values with predicted values in regression
data_3.1a[which(is.na(data3$Bare_Nuclei)),]$Bare_Nuclei <- as.integer(predicted_Na_value)
summary(data_3.1a)

#Q14.1.3
#Imputation with Reg and Perturbation
#total number of Na values
t_val= sum(is.na(data$Bare_Nuclei))
t_val
#Generating random number using predicted values from regression
P <- as.integer(abs(rnorm(t_val, mean = predicted_Na_value, sd = sd(predicted_Na_value))))

data4 <- data
data4[which(is.na(data3$Bare_Nuclei)),]$Bare_Nuclei<- P
summary(data4)
