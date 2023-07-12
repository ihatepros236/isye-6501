library(tidyverse)
setwd("C:/Users/Muhammad/ISYE/hw4")
library(outliers)
library(knitr)
library(qcc)
#Question 5.1
crime_data <- read.table("uscrime.txt", header = TRUE)

outlier_test <- grubbs.test(crime_data$Crime, type=11)
outlier_test$alternative

#Question 6.2

#Reading Data:
temp_data<- read.table("temps.txt", header = TRUE)

#Cleaning Data
temp_data <- data.frame(temp_data)
names(temp_data) = gsub(pattern = "X", replacement = "", x = names(temp_data))

#Calculating yearly mean
mean_1<-colMeans(temp_data[,2:21])
temp_data2 <-select(temp_data,2:21)
temp_data2 <-t(temp_data2)
temp_data2<-data.frame(temp_data2)

temp_data2$mean <-mean_1
#Calculating standard deviation
sd_1<-apply(temp_data2[ ,1:123],1,sd)
temp_data2$SD<-sd


Change_graphs <-list() 

violations <- list()

# Checking cusum with decision interval of 3 standard deviations, I don't want outliers or data above 3SD

temp_3= select(temp_data,2:21)

for (i in 1:20){
  Change_graphs[[i]] <- cusum(temp_3[,i], center=temp_data2$mean[i], 
                              std.dev = temp_data2$SD[i], decision.interval=3, se.shift=3, plot = TRUE)
  violations[[i]] <- Change_graphs[[i]]$violations$lower
}

#from graphs we can tell there were 16 events where graphs passed our threshold
threshold_passed <-16
index_for_cusum<- list()
#finding index for times temp dropped below threshold
for (i in 1:16) {
  index_for_cusum[i]<- max(violations[[i]])
}

index_for_lowest<-index_for_cusum[is.finite(index_for_cusum)]

#Days from Jul 1st when temperature starts cooling off

index_for_lowest
min(index_for_lowest)
max(index_for_lowest)
# Hence, the temperature starts cooling from minimum of 97 days from July 1st and maximum of 122 days, which is between October 6th and 31st.
#In Atlanta summers end in October according to our model.


#Q6.2.2
#Finding mean of all years
mean_of_all= mean(temp_data2$mean)

#Standard deviation of yearly average temperatures 
SD_ALL= sd(temp_data2$mean)

Change_graphs1 <-list() 

violations1 <- list()

# I am going with decision interval of 4 SD because I want to be sure whether there is an actual increase 



Change_graphs1<- cusum(temp_data2$mean, center=mean_of_all, 
                              std.dev = SD_ALL, decision.interval=5, se.shift=3, plot = TRUE)
violations1<- Change_graphs$violations1$lower

#According to my model average temperature isn't increasing.


