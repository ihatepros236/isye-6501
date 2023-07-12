library(tidyverse)
library(stats)
library(smooth)

#Q7.2
set.seed(1)

setwd("C:/Users/Muhammad/ISYE/Hw5")

temp_data<- read.table("temps.txt", header = TRUE)
names(temp_data) = gsub(pattern = "X", replacement = "", x = names(temp_data))
temp_data <- data.frame(unlist(temp_data[,2:21]))
ts_temp= ts(temp_data, frequency = 123, start=1996)
summary(ts_temp)
ts.plot(ts_temp)

Model_HW_Mult<-HoltWinters(ts_temp, seasonal="multiplicative")
summary(Model_HW_Mult)
plot(Model_HW_Mult)

Model_HW_ADD<-HoltWinters(ts_temp, seasonal="additive")
summary(Model_HW_ADD)
plot(Model_HW_ADD)

cat("Sum of squared Errors in Multiplicative model",Model_HW_Mult$SSE,"\n")
cat("Sum of squared Errors in Addititve model",Model_HW_ADD$SSE,"\n")

#Additive model has smaller SSE hence I am choosing additive seasonality, better way to do it would be using cross-validation but for now I am choosing seasonality as additive
#Lets visualize the trend to understand if the temperature is increasing 
plot(fitted(Model_HW_ADD))
# We can observe that there is no visible change in trend over time in the temperature 
# I think one of the reason why it's hard to see the change over time in temperature is because the temperature is increasing very slowly
# and the changes would be very minute, additional our data is too small to capture that since there are random fluctuation in yearly temperature as well such as heatwaves etc.

#Let us further use cum-sum to determine if there is change is seasonality over the years

Seasonality_data<- matrix(Model_HW_ADD$fitted[,4],nrow = 123)

temp_data1<- read.table("temps.txt", header = TRUE)
names(temp_data1) = gsub(pattern = "X", replacement = "", x = names(temp_data1))

dim(Seasonality_data)
dim(temp_data1)
colnames(Seasonality_data)<-c(1997:2015)
days <- temp_data1[,1]
rownames(Seasonality_data)<-days

cumsum_data <- data.frame(matrix(0, nrow=123, ncol=19))
rownames(cumsum_data) <-days
colnames(cumsum_data)<-c(1997:2015)

mean_year1= mean(Seasonality_data[,1])
var=sd(Seasonality_data[,1])
day_counter=1
threshold= var*3 #(Using threshold for change as 3 times SD, as I want to give some room for variation)


for(i in 1:ncol(cumsum_data)){
  date_change=NULL
  for (j in 1:nrow(cumsum_data)){
    val=Seasonality_data[j,i]
    cumsum_data[j,i] <- max(0, cumsum_data[j-1,i] + (mean_year1 - val- var*0.5))
    
    if (cumsum_data[j,i]>=threshold){
    
      cat("Year",colnames(cumsum_data[i]),"Day winter started is:",
          rownames(cumsum_data)[j],"Day","\n")
      break
    }
    
    
  }
    
}

#According to my CUSUM model with C of 0.5 SD and threshold of 3 times SD, on seasonality measure from our Holt Winters model, it appears that summer end is delayed slowly over the years,it ends with 30th September in 1997 
#but for 2015 it ends in 6th October
#Note that we can't say if summer is longer or shorter because we don't have data to know if summer started earlier or delayed
#but according to our model it is gradually delayed




