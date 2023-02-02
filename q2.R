getwd()
setwd("C:/Users/Osama Zafar/Documents")
# loading the data
data<-read.csv('CityPayrollDataset.csv')
# Summary of the data
summary(data)

#Subset of Public Works - Sanitation
Public_Works_Sanitation <- data[data$Department.Title == "Public Works - Sanitation",]
Public_Works_Sanitation
# Cleaning data
data <- as.numeric(gsub("\\$", "", data$Permanent.Bonus.Pay))
data

p = length(Public_Works_Sanitation)
s = length(data)

#Mean of population
mean_population <- mean(data)
mean_population

#Finding sample of 100 size, mean and Std:

sample = sample(1:nrow(Public_Works_Sanitation), size=100)
sample

sample <- Public_Works_Sanitation[sample,]
sample

sample$Permanent.Bonus.Pay = as.numeric(gsub("\\$", "", sample$Permanent.Bonus.Pay))
sample$Permanent.Bonus.Pay

mean_of_sample <- mean(sample$Permanent.Bonus.Pay)
mean_of_sample

std_of_sample <- sd(sample$Permanent.Bonus.Pay)
std_of_sample

# t-distribution
ts <- ((mean_of_sample-mean_population)*sqrt(120))/std_of_sample
ts

#Function of t-test
t.test(sample$Permanent.Bonus.Pay, mu=mean_population, alt="greater", conf=0.95)

#Hence the T value lies in the rejection region so it can be concluded that
# the Employees who get Permanent Bonus Pay are most likely to be from Public Works - Sanitation Department.