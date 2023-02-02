getwd()
setwd("C:/Users/Osama zafar/Documents")
# loading the data
data<-read.csv('CityPayrollDataset.csv')
# Summary of the data
summary(data)
# cleaning data
data$Longevity.Bonus.Pay = as.numeric(gsub("\\$", "", data$Longevity.Bonus.Pay))
dataset <- subset(data,data$Department.Title=="Recreation And Parks" & data$Year=='2014' )
p <- subset(data,data$Year=='2014' )
# population_Mean
Population_Mean <- mean(p$Longevity.Bonus.Pay)
# population_Standard deviation
Population_std <- sd(p$Longevity.Bonus.Pay)
# extracting Sample
sample = sample(1:nrow(dataset), size=120)
sample_of_Longevity <- dataset[sample,]
mean_of_dataset <- mean(sample_of_Longevity$Longevity.Bonus.Pay)
std_of_dataset <- sd(sample_of_Longevity$Longevity.Bonus.Pay)
# t-distribution
t_dataset <- ((mean_of_dataset-Population_Mean)*sqrt(120))/std_of_dataset
t_dataset
#Function of t-test
t.test(sample_of_Longevity$Longevity.Bonus.Pay, mu=Population_Mean, alt="greater", conf=0.95)
