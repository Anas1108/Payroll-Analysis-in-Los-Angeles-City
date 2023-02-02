getwd()
setwd("C:/Users/Osama zafar/Documents")
# loading the data
data<-read.csv('CityPayrollDataset.csv')
# Summary of the data
summary(data)
# cleaning data
data$Average.Health.Cost = as.numeric(gsub("\\$", "", data$Average.Health.Cost))
dataset <- subset(data,data$Department.Title=="Harbor (Port of LA)" & data$Job.Class.Title=='Senior Clerk Typist' )
# find the population mean
Population_Mean <- mean(dataset$Average.Health.Cost)
# find the population standard deviation
Population_std <- sd(dataset$Average.Health.Cost)
# extracting sample
sample <- sample(1:nrow(dataset), size=30)
sample <- dataset[sample,]
mean_of_dataset <- mean(sample$Average.Health.Cost)
std_of_dataset <- sd(sample$Average.Health.Cost)
# t-distribution
t_dataset <- ((mean_of_dataset-Population_Mean)*sqrt(30))/std_of_dataset
t_dataset
#Function of t-test 1
t.test(sample$Average.Health.Cost, mu=Population_Mean, alt="greater", conf=0.95)

dataset1 <- subset(data, data$Department.Title=="Water And Power (DWP)" & data$Job.Class.Title=='Senior Clerk Typist'  )
Population_Mean1 <- mean(dataset1$Average.Health.Cost)
Population_std1 <- sd(dataset1$Average.Health.Cost)
sample1<- sample(1:nrow(dataset1), size=120)
sample1 <- dataset1[sample1,]
mean_of_dataset1 <- mean(sample1$Average.Health.Cost)
std_of_dataset1 <- sd(sample1$Average.Health.Cost)
# t-distribution
t_dataset1 <- ((mean_of_dataset1-Population_Mean1)*sqrt(120))/std_of_dataset1
t_dataset1
#Function of t-test 2
t.test(sample1$Average.Health.Cost, mu=Population_Mean1, alt="greater", conf=0.95)