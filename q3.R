getwd()
setwd("C:/Users/Osama Zafar/Documents")
# loading the data
data<-read.csv('CityPayrollDataset.csv')
# Summary of the data
summary(data)

DWP <- data[data$Department.Title == "Water And Power (DWP)",]
DWP
# Extracting  data
Data <- sub("^$", 0.00, data$Overtime.Pay)

DWP
Data <- as.numeric(gsub("\\$", "", Data))

n = length(DWP)
m = length(Data)

mean_population <- mean(Data)

sample = sample(1:nrow(DWP), size=120)

sample_of_DWP <- DWP[sample,]

sample_of_DWP$Overtime.Pay <- sub("^$", 0.00, sample_of_DWP$Overtime.Pay)
sample_of_DWP$Overtime.Pay = as.numeric(gsub("\\$", "", sample_of_DWP$Overtime.Pay))

mean_of_DWP <- mean(sample_of_DWP$Overtime.Pay)
std_of_DWP <- sd(sample_of_DWP$Overtime.Pay)

# t-distribution
t_DWP <- ((mean_of_DWP-mean_population)*sqrt(99))/std_of_DWP
t_DWP

#Function of t-test
t.test(sample_of_DWP$Overtime.Pay, mu=mean_population, alt="greater", conf=0.95)


#Hence the T value lies inside the critical region. So Null Hypothesis has not rejected and  it can be concluded that
# Employees working in Water and Power (DWP) Department have not a better chance of being employed overtime