print(getwd())
setwd("C:/Users/Osama Zafar/Documents")
data <- read.csv("CityPayrollDataset.csv")
#Removing $ from the data
data$Temporary.Bonus.Pay = as.numeric(gsub("\\$", "", data$Temporary.Bonus.Pay))
popMeanTempBonus <- mean(data$Temporary.Bonus.Pay)
popStdTempBonus <- sd(data$Temporary.Bonus.Pay)
# Null hypothesis in this case is
# employees working as Police Officer-II have same chance of getting Temporary Bonus Pay
# H0 : mu = popMeanTempBonus
# Alternate hypothesis
# Ha: mu > popMeanTempBonus
# right tailed test
a<-subset(data, Job.Class.Title=='Police Officer II')
PoIIMeanTempBonus <- mean(a$Temporary.Bonus.Pay)
index <- sample(1:nrow(a), size = 120)
smple <- a[index,]
SampleMean = mean(smple$Temporary.Bonus.Pay)
SampleStd = sd(smple$Temporary.Bonus.Pay)
ts = ((SampleMean - popMeanTempBonus)*sqrt(120))/SampleStd
ts
#Function of t-test
t.test(data$Temporary.Bonus.Pay, mu=popMeanTempBonus, alt="greater", conf=0.95)
#Hence the T value lies in the rejection region so it can be concluded that
# the employees working as Police Officer-II have a better chance of getting Temporary Bonus Pay.
