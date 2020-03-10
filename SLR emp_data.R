#3.Emp_data -> Build a prediction model for Churn_out_rate 

Emp_data <- read.csv("~/Downloads/Data Science/data set/emp_data.csv")

summary(Emp_data)

var(Emp_data$Salary_hike)

sd(Emp_data$Salary_hike)

var(Emp_data$Churn_out_rate)

sd(Emp_data$Churn_out_rate)

library(e1071)

skewness(Emp_data$Salary_hike)

kurtosis(Emp_data$Salary_hike)

boxplot(Emp_data$Salary_hike)

barplot(Emp_data$Salary_hike)

hist(Emp_data$Salary_hike)

qqnorm(Emp_data$Salary_hike)

plot(Emp_data)

cor(Emp_data)

Model1 <- lm(Churn_out_rate ~ Salary_hike, data = Emp_data)

summary(Model1)

Model2 <- lm(Churn_out_rate ~ log(Salary_hike),data = Emp_data)

summary(Model2)

FinalModel <- lm(log(Churn_out_rate) ~ log(Salary_hike),data = Emp_data)

summary(FinalModel)

plot(FinalModel)

