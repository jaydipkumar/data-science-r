
library(kernlab)
library(caret)
library(plyr)
library(ggplot2)
library(psych)
library(e1071)

train_sal <- read.csv(file.choose())
str(train_sal)
View(train_sal)

train_sal$educationno <- as.factor(train_sal$educationno)
class(train_sal)

test_sal <- read.csv(file.choose())
str(test_sal)
View(test_sal)

test_sal$educationno <- as.factor(test_sal$educationno)
class(test_sal)


model1<-ksvm(train_sal$Salary~., 
             data= train_sal, kernel = "vanilladot")

Salary_prediction <- predict(model1, test_sal)

table(Salary_prediction,test_sal$Salary)

agreement <- Salary_prediction == test_sal$Salary
table(agreement)

prop.table(table(agreement))

model_rfdot<-ksvm(train_sal$Salary~., 
                  data= train_sal,kernel = "rbfdot")

pred_rfdot<-predict(model_rfdot,newdata=test_sal)
mean(pred_rfdot==test_sal$Salary) # 85.19

model_vanilla<-ksvm(train_sal$Salary~., 
                    data= train_sal,kernel = "vanilladot")

pred_vanilla<-predict(model_vanilla,newdata=test_sal)
mean(pred_vanilla==test_sal$Salary) # 84.64

pred_salary_test <- predict(salary_train_model,salary_test)
mean(pred_salary_test==salary_test[,13]) #0.8461487

