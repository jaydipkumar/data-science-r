
Salary_train <- read.csv("~/Downloads/Data Science/data set/SalaryData_Train.csv")
Salary_test <- read.csv("~/Downloads/Data Science/data set/SalaryData_Test.csv")

levels(Salary_test$Salary)
levels(Salary_train$Salary)

Salary_train[,14] <- trimws(Salary_train[,14])
Salary_train[,14] <- as.factor(Salary_train[,14])

levels(Salary_test$Salary)
levels(Salary_train$Salary)

str(Salary_train)
str(Salary_test)

sum(is.na(Salary_train))
sum(is.na(Salary_test))

Salary_train$educationno <- as.factor(Salary_train$educationno)
Salary_test$educationno <- as.factor(Salary_test$educationno)

str(Salary_train)
str(Salary_test)

levels(Salary_train$Salary)
for(i in c (1:13)){
    col_name <- colnames(Salary_train)[i]
    barplot(table(as.factor(Salary_train[,14]),as.factor(Salary_train[,i])),xlab = col_name,ylab = "Frequency",legend=c("<=50K",">50K"))
    readline("Hint Enter..See Next Plot")
}

for (i in c(1:13)) {
  col_name <- colnames(Salary_test)[i]
  barplot(table(as.factor(Salary_test[,14]),as.factor(Salary_test[,i])),xlab = col_name,ylab = "Frequncy",legend=c("<=50K",">50K"))
  readline("Hint Enter..See Next Plot")
}

Salary_train_labal <- Salary_train[,14]
Salary_test_labal <- Salary_test[,14]

library(e1071)

Salary_train_model <- naiveBayes(Salary_train$Salary~.,data = Salary_train)
Model_predict <- predict(Salary_train_model,newdata=Salary_test)
mean(Model_predict==Salary_test[,14])


library(caret)

confusionMatrix(Model_predict,Salary_test$Salary)

