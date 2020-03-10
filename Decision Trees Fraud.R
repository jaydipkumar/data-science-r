
fraud_data <- read.csv("~/Downloads/Data Science/data set/Fraud_check.csv")
str(fraud_data)
head(fraud_data)


hist(fraud_data$Taxable.Income)


sum(is.na(fraud_data))

fraud_data$Taxable.Income <- ifelse(fraud_data$Taxable.Income<=30000,"Risky","Good")
fraud_data$Taxable.Income <- as.factor(fraud_data$Taxable.Income)


fraud_train <- fraud_data[1:300,]
fraud_test <- fraud_data[301:600,]


library(C50)
fraud_train_model<-C5.0(fraud_train[,-3],fraud_train$Taxable.Income)
plot(fraud_train_model)

fraud_train_pred <- predict(fraud_train_model,fraud_train)
mean(fraud_train$Taxable.Income==fraud_train_pred)

library(caret)
confusionMatrix(fraud_train_pred,fraud_train$Taxable.Income)


fraud_test_model <- predict(fraud_train_model,newdata=fraud_test)
mean(fraud_test_model==fraud_test$Taxable.Income)
confusionMatrix(fraud_test_model,fraud_test$Taxable.Income)

library(gmodels)

CrossTable(fraud_test_model,fraud_test$Taxable.Income)
