
company_data <- read.csv("~/Downloads/Data Science/data set/Company_Data.csv")
str(company_data)
head(company_data)

hist(company_data$Sales)

cut(company_data$Sales,2)
company_sales <- cut(company_data$Sales,2,labels = c("low","high"))
company_data <- company_data[,-1]
company_data <- cbind(company_sales,company_data)

company_train <- company_data[1:200,]
company_test <- company_data[201:400,]


library(C50)

company_train_model <- C5.0(company_train[,-1],company_train$company_sales)
plot(company_train_model)


company_pred_train <- predict(company_train_model,company_train)
mean(company_train$company_sales==company_pred_train)

library(caret)
confusionMatrix(company_pred_train,company_train$company_sales)


company_test_model <- predict(company_train_model,newdata=company_test)
mean(company_test_model==company_test$company_sales) 
confusionMatrix(company_test_model,company_test$company_sales)

library(gmodels)

CrossTable(company_test$company_sales,company_test_model)
