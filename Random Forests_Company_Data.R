
company_data <- read.csv("~/Downloads/Data Science/data set/Company_Data.csv")
head(company_data)
str(company_data)

sum(is.na(company_data))

company_data$Sales <- ifelse(company_data$Sales<9,"low","high")
company_data$Sales <- as.factor(company_data$Sales)
str(company_data)

attach(company_data)
barplot(table(Sales,ShelveLoc),xlab = "Salaes",ylab = "Frequency")
barplot(table(Sales,Urban),xlab = "Salaes",ylab = "Frequency")
barplot(table(Sales,US),xlab = "Salaes",ylab = "Frequency")

library(ggplot2)
library(ggforce)
ggplot(company_data,aes(company_data$Sales,company_data$CompPrice))+
  geom_sina(aes(color=company_data$Sales), size=1)+
  scale_color_manual(values = c("#34d800","#d80400"))

ggplot(company_data,aes(company_data$Sales,company_data$Income))+
  geom_sina(aes(color=company_data$Sales), size=1)+
  scale_color_manual(values = c("#34d800","#d80400"))

ggplot(company_data,aes(company_data$Sales,company_data$Advertising))+
  geom_sina(aes(color=company_data$Sales), size=1)+
  scale_color_manual(values = c("#34d800","#d80400"))

ggplot(company_data,aes(company_data$Sales,company_data$Population))+
  geom_sina(aes(color=company_data$Sales), size=1)+
  scale_color_manual(values = c("#34d800","#d80400"))

library(caTools)
split <- sample.split(company_data$Sales,SplitRatio = 0.7)
company_train <- subset(company_data,split==TRUE)
company_test <- subset(company_data,split==FALSE)

library(randomForest)
company_train_model <- randomForest(Sales~.,data = company_train,na.action=na.roughfix,importance=TRUE)
company_train_model
plot(company_train_model)
company_train_pred <- predict(company_train_model,company_train)
mean(company_train_pred==company_train$Sales) 

library(caTools)

company_test_pred <- predict(company_train_model)
confusionMatrix(company_test_pred,company_train$Sales)

mean(company_test_pred==company_test_pred) 
confusionMatrix(company_train_pred,company_train$Sales)

