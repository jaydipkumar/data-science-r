
fraud_data <- read.csv("~/Downloads/Data Science/data set/Fraud_check.csv")
head(fraud_data)

colnames(fraud_data) <- c("Undergraduate","Marital_Status","Taxable_Income","City_Population","Work_Experience","Urban")
fraud_data$Taxable_Income <- ifelse(fraud_data$Taxable_Income<=30000,"risky","good")
fraud_data$Taxable_Income <- as.factor(fraud_data$Taxable_Income)
str(fraud_data)
table(fraud_data$Taxable_Income)

barplot(table(as.factor(fraud_data$Taxable_Income),as.factor(fraud_data$Marital_Status)),xlab = "Taxable Income",ylab = "Frequency",legend=c("Risky","Good"))
barplot(table(as.factor(fraud_data$Taxable_Income),as.factor(fraud_data$Undergraduate)),xlab = "Taxable Income",ylab = "Frequency",legend=c("Risky","Good"))
barplot(table(as.factor(fraud_data$Taxable_Income),as.factor(fraud_data$Urban)),xlab = "Taxable Income",ylab = "Frequency",legend=c("Risky","Good"))

library(ggplot2)
library(ggforce)
ggplot(fraud_data,aes(fraud_data$Taxable_Income,fraud_data$City_Population))+
  geom_sina(aes(color=fraud_data$Taxable_Income), size=1)+
  scale_color_manual(values = c("#34d800","#d80400"))

ggplot(fraud_data,aes(fraud_data$Taxable_Income,fraud_data$Work_Experience))+
  geom_sina(aes(color=fraud_data$Taxable_Income), size=1)+
  scale_color_manual(values = c("#34d800","#d80400"))


library(caTools)
split <- sample.split(fraud_data$Undergraduate,SplitRatio = 0.7)
fraud_train <- subset(fraud_data,split==TRUE)
fraud_test <- subset(fraud_data,split==FALSE)

library(randomForest)
fraud_train_model <- randomForest(Taxable_Income~.,data=fraud_train, na.action=na.roughfix,importance=TRUE)
plot(fraud_train_model)
fraud_train_pred <- predict(fraud_train_model,fraud_train)
mean(fraud_train$Taxable_Income==fraud_train_pred) #0.9071429

library(caret)
confusionMatrix(fraud_train$Taxable_Income,fraud_train_pred)

fraud_test_pred <- predict(fraud_train_model,fraud_test)
mean(fraud_test$Taxable_Income==fraud_test_pred) #0.7777778

confusionMatrix(fraud_test$Taxable_Income,fraud_test_pred)


