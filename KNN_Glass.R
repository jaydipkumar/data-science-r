
Glass_Data <- read.csv("~/Downloads/Data Science/data set/glass.csv")

head(Glass_Data)

str(Glass_Data)

Glass_Data$Type <- as.factor(Glass_Data$Type)

str(Glass_Data)
levels(Glass_Data$Type)

normal_glass_data <- scale(Glass_Data[,1:9])

Glass_train <- Glass_Data[1:130,]
Glass_test <- Glass_Data[131:214,]

Glass_train_label <- Glass_Data[1:130,10]
Glass_test_label <- Glass_Data[131:214,10]

library("class")

Glass_train_pred <- knn(train=Glass_train,test=Glass_train,cl=Glass_train_label,k=2)
Glass_train_pred <- knn(train=Glass_train,test=Glass_train,cl=Glass_train_label,k=2)


mean(Glass_train_pred==Glass_train_label)

Glass_test_pred <- knn(train=Glass_test,test=Glass_test,cl=Glass_test_label,k=2)

mean(Glass_test_pred==Glass_test_label)


Glass_train_accu <- NULL
Glass_test_accu <- NULL

for (i in seq(2,30)){
    Glass_train_pred <- knn(train = Glass_train,test = Glass_train,cl=Glass_train_label,k=i)
    Glass_train_accu <- c (Glass_train_accu,mean(Glass_train_pred==Glass_train_label))
    Glass_test_pred <- knn(train = Glass_test,test = Glass_test,cl=Glass_test_label,k=i)
    Glass_test_accu <- c(Glass_test_accu,mean(Glass_test_pred==Glass_test_label))
    }

#plot
accuracy <- data.frame(list(Glass_train_accu=Glass_train_accu,Glass_test_accu=Glass_test_accu,k_value=seq(2,30,1) ))


library(ggplot2)

ggplot(accuracy,aes(x=accuracy$k_value)) +
  geom_line(aes(y=accuracy$Glass_train_accu,colours="train_acc"),lwd=1)+
  geom_line(aes(y=accuracy$Glass_test_accu,clolors="test_acc"),lwd=1)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))
