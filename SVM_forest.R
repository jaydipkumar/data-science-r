forest_fire <- mydata
forest_fire <- read.csv("")
forest_fire <- forest_fire[,c(-1,-2)]
head(forest_fire)
str(forest_fire)
options(warn=-1)

library(ggplot2)
library(ggforce)

ggplot(forest_fire,aes(forest_fire$size_category,forest_fire$temp))+
  geom_sina(aes(color=forest_fire$size_category), size=0.7)+
  scale_color_manual(values =   c("#34d800","#d80400"))

ggplot(forest_fire,aes(forest_fire$size_category,forest_fire$wind))+
  geom_sina(aes(color=forest_fire$size_category), size=0.7)+
  scale_color_manual(values =   c("#34d800","#d80400"))

ggplot(forest_fire,aes(forest_fire$size_category,forest_fire$rain))+
  geom_sina(aes(color=forest_fire$size_category), size=0.7)+
  scale_color_manual(values =   c("#34d800","#d80400"))

ggplot(forest_fire,aes(forest_fire$size_category,forest_fire$area))+
  geom_sina(aes(color=forest_fire$size_category), size=0.7)+
  scale_color_manual(values =   c("#34d800","#d80400"))

sum(is.na(forest_fire))

forest_fire[,29] <- ifelse(forest_fire[,29]=="small",0,1)
forest_fire[,29] <- as.factor(forest_fire[,29])


forest_train <- forest_fire[1:300,]
forest_test <- forest_fire[301:517,]

library(kernlab)
library(caret)

f_train_model <- ksvm(size_category~.,data=forest_train,kernal="laplacedot")
f_test_model <- predict(f_train_model,newdata=forest_test)
mean(f_test_model==forest_test$size_category)
table(f_test_model,forest_test$size_category)

kernals <- c("rbfdot","polydot","tanhdot","vanilladot","laplacedot","besseldot","anovadot","splinedot","matrix","stringdot")


accuracy = list()
for (i in kernals) {
  f_train_model <- ksvm(size_category~.,data=forest_train,kernal=i)
  f_test_model <- predict(f_train_model,newdata=forest_test)
  accuracy[i] <- mean(f_test_model==forest_test$size_category)  
}
accuracy

