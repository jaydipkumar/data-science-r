animal_data <- read.csv("~/Downloads/Data Science/data set/Zoo.csv")
animal_data <- animal_data[,-1]

sum(is.na(animal_data))

animal_data$type <- as.factor(animal_data$type)
levels(animal_data$type)

animal_data$type <- factor(animal_data$type, levels = c("1","2","3","4","5","6","7"),labels = c("mammal","bird","reptile","fish","amphibian","insect","crustacean"))
str(animal_data)

Final_animal_Data <- animal_data[,-1]
Normal_Animal_Data <- scale(Final_animal_Data[,c(1:15)])

animal_train <- Normal_Animal_Data[1:70,]
animal_test <- Normal_Animal_Data[71:101,]

animal_train_Label <- animal_data[1:70,18]
animal_test_label <- animal_data[71:101,18]

library("class")

animal_train_pred <- knn(train = animal_train,test = animal_train,cl = animal_train_Label,k = 3)

mean(animal_train_pred==animal_train_Label)

animal_test_pred  <- knn(train = animal_test,test = animal_test,cl = animal_test_label,k = 3)
mean(animal_test_pred==animal_test_label)

animal_train_accu <- NULL
animal_test_accu  <- NULL

for (i in seq(2,30,2)){
  animal_train_pred <- knn(train = animal_train,test = animal_train,cl = animal_train_Label,k = i)
  animal_train_accu <- c(animal_train_accu,mean(animal_train_pred==animal_train_Label))
  animal_test_pred <- knn(train = animal_test,test = animal_test,cl = animal_test_label,k = i)
  animal_test_accu  <- c(animal_test_accu,mean(animal_test_pred==animal_test_label))
  
}

accuracy <- data.frame(list(animal_train_accu=animal_train_accu,animal_test_accu=animal_test_accu,k_value=seq(2,30,2)))

library(ggplot2)

ggplot(accuracy,aes(x=accuracy$k_value))+
  geom_line(aes(y = accuracy$animal_train_accu,colours="train_acc"),lwd=1)+
  geom_line(aes(y=accuracy$animal_test_accu,colours="test_acc"),lwd=1)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))
    
