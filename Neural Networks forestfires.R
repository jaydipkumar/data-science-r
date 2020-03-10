library(neuralnet)  
library(nnet)
library(plyr)

fire_data <- read.csv(file.choose())
fire_data <- fire_data[,c(-1,-2)]
head(fire_data)
str(fire_data)


library(ggplot2)
library(ggforce)
ggplot(fire_data, aes(fire_data$size_category, fire_data$temp)) +
  geom_sina(aes(color = fire_data$size_category), size = 0.7)+
  scale_color_manual(values =  c("#34d800", "#d80400"))

ggplot(fire_data, aes(fire_data$size_category, fire_data$wind)) +
  geom_sina(aes(color = fire_data$size_category), size = 0.7)+
  scale_color_manual(values =  c("#34d800", "#d80400"))

ggplot(fire_data, aes(fire_data$size_category, fire_data$rain)) +
  geom_sina(aes(color = fire_data$size_category), size = 0.7)+
  scale_color_manual(values =  c("#34d800", "#d80400"))


fire_data$size_category <- ifelse(fire_data$size_category=="small",0,1)
fire_data$size_category <- as.factor(fire_data$size_category)
head(fire_data)
str(fire_data)


library(caTools)
fire_train <- fire_data[1:300,]
fire_test <- fire_data[301:517,]


fire_model <- neuralnet(size_category~FFMC+DMC+DC+ISI+temp+RH+wind+rain+area,
                        data = fire_train,
                        linear.output = FALSE, 
                        err.fct = 'ce', 
                        likelihood = TRUE)
str(fire_model)
plot(fire_model)
summary(fire_model)


model_results <- compute(fire_model,fire_test[1:28])
str(model_results)
predicted_area <- model_results$net.result
predicted_area <- as.data.frame(predicted_area)
predicted_area <- ifelse(predicted_area$V1>0.5,0,1)
table(predicted_area,fire_test[,29])

library(gmodels)
CrossTable(predicted_area,fire_test[,29]) #0.705



