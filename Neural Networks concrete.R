library(ggvis) 
library(psych)
library(knitr) 
library(neuralnet) 

concrete <- read.csv(file.choose())

knitr::kable(head(concrete), caption = "Partial Table Preview")
str(concrete)

concrete %>% ggvis(x = ~strength, fill:= "#27bc9c") %>% layer_histograms() %>% layer_paths(y = ~strength, 35.82, stroke := "red")
pairs.panels(concrete[c("cement", "slag", "ash", "strength")])
pairs.panels(concrete[c("superplastic", "coarseagg", "fineagg", "age", "strength")])
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x) ))
}

concrete_norm <- as.data.frame(lapply(concrete, normalize))
kable(round(head(concrete_norm), digits = 3), caption = "Normalized Data Preview")

#training set
concrete_train <- concrete_norm[1:773, ]

#test set
concrete_test <- concrete_norm[774:1030, ]


concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age , data = concrete_train, hidden = 1)
plot(concrete_model)
model_results <- compute(concrete_model, concrete_test[1:8]) 
predicted_strength <- model_results$net.result
cor(predicted_strength, concrete_test$strength)#0.806411

concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic + coarseagg + fineagg + age, data = concrete_train, hidden = 5 )
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength) #0.9279936
