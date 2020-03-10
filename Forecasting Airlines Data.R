
library(forecast)
library(fpp)
library(smooth)
library(readxl)

Airlines<-read_excel(file.choose())

View(Airlines)
windows()
plot(Airlines$Passengers,type="o")

X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )


colnames(X)<-month.abb 
# View(X)
AirlinesData<-cbind(Airlines,X)
View(AirlinesData)
colnames(AirlinesData)

AirlinesData["t"]<- 1:96
View(AirlinesData)
AirlinesData["log_Passenger"]<-log(AirlinesData["Passengers"])
AirlinesData["t_square"]<-AirlinesData["t"]*AirlinesData["t"]
attach(AirlinesData)

train<-AirlinesData[1:84,]

test<-AirlinesData[85:96,]

# LINEAR MODEL

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)

linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53.19924

# Exponential

expo_model<-lm(log_Passenger~t,data=train)
summary(expo_model)

expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46.05736  and Adjusted R2 - 82.18 %

# Quadratic

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)

Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.05189 and Adjusted R2 - 79.12%

# Additive Seasonality

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)

sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 132.8198

# Final Model

new_model<-lm(log_Passenger~t,data=AirlinesData)
summary(new_model)
new_model_pred<-data.frame(predict(new_model,interval='predict',newdata=AirlinesData))

new_model_fin <- new_model$fitted.values

View(new_model_fin)

Month <- as.data.frame(AirlinesData$Month)

Final <- as.data.frame(cbind(Month,AirlinesData$Passengers,new_model_fin))

colnames(Final) <-c("Month","Passengers","New_Pred_Value")
plot(Final$Passengers,main = "ActualGraph", xlab="Passengers(Actual)", ylab="Month",col.axis="blue",type="o") 


plot(Final$Passengers, main = "PredictedGraph", xlab="Passengers(Predicted)", ylab="Month",col.axis="Green",type="s")

View(Final)


table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)



setwd("C:/Users/Aju/Downloads/")
write.csv(table_rmse,file="table_rmse_Airlines.csv",col.names = F,row.names = F)

