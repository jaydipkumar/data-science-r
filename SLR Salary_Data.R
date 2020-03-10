#Salary_hike -> Build a prediction model for Salary_hike

Salary_hike <- read.csv("~/Downloads/Data Science/data set/Salary_Data.csv")

summary(Salary_hike)

attach(Salary_hike)

var(Salary_hike$YearsExperience)

sd(Salary_hike$YearsExperience)

var(Salary_hike$Salary)

sd(Salary_hike$Salary)

plot(YearsExperience,Salary)

boxplot(Salary_hike)

hist(YearsExperience)

hist(Salary)

summary(Salary_hike)

ye<- YearsExperience
sh <- Salary

cor(ye,sh)

reg<-lm(sh~ye)

summary(reg)

confint(reg,level = 0.95)
predict(reg,interval="predict")
reg_log<-lm(sh~log(ye)) 
summary(reg_log)

confint(reg_log,level=0.95)
predict(reg_log,interval="predict")
reg_exp<-lm(log(sh)~ye)
summary(reg_exp)

exp(predict(reg_exp,interval="predict"))
Salary_hike[,"ye_sq"] = ye*ye
quad_mod <- lm(sh~ye+I(ye^2))
summary(quad_mod)

confint(quad_mod,level=0.95)
predict(quad_mod,interval="predict")
qd_model <- lm(sh~ye+ye_sq)
summary(qd_model)
confint(quad_mod,level=0.95)

predict(quad_mod,interval="predict")
poly_mod <- lm(sh~ye+I(ye^2)+I(ye^3))
summary(poly_mod) 

confint(poly_mod,level=0.95)
predict(poly_mod,interval="predict")
model_R_Squared_values <- list(model=NULL,R_squared=NULL)
model_R_Squared_values[["model"]] <- c("reg","reg_log","reg_exp","quad_mod","poly_mod")
model_R_Squared_values[["R_squared"]] <- c(0.9554,0.8487,0.9295,0.9538,0.9594)


Final <- cbind(model_R_Squared_values[["model"]],model_R_Squared_values[["R_squared"]])
View(model_R_Squared_values)
predicted_Value <- predict(poly_mod)
predicted_Value

Final <- cbind(YearsofExp=YearsExperience,Sal_Hike = Salary,Pred_sal_hike=predicted_Value)

View(Final)

rmse<-sqrt(mean((predicted_Value-sh)^2))
rmse
plot(poly_mod)

hist(residuals(poly_mod))



Salary_hike_Model <- lm(Salary ~ YearsExperience, data = Salary_hike)

summary(Salary_hike_Model)

plot(Salary_hike_Model)

