#Consider only the below columns and prepare a prediction model for predicting Price.

Data <- read.csv("~/Downloads/Data Science/data set/ToyotaCorolla.csv")

Corolla<- Data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]

attach(Corolla)

summary(Corolla)

sd(Price)

sd(Age_08_04)

sd(KM)

sd(HP)

sd(cc)

sd(Doors)

sd(Gears)

sd(Quarterly_Tax)

sd(Weight)

var(Price)

var(Age_08_04)

var(KM)

var(HP)

var(cc)

var(Doors)

var(Gears)

var(Quarterly_Tax)

var(Weight)

skewness(Price)

skewness(Age_08_04)

skewness(KM)

skewness(HP)

skewness(cc)

skewness(Doors)

skewness(Gears)

skewness(Quarterly_Tax)

skewness(Weight)

kurtosis(Price)

kurtosis(Age_08_04)

kurtosis(KM)

kurtosis(HP)

kurtosis(cc)

kurtosis(Doors)

kurtosis(Gears)

kurtosis(Quarterly_Tax)

kurtosis(Weight)

plot(Age_08_04, Price)

plot(KM, Price)

plot(HP, Price)

plot(cc, Price)

plot(Doors, Price)

plot(Gears, Price)

plot(Quarterly_Tax, Price)

plot(Weight, Price)

summary(Data$Fuel_Type)

summary(Data$Color)

pairs(Corolla)

cor(Corolla)

library(corpcor)

cor2pcor(cor(Corolla))

model <- lm(Price ~ ., data = Corolla)

summary(model)

model.carcc <- lm(Price ~ cc)

summary(model.carcc)

model.cardoor <- lm(Price ~ Doors)

summary(model.cardoor)

model.car <- lm(Price ~ cc + Doors)

summary(model.car) 

?influence.measures

influence.measures(model.car)

influenceIndexPlot(model.car)

influencePlot(model.car)

model1 <- lm(Price ~ ., data = Corolla[-c(81),])

summary(model1)

vif(model1)

avPlots(model1)

finalmodel <- lm(Price ~ Age_08_04 + KM + HP + cc + Gears + Quarterly_Tax + Weight, data = Corolla[-c(81),])

summary(finalmodel)

plot(finalmodel)

qqPlot(model)


