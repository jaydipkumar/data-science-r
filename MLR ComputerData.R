library(e1071)
library(car)

ComputerData  <- read.csv("~/Downloads/Data Science/data set/Computer_Data.csv")

ComputerData <- ComputerData[,-1]
orgdata <- ComputerData

attach(ComputerData)

summary(ComputerData)
sd(price)
sd(speed)
sd(hd)
sd(ram)
sd(screen)
sd(ads)
sd(trend)
var(price)
var(speed)
var(hd)
var(ram)
var(screen)
var(ads)
var(trend)
skewness(price)
skewness(speed)
skewness(hd)
skewness(ram)
skewness(screen)
skewness(ads)
skewness(trend)
kurtosis(price)
kurtosis(speed)
kurtosis(hd)
kurtosis(ram)
kurtosis(screen)
kurtosis(ads)
kurtosis(trend)
plot(speed, price)


plot(hd, price)
plot(ram, price)
plot(screen, price)
plot(ads, price)
plot(trend, price)
plot(cd, price)
plot(multi, price)
plot(premium, price)
pairs(ComputerData)

model <- lm(price ~ speed + hd + ram + screen + ads + trend + cd + multi + premium, data = ComputerData)
summary(model)

model2 <- lm(price ~ ., data = ComputerData[-c(1441, 1701),])
summary(model2)

vif(model2)
avPlots(model2)

model3 <- lm(price ~ speed + hd + ram + screen + ads + trend + premium, data = ComputerData[-c(1441, 1701),])
summary(model3)

avPlots(model3)

plot(model)
qqPlot(model)




