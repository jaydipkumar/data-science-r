library(forecast)
library(fpp)
library(smooth)

data(AirPassengers)
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)

plot(AirPassengers) #plot time series

abline(reg=lm(AirPassengers~time(AirPassengers))) # fit in a line

cycle(AirPassengers) #show result the cycle across years

plot(aggregate(AirPassengers,FUN=mean)) #aggregate the cycles and show year on year trend

boxplot(AirPassengers~cycle(AirPassengers)) #Box plot show seasonal effect

adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)


acf(log(AirPassengers)) #ACF Plots

acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers))) #pacf Plots

#bulid Arima model
#p should be 0 based on ACF cut off
#q should be 1 or 2
(fit <- arima(log(AirPassengers), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12)))

#Fit Model And predict the future 10 years 
pred <- predict(fit, n.ahead = 10*12)

ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))





