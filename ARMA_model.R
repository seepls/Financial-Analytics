#ARMA Model

# Installing Packages
install.packages("tseries")
install.packages("timeSeries")
install.packages("forecast")
install.packages("urca")
library(ucra)
library(tseries)
library(timeSeries)
library(forecast)

install.packages("testthat")
library(testthat)
# Reading Data
# data1 <- read.csv("C:/Users/MBA/Downloads/data.csv")  # Daily NIFTY50 data
data1=read.csv(file.choose(),header=TRUE)
data.ts <- ts(data = data1)  # Conveting to time series data

count_d1 = diff(data.ts, differences = 1)
adf.test(data.ts)
adf.test(count_d1)

# Applying arma model and converting to object

obj1 = arma(data.ts, order = c(1,1,1))

#Extracting coefficients
obj1$coef

#Extracting residuals
obj1$residuals

#Extracting fitted values
obj1$fitted.values

# Printing multiple values
cbind(data.ts, obj1$fitted.values, obj1$residuals)

# P Values
summary(obj1)


# trend and drift 
#urca::ur.df(data.ts, type = c("none", "drift", "trend"), lags = 5,
 #     selectlags = c("Fixed", "AIC", "BIC")) 



#Checking Lag
cbind(data.ts, lag(data.ts))
ret.data = diff(log(data.ts))
adf.test(ret.data)

#auto.arima
pacf(ret.data)
obj2 = arima(data.ts, order = c(3,0,3) )
obj3 = auto.arima(ret.data, max.p=10, max.q=10, stationary=TRUE, ic="aicc", test="adf")

summary(obj2)

test1 <- urca::ur.df(data.ts, type = "trend", lags = 5)
summary(test1)

#predict
obj4 <- forecast::auto.arima(ret.data)
predict(obj4,3)
summary(obj4)
res = obj4$residuals
