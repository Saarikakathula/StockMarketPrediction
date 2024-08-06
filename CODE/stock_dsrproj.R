# Stock Market Price Prediction using R 

library(quantmod)
library(tseries)
library(forecast)

start <- as.Date("2021-03-20")
end <- as.Date("2024-03-20")


getSymbols(c("MSFT","NVDA", "AMZN","TSLA"), src = "yahoo", from = start, to = end)
View(MSFT)
View(TSLA)
View(NVDA)
View(AMZN)

stocks <- as.xts(data.frame(MSFT = MSFT[, "MSFT.Close"], AMZN = AMZN[, "AMZN.Close"], 
                            NVDA = NVDA[, "NVDA.Close"], TSLA = TSLA[, "TSLA.Close"]))
head(stocks)
View(stocks)

plot(as.zoo(stocks), screens = 1, lty = 1:4, xlab = "Date", ylab = "Price")
legend("right", c("MSFT", "AMZN", "NVDA","TSLA"), lty = 1:4, cex = 0.5)



chartSeries(MSFT, theme='white',  subset = 'last 3 months', type = 'auto',TA=c(addBBands(),addMACD()))
chartSeries(AMZN, theme='white',  subset = 'last 3 months', type = 'auto',TA=c(addBBands(),addMACD()))
chartSeries(NVDA, theme='white',  subset = 'last 3 months', type = 'auto',TA=c(addBBands(),addMACD()))
chartSeries(TSLA, theme='white',  subset = 'last 3 months', type = 'auto',TA=c(addBBands(),addMACD()))
addBBands() # bollinger bands
addMACD() # moving average convergence or divergence
addCMF()

stock_names <- list(MSFT,AMZN,NVDA,TSLA)

for (stock in stock_names){
  str(stock)
}

for (stock in stock_names){
  print(summary(stock))
}


#Calculate Returns
stocks$MSFT.Returns <- diff(log(stocks$MSFT.Close))
stocks$AMZN.Returns <- diff(log(stocks$AMZN.Close))
stocks$NVDA.Returns <- diff(log(stocks$NVDA.Close))
stocks$TSLA.Returns <- diff(log(stocks$TSLA.Close))

stock_return <- as.xts(data.frame(
  MSFT = stocks[, "MSFT.Returns", drop = FALSE],
  AMZN = stocks[, "AMZN.Returns", drop = FALSE],
  NVDA = stocks[, "NVDA.Returns", drop = FALSE],
  TSLA = stocks[, "TSLA.Returns", drop = FALSE]
))

View(stock_return)


#Calculate moving averages

stocks$MSFT.MA <- SMA(stocks$MSFT.Close, n=10)
stocks$AMZN.MA <- SMA(stocks$AMZN.Close, n=10)
stocks$NVDA.MA <- SMA(stocks$NVDA.Close, n=10)
stocks$TSLA.MA <- SMA(stocks$TSLA.Close, n=10)

stock_MA <- as.xts(data.frame(
  MSFT = stocks[, "MSFT.MA", drop = FALSE],
  AMZN = stocks[, "AMZN.MA", drop = FALSE],
  NVDA = stocks[, "NVDA.MA", drop = FALSE],
  TSLA = stocks[, "TSLA.MA", drop = FALSE]
))
View(stock_MA)


stock_prices <- Cl(NVDA) # AMZN  NVDA  TSLA

any(is.na(stock_prices))  
# Calculate quartiles and IQR

q <- quantile(stock_prices, probs=c(.25, .75))
lower <- q[1] - 1.5 * IQR(stock_prices)
upper <- q[2] + 1.5 * IQR(stock_prices)

  # outliers
outliers <- stock_prices[stock_prices < lower | stock_prices > upper]
boxplot(stock_prices, main="Stock Prices", sub=paste("Outliers:", length(outliers)))


 # money flowchart


stock_names <- list(MSFT,AMZN,NVDA,TSLA)

for (stock_name in stock_names) {
  
  daily_ret <- dailyReturn(stock_name)
  print(head(daily_ret))
  
  
  # Yearly returns
  year_ret <- yearlyReturn(stock_name)
  print(year_ret)
  
  # All returns (daily, weekly, monthly, quarterly, yearly)
  all_ret <- allReturns(stock_name)
  print(head(all_ret))
}

############### Model ################

##################################### MSFT
Closed_prices= Cl(MSFT)
class(Closed_prices)

#Augmented dickey-Fuller Test to determine the non-stationary of the data
print(adf.test(Closed_prices))


modelfit_msft <- auto.arima(Closed_prices, lambda = "auto")
summary(modelfit_msft)
plot(resid(modelfit_msft),ylab="Residuals",main="Residuals(Arima (2,1,3)) vs. Time")

tsdiag(modelfit_msft)

plot(as.ts(Closed_prices))
lines(modelfit_msft$fitted,col="red")

price_forecast <- forecast(modelfit_msft,h=100)
plot(price_forecast)
price_forecast


# Dividing the data into train & test sets , Applying the model
N = length (Closed_prices)
n = 0.8*N

train <- stocks$MSFT.Close[1:n, ]
test <- stocks$MSFT.Close[(n+1):N,]

arima_msft <- auto.arima(train,seasonal = FALSE ,lambda= "auto")
#summary(trainarimafit)

test_fit <- Arima(test)

pred <- predict(arima_msft, n.ahead = length(test))$pred
pred

predlen= length(test)

test_forecast_msft <- forecast(test, h= predlen)
test_forecast_msft

par(mfrow=c(1,1))
plot(test_forecast_msft, main = "Arima forecast for MicroSoft Stock")
accuracy(test_forecast_msft,test)



#####################################AMZN

Closed_prices= Cl(AMZN)
class(Closed_prices)

#Augmented dickey-Fuller Test to determine the non-stationary of the data
print(adf.test(Closed_prices))



modelfit_amzn <- auto.arima(Closed_prices, lambda = "auto")
#summary(modelfit_amzn)

tsdiag(modelfit_amzn)


plot(as.ts(Closed_prices))
lines(modelfit_amzn$fitted,col="red")

price_forecast <- forecast(modelfit_amzn,h=100)
plot(price_forecast)
price_forecast


# Dividing the data into train & test sets , Applying the model
N = length (Closed_prices)
n = 0.8*N

train <- stocks$AMZN.Close[1:n, ]
test <- stocks$AMZN.Close[(n+1):N,]

trainarimafit <- auto.arima(train,seasonal = FALSE ,lambda= "auto")
#summary(trainarimafit)

test_fit <- Arima(test)

pred <- predict(test_fit, n.ahead = length(test))$pred
pred

predlen= length(test)

test_forecast_amzn <- forecast(test, h= predlen)
test_forecast_amzn

par(mfrow=c(1,1))
plot(test_forecast_amzn, main = "Arima forecast for Amazon Stock")
accuracy(test_forecast_amzn,test)

##############################################NVDA

Closed_nvda= Cl(NVDA)
class(Closed_nvda)

#Augmented dickey-Fuller Test to determine the non-stationary of the data
print(adf.test(Closed_nvda))


modelfit_nvda <- auto.arima(Closed_nvda, lambda = "auto")
summary(modelfit_nvda)

tsdiag(modelfit_nvda)


plot(as.ts(Closed_nvda))
lines(modelfit_nvda$fitted,col="red")

price_forecast <- forecast(modelfit_nvda,h=100)
plot(price_forecast)
price_forecast


# Dividing the data into train & test sets , Applying the model
N = length (Closed_nvda)
n = 0.8*N

train <- stocks$NVDA.Close[1:n, ]
test <- stocks$NVDA.Close[(n+1):N,]

trainarimafit <- auto.arima(train,seasonal = FALSE ,lambda= "auto")
#summary(trainarimafit)


pred <- predict(trainarima_fit,data = as.data.frame(test))
pred
accuracy(pred)

predlen= length(test)
test_forecast_nvda <- forecast(test, h= predlen)
test_forecast_nvda

par(mfrow=c(1,1))
plot(test_forecast_nvda, main = "Arima forecast for NVDIA Stock")

#########################################TSLA

Closed_tsla= Cl(TSLA)
class(Closed_tsla)

#Augmented dickey-Fuller Test to determine the non-stationary of the data
print(adf.test(Closed_tsla))


modelfit_tsla <- auto.arima(Closed_tsla, lambda = "auto")
summary(modelfit_tsla)

tsdiag(modelfit_tsla)


plot(as.ts(Closed_tsla))
lines(modelfit_tsla$fitted,col="red")

price_forecast <- forecast(modelfit_tsla,h=100)
plot(price_forecast)
price_forecast


# Dividing the data into train & test sets , Applying the model
N = length (Closed_tsla)
n = 0.8*N

train <- stocks$TSLA.Close[1:n, ]
test <- stocks$TSLA.Close[(n+1):N,]

trainarimafit <- auto.arima(train,seasonal = FALSE ,lambda= "auto")
#summary(trainarimafit)


pred <- predict(trainarima_fit,data = as.data.frame(test))
pred
accuracy(pred)

predlen= length(test)
test_forecast_tsla <- forecast(test, h= predlen)
test_forecast_tsla

par(mfrow=c(1,1))
plot(test_forecast_tsla, main = "Arima forecast for TESLA Stock")


par(mfrow = c(2,2))

plot(test_forecast_msft, main = 'Arima forecast for MICROSOFT Stock')
plot(test_forecast_amzn, main = 'Arima forecast for AMAZON Stock')
plot(test_forecast_nvda, main = 'Arima forecast for NVDIA Stock')
plot(test_forecast_tsla, main = 'Arima forecast for TESLA Stock ')
