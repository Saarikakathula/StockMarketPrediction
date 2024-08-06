###### msft


lambda = BoxCox.lambda(stocks$MSFT.Close)
dnn_fit = nnetar(stocks[,1],lambda=lambda)
dnn_fit
fcast_msft = forecast(dnn_fit,PI=T,h=100)
par(mfrow=c(2,2))
autoplot(fcast_msft)
accuracy(dnn_fit)

####### amzn

lambda = BoxCox.lambda(stocks$AMZN.Close)
dnn_fit = nnetar(stocks[,2],lambda=lambda)
dnn_fit
fcast_amzn = forecast(dnn_fit,PI=T,h=100)
autoplot(fcast_amzn)
accuracy(dnn_fit)

####### NVDA

lambda = BoxCox.lambda(stocks$NVDA.Close)
dnn_fit = nnetar(stocks[,3],lambda=lambda)
dnn_fit
fcast_nvda = forecast(dnn_fit,PI=T,h=100)
autoplot(fcast_nvda)
accuracy(dnn_fit)

####### TSLA

lambda = BoxCox.lambda(stocks$TSLA.Close)
dnn_fit = nnetar(stocks[,1],lambda=lambda)
dnn_fit
fcast_tsla = forecast(dnn_fit,PI=T,h=100)
autoplot(fcast_tsla)
accuracy(dnn_fit)
