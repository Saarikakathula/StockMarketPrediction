library(prophet)
library(ggplot2)
library(plotly)

df <- data.frame(ds = index(stocks),y = as.numeric(stocks[,1]))

prophet_pred = prophet(df)
future = make_future_dataframe(prophet_pred,periods=50)
fcastprophet = predict(prophet_pred,future)
fcastprophet
plot_forecast <- plot(prophet_pred,fcastprophet)
ggplotly(plot_forecast)

#Creating train prediction dataset to compare real data
dataprediction = data.frame(fcastprophet$ds,fcastprophet$yhat)

View(dataprediction)
trainlen = length(stocks$MSFT.Close)
data_prediction = dataprediction[c(1:trainlen),]
#Visualizing train prediction vs real data
p= ggplot()+
  geom_smooth(aes(x= data_prediction$fcastprophet.ds,y= stocks$MSFT.Close),
              colour="blue",level=0.99,fill="#69b3a2",se=T)+
  geom_point(aes(x= dataprediction$fcastprophet.ds,y=dataprediction$fcastprophet.yhat))+
  xlab("ds")+
  ylab("y= MSFT.Close")+
  ggtitle("Training Prediction vs. Real Data:Prophet")
p
#Creating Cross Validation
accuracy(dataprediction$fcastprophet.yhat,df$y)
prophet_plot_components(prophet_pred,fcastprophet)

##############AMZN

df <- data.frame(ds = index(stocks),y = as.numeric(stocks[,2]))

prophet_pred = prophet(df)
future = make_future_dataframe(prophet_pred,periods=50)
fcastprophet = predict(prophet_pred,future)
plot_forecast <- plot(prophet_pred,fcastprophet)
ggplotly(plot_forecast)
#Creating train prediction dataset to compare real data
dataprediction = data.frame(fcastprophet$ds,fcastprophet$yhat)
View(dataprediction)
trainlen = length(stocks$AMZN.Close)
data_prediction = dataprediction[c(1:trainlen),]
#Visualizing train prediction vs real data
p= ggplot()+
  geom_smooth(aes(x= data_prediction$fcastprophet.ds,y= stocks$AMZN.Close),
              colour="blue",level=0.99,fill="#69b3a2",se=T)+
  geom_point(aes(x= dataprediction$fcastprophet.ds,y=dataprediction$fcastprophet.yhat))+
  xlab("ds")+
  ylab("y= AMZN.Close")+
  ggtitle("Training Prediction vs. Real Data:Prophet")
p

#Creating Cross Validation
accuracy(dataprediction$fcastprophet.yhat,df$y)
prophet_plot_components(prophet_pred,fcastprophet)

################# NVDA

df <- data.frame(ds = index(stocks),y = as.numeric(stocks[,3]))

prophet_pred = prophet(df)
future = make_future_dataframe(prophet_pred,periods=30)
fcastprophet = predict(prophet_pred,future)
plot_forecast <- plot(prophet_pred,fcastprophet)
ggplotly(plot_forecast)
#Creating train prediction dataset to compare real data
dataprediction = data.frame(fcastprophet$ds,fcastprophet$yhat)
View(dataprediction)
trainlen = length(stocks$NVDA.Close)
data_prediction = dataprediction[c(1:trainlen),]
#Visualizing train prediction vs real data
p= ggplot()+
  geom_smooth(aes(x= data_prediction$fcastprophet.ds,y= stocks$NVDA.Close),
              colour="blue",level=0.99,fill="#69b3a2",se=T)+
  geom_point(aes(x= dataprediction$fcastprophet.ds,y=dataprediction$fcastprophet.yhat))+
  xlab("ds")+
  ylab("y= NVDA.Close")+
  ggtitle("Training Prediction vs. Real Data:Prophet")
p
#Creating Cross Validation
accuracy(dataprediction$fcastprophet.yhat,df$y)
prophet_plot_components(prophet_pred,fcastprophet)

################# TSLA
df <- data.frame(ds = index(stocks),y = as.numeric(stocks[,4]))

prophet_pred = prophet(df)
future = make_future_dataframe(prophet_pred,periods=30)
fcastprophet = predict(prophet_pred,future)
plot_forecast <- plot(prophet_pred,fcastprophet)
ggplotly(plot_forecast)
#Creating train prediction dataset to compare real data
dataprediction = data.frame(fcastprophet$ds,fcastprophet$yhat)
View(dataprediction)
trainlen = length(stocks$TSLA.Close)
data_prediction = dataprediction[c(1:trainlen),]
#Visualizing train prediction vs real data
p= ggplot()+
  geom_smooth(aes(x= data_prediction$fcastprophet.ds,y= stocks$TSLA.Close),
              colour="blue",level=0.99,fill="#69b3a2",se=T)+
  geom_point(aes(x= dataprediction$fcastprophet.ds,y=dataprediction$fcastprophet.yhat))+
  xlab("ds")+
  ylab("y= TSLA.Close")+
  ggtitle("Training Prediction vs. Real Data:Prophet")
p
#Creating Cross Validation
accuracy(dataprediction$fcastprophet.yhat,df$y)

prophet_plot_components(prophet_pred,fcastprophet)
