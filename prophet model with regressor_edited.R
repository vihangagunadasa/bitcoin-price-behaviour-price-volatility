library(readxl)
data2 <- read_excel("data2.xlsx")
#View(data2)

library(dplyr)
data2=mutate(data2,ds=date,y=close)
data2

library(tibble)
data2=column_to_rownames(data2,var="date")
data2

library(ggplot2)
ggplot(data2,aes(x=ds,y=y))+geom_line()


library(prophet)

changepointss=c("2017-07-14","2017-11-05","2017-12-05","2018-01-21","2018-04-28","2018-09-08","2018-10-18","2018-11-13")

m=prophet(growth = "linear",yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
m=add_seasonality(m,name='monthly',period=31,fourier.order=12) 
m=add_seasonality(m,name='daily',period=1,fourier.order=15) 
m=add_seasonality(m,name='yearly',period=240,fourier.order=20) 
m=add_regressor(m,'litecoin',standardize = FALSE)
m=add_regressor(m,'ethereum',standardize = FALSE)
m=fit.prophet(m,data2)

future_m=make_future_dataframe(m,periods = 248,freq = "day")%>% 
                left_join((m$history%>% select(ds,litecoin)),by=c('ds'='ds'))%>%
                left_join((m$history%>% select(ds,ethereum)),by=c('ds'='ds'))
#View(future_m)
forecasts=predict(m,future_m) #error
#View(forecasts)


Litecoin_prophet <- read_excel("Litecoin_prophet.xlsx")
#View(Litecoin_prophet)
Litecoin_prophet=mutate(Litecoin_prophet,ds=Date,y=L_Close)
Litecoin_prophet=column_to_rownames(Litecoin_prophet,var="Date")
k=prophet(Litecoin_prophet,changepoint.range=0.95,n.changepoints=5,
          daily.seasonality = TRUE,yearly.seasonality = TRUE,weekly.seasonality = TRUE)
# k=prophet(Litecoin_prophet)
future_litecoin=make_future_dataframe(k,periods = 248,freq = "day")
forecast_litecoin = predict(k,future_litecoin)
forecasts_l=forecast_litecoin$yhat
plot(k,forecast_litecoin)
future_m$litecoin[580:827]=tail(forecasts_l,n=248L)
#View(future_m)

#litecoin_svm
train_df <- read_excel("svm training set.xlsx")
colnames(train_df)[1] <- "Date"
test_df <- read_excel("svm testing set.xlsx")
df<-rbind(train_df,test_df)
validation_df<-train_df[floor(nrow(train_df)*0.85):nrow(train_df),]
train_df_wo_validation<-train_df[1:floor(nrow(train_df)*0.85),]
#base model
Litecoin_svmodel <- svm(Litecoin ~ .,data=subset(train_df_wo_validation,select=c(Litecoin,Date)),
                        type="eps-regression",kernel="radial",cost=10000, gamma=10)

Litecoin_predicted <- predict(Litecoin_svmodel,subset(df,select=c(Litecoin,Date)))
Litecoin_df <- subset(df,select=c(Litecoin,Date))
Litecoin_df$Predicted <- Litecoin_predicted
MAPE(Litecoin_df$Predicted,Litecoin_df$Litecoin)
future_m$litecoin[580:827]<-tail(Litecoin_df$Predicted,n=248L)
Litecoin_df$Prophet <- forecasts_l
Litecoin_df_long <-gather(Litecoin_df,type,value,c(Litecoin,Predicted,Prophet))
ggplot(Litecoin_df_long,aes(x=Date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  theme_minimal()

Ethereum_prophet <- read_excel("Ethereum_prophet.xlsx")
#View(Ethereum_prophet)
Ethereum_prophet=mutate(Ethereum_prophet,ds=Date,y=E_Close)
Ethereum_prophet=column_to_rownames(Ethereum_prophet,var="Date")
e=prophet(Ethereum_prophet,daily.seasonality = TRUE,yearly.seasonality = TRUE)
future_ethereum=make_future_dataframe(e,periods = 248,freq = "day")
forecast_ethereum = predict(e,future_ethereum)
forecasts_e=forecast_ethereum$yhat
plot(k,forecast_litecoin)
future_m$ethereum[580:827]=tail(forecasts_e,n=248L)
#View(future_m)

#ethereum svm model


forecasts=predict(m,future_m)
dyplot.prophet(m,forecasts)
prophet_plot_components(m,forecasts)

df$Predicted<-forecasts$yhat
df_long<-gather(df,type,value,c(Close,Predicted))
ggplot(df_long,aes(x=Date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

m$params$beta%*% 
  
as.matrix(m$train.component.cols)

##plot test set and test set forecasts

##MAPE
results1=cross_validation(m,horizon=90,units="days") ##check
performance_metrics(results1)
plot_cross_validation_metric(results1, metric = 'mape')
