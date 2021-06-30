library(MLmetrics)

library(readxl)
data2 <- read_excel("data2.xlsx")
View(data2)

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

p=prophet(growth = "linear",changepoints = changepointss,yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
p=add_seasonality(p,name='monthly',period=31,fourier.order=12) 
p=add_seasonality(p,name='daily',period=1,fourier.order=15) 
p=add_seasonality(p,name='yearly',period=240,fourier.order=20) 
p=add_regressor(p,'litecoin',standardize = FALSE)
p=add_regressor(p,'ethereum',standardize = FALSE)
p=fit.prophet(p,data2)

future_p=make_future_dataframe(p,periods = 248,freq = "day")%>% 
  left_join((p$history%>% select(ds,litecoin)),by=c('ds'='ds'))%>% 
  left_join((p$history%>% select(ds,ethereum)),by=c('ds'='ds'))
View(future_p)
# forecast_p=predict(p,future_p) #error
# View(forecast_p)


Litecoin_prophet <- read_excel("Litecoin_prophet.xlsx")
View(Litecoin_prophet)

Litecoin_prophet=mutate(Litecoin_prophet,ds=Date,y=L_Close)

Litecoin_prophet=column_to_rownames(Litecoin_prophet,var="Date")


changepoints_litecoin = c("2017-12-06","2018-02-09","2018-04-15","2018-08-04","2018-10-08")
litecoin_holidays <- data.frame(
  holiday = 'litecoin fiesta',
  ds = as.Date(c("2017-12-06","2017-12-10","2018-02-09","2018-04-15","2018-08-04","2018-10-08")),
  lower_window = 0,
  upper_window = 1
)
k=prophet(growth = "linear", holidays = litecoin_holidays,
          # changepoints = changepoints_litecoin, 
          # n.changepoints = 5, 
          changepoint.prior.scale = 0.0001,
          daily.seasonality = FALSE,yearly.seasonality = FALSE,weekly.seasonality = FALSE)
k=add_seasonality(k,name='monthly',period=31,fourier.order=12) 
k=add_seasonality(k,name='daily',period=1,fourier.order=15) 
k=add_seasonality(k,name='yearly',period=240,fourier.order=20)
k=fit.prophet(k,Litecoin_prophet)

future_litecoin=make_future_dataframe(k,periods = 248,freq = "day")

forecast_litecoin = predict(k,future_litecoin)

forecasts_l=forecast_litecoin$yhat

plot(k,forecast_litecoin)

dyplot.prophet(k,forecast_litecoin)
plot(k,forecast_litecoin)+add_changepoints_to_plot(k)


future_p$litecoin[580:827]=tail(forecasts_l,n=248L)
View(future_p)





Ethereum_prophet <- read_excel("Ethereum_prophet.xlsx")
View(Ethereum_prophet)

Ethereum_prophet=mutate(Ethereum_prophet,ds=Date,y=E_Close)

Ethereum_prophet=column_to_rownames(Ethereum_prophet,var="Date")



e=prophet(growth = "linear",# changepoint.prior.scale = 0.1,
          daily.seasonality = FALSE,yearly.seasonality = FALSE,weekly.seasonality = FALSE)
e=add_seasonality(e,name='monthly',period=31,fourier.order=12) 
e=add_seasonality(e,name='daily',period=1,fourier.order=15) 
e=add_seasonality(e,name='yearly',period=240,fourier.order=20)
e=fit.prophet(e,Ethereum_prophet)

future_ethereum=make_future_dataframe(e,periods = 248,freq = "day")

forecast_ethereum = predict(e,future_ethereum)

forecasts_e=forecast_ethereum$yhat

plot(e,forecast_ethereum)


future_p$ethereum[580:827]=tail(forecasts_e,n=248L)
View(future_p)

forecast_p=predict(p,future_p)

dyplot.prophet(p,forecast_p)
prophet_plot_components(p,forecast_p)

p$params$beta%*% 
  
as.matrix(m$train.component.cols)

##plot test set and test set forecasts

##MAPE
##additive
library(readxl)
library(MLmetrics)
Bitcoin <- read_excel("Bitcoin.xlsx")
Close.ts=ts(Bitcoin$Close,frequency = 31)
trainset=Close.ts[1:579]
trainset 
testset=Close.ts[580:length(Close.ts)] #(30% of the dataset) #as mentioned in the research paper
testset
prophet_regressors2_predicted=tail(forecast_p$yhat,n=248L)
MAPE(prophet_regressors2_predicted,testset) #0.3561119


#multiplicative - provides negative prices

