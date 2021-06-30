###volume divided by 100000 and MC divided by 10000000

library(MLmetrics)

library(readxl)
data3 <- read_excel("data3.xlsx")
View(data3)

library(dplyr)
data3=mutate(data3,ds=date,y=close,
             Volume_divided=Volume/100000,
             MC_divided=MC/10000000)
data3

library(tibble)
data3=column_to_rownames(data3,var="date")
data3

library(ggplot2)
ggplot(data3,aes(x=ds,y=y))+geom_line()


library(prophet)
changepointss=c("2017-07-14","2017-11-05","2017-12-05","2018-01-21","2018-04-28","2018-09-08","2018-10-18","2018-11-13")

r=prophet(growth = "linear",changepoints = changepointss,yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
r=add_seasonality(r,name='monthly',period=31,fourier.order=12) 
r=add_seasonality(r,name='daily',period=1,fourier.order=15) 
r=add_seasonality(r,name='yearly',period=240,fourier.order=20) 
r=add_regressor(r,'litecoin',standardize = FALSE)
r=add_regressor(r,'ethereum',standardize = FALSE)
r=add_regressor(r,'volume',standardize = FALSE)
r=add_regressor(r,'MC',standardize = FALSE)
r=fit.prophet(r,data3)

future_r=make_future_dataframe(r,periods = 248,freq = "day")%>% 
  left_join((r$history%>% select(ds,litecoin)),by=c('ds'='ds'))%>% 
  left_join((r$history%>% select(ds,ethereum)),by=c('ds'='ds'))%>% 
  left_join((r$history%>% select(ds,volume)),by=c('ds'='ds'))%>% 
  left_join((r$history%>% select(ds,MC)),by=c('ds'='ds'))
#View(future_r)
forecast_r=predict(r,future_r) #error
#View(forecast_r)


####@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Litecoin_prophet <- read_excel("Litecoin_prophet.xlsx")
#View(Litecoin_prophet)

Litecoin_prophet=mutate(Litecoin_prophet,ds=Date,y=L_Close)

Litecoin_prophet=column_to_rownames(Litecoin_prophet,var="Date")



k=prophet(growth = "linear",daily.seasonality = FALSE,yearly.seasonality = FALSE,weekly.seasonality = FALSE)
k=add_seasonality(k,name='monthly',period=31,fourier.order=12) 
k=add_seasonality(k,name='daily',period=1,fourier.order=15) 
k=add_seasonality(k,name='yearly',period=240,fourier.order=20)
k=fit.prophet(k,Litecoin_prophet)

future_litecoin=make_future_dataframe(k,periods = 248,freq = "day")

forecast_litecoin = predict(k,future_litecoin)

forecasts_l=forecast_litecoin$yhat

plot(k,forecast_litecoin)


future_r$litecoin[580:827]=tail(forecasts_l,n=248L)
#View(future_r)

###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



Ethereum_prophet <- read_excel("Ethereum_prophet.xlsx")
#View(Ethereum_prophet)

Ethereum_prophet=mutate(Ethereum_prophet,ds=Date,y=E_Close)

Ethereum_prophet=column_to_rownames(Ethereum_prophet,var="Date")



e=prophet(growth = "linear",daily.seasonality = FALSE,yearly.seasonality = FALSE,weekly.seasonality = FALSE)
e=add_seasonality(e,name='monthly',period=31,fourier.order=12) 
e=add_seasonality(e,name='daily',period=1,fourier.order=15) 
e=add_seasonality(e,name='yearly',period=240,fourier.order=20)
e=fit.prophet(e,Ethereum_prophet)

future_ethereum=make_future_dataframe(e,periods = 248,freq = "day")

forecast_ethereum = predict(e,future_ethereum)

forecasts_e=forecast_ethereum$yhat

plot(e,forecast_ethereum)


future_r$ethereum[580:827]=tail(forecasts_e,n=248L)
#View(future_r)


###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


Volume_prophet <- read_excel("Volume_prophet.xlsx")
#View(Volume_prophet)

Volume_prophet=mutate(Volume_prophet,ds=Date,y=Volume_close)

Volume_prophet=column_to_rownames(Volume_prophet,var="Date")




v=prophet(growth = "linear",daily.seasonality = FALSE,yearly.seasonality = FALSE,weekly.seasonality = FALSE)
v=add_seasonality(v,name='monthly',period=31,fourier.order=12) 
v=add_seasonality(v,name='daily',period=1,fourier.order=15) 
v=add_seasonality(v,name='yearly',period=240,fourier.order=20)
v=fit.prophet(v,Volume_prophet)

future_volume=make_future_dataframe(v,periods = 248,freq = "day")

forecast_volume = predict(v,future_volume)

forecasts_v=forecast_volume$yhat

plot(v,forecast_volume)


future_r$volume[580:827]=tail(forecasts_v,n=248L)
#View(future_r)


##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



MC_prophet <- read_excel("MC_prophet.xlsx")
#View(MC_prophet)

MC_prophet=mutate(MC_prophet,ds=Date,y=MarketCap_close)

MC_prophet=column_to_rownames(MC_prophet,var="Date")



MC=prophet(growth = "linear",daily.seasonality = FALSE,yearly.seasonality = FALSE,weekly.seasonality = FALSE)
MC=add_seasonality(MC,name='monthly',period=31,fourier.order=12) 
MC=add_seasonality(MC,name='daily',period=1,fourier.order=15) 
MC=add_seasonality(MC,name='yearly',period=240,fourier.order=20)
MC=fit.prophet(MC,MC_prophet)

future_MC=make_future_dataframe(MC,periods = 248,freq = "day")

forecast_MC = predict(MC,future_MC)

forecasts_MC=forecast_MC$yhat

plot(MC,forecast_MC)


future_r$MC[580:827]=tail(forecasts_MC,n=248L)
#View(future_r)

##@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


forecast_r=predict(r,future_r)

dyplot.prophet(r,forecast_r,main="Prophet 3 Actual Set vs Forecasts")
prophet_plot_components(r,forecast_r)


prophet_regressors4_predicted=forecast_r$yhat[580:827]
mape_prophet3=MAPE(prophet_regressors4_predicted,testset) ##0.4887653
mape_prophet3
