library(readxl)
library(MLmetrics)
Bitcoin <- read_excel("Bitcoin.xlsx")
Close.ts=ts(Bitcoin$Close,frequency = 31)
trainset=Close.ts[1:579]
trainset 
testset=Close.ts[580:length(Close.ts)] #(30% of the dataset) #as mentioned in the research paper
testset


library(readxl)
data1 <- read_excel("data.xlsx")
View(data1)

library(dplyr)
data1=mutate(data1,ds=date,y=close)
data1

library(tibble)
data1=column_to_rownames(data1,var="date")
data1

library(ggplot2)
ggplot(data1,aes(x=ds,y=y))+geom_line()


library(prophet)

changepointss=c("2017-07-14","2017-11-05","2017-12-05","2018-01-21","2018-04-28","2018-09-08","2018-10-18","2018-11-13")
m=prophet(growth = "linear",changepoints = changepointss ,yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
m=add_seasonality(m,name='monthly',period=31,fourier.order=12) 
m=add_seasonality(m,name='daily',period=1,fourier.order=15) 
m=add_seasonality(m,name='yearly',period=240,fourier.order=20) 
m=add_seasonality(m,name = 'weekly',period = 7,fourier.order = 20)
m=fit.prophet(m,data1)

##check whether the changepoints given automatically by the function are the same as that of what we have given
#n=prophet(data1,growth = "linear",n.changepoints = 8,yearly.seasonality = TRUE,daily.seasonality = TRUE,weekly.seasonality = FALSE)
#n
##even after specifying the n.changepoints as 8, the changepoints given are different than those of what we manually obtained. Thus, the method of selection of changepoints from this function is different than the mthod we adhered to.
##even though the automatically given changepoints are considered, the forecasts have the same plot as that of the model with us giving the changepoints

future_m=make_future_dataframe(m,periods = 248,freq = "day") #future contains 827 data points, which means it contains, earlier data points and the new data points
View(future_m)
tail(future_m,n=248L) #shows the new data points
forecast_m = predict(m,future_m) #prediction is for the entire 827 data points
tail(forecast_m[c('ds','yhat','yhat_lower','yhat_upper')],n=248L)

prophet.pred=head(tail(forecast_m$yhat,n=248L),1) #1st forecast

#plot
plot(m,forecast_m) + 
  labs(
    title = "Forecast for ....."
    , subtitle = paste0(
      "Model Desc - fbProphet - Forecast = "
      , round(prophet.pred, 3)
    )
    , x = ""
    , y = ""
  ) 

#advanced plot
dyplot.prophet(m,forecast_m)


#components plot
prophet_plot_components(m,forecast_m)

##plot test set and test set forecasts

##MAPE
##additive
prophet_bitcoin_predicted=forecast_m$yhat[580:827]

MAPE(prophet_bitcoin_predicted,testset) ##0.7578042



#multiplicative - provides negative prices
