#prophet model with no regressors 
library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(prophet)
library(MLmetrics)
data1 <- read_excel("data.xlsx")
# data1=mutate(data1,ds=date,y=close)
colnames(data1) <- c("ds","y")
# data1=column_to_rownames(data1,var="ds")
ggplot(data1,aes(x=ds,y=y))+geom_line()

test_set <- read_excel("svm testing set.xlsx")
test_set <- test_set[,c("Date","Close")]
colnames(test_set) <- c("ds","y")
full_set <- rbind(data1,test_set)
###### PROPHET MODEL ########  
changepointss=c("2017-07-14","2017-11-05",
                "2017-12-05","2018-01-21",
                "2018-04-28","2018-09-08",
                "2018-10-18","2018-11-13")
m=prophet(growth = "linear",changepoints = changepointss,
          yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
m=add_seasonality(m,name='monthly',period=31,fourier.order=12) 
m=add_seasonality(m,name='daily',period=1,fourier.order=15) 
m=add_seasonality(m,name='yearly',period=240,fourier.order=20) 
m=add_seasonality(m,name = 'weekly',period = 7,fourier.order = 20)
m=fit.prophet(m,data1)

####### PROPHET MODEL AUTO CHANGEPOINTS #########
##check whether the changepoints given automatically by the function are the same as that of what we have given
n=prophet(data1,growth = "linear",n.changepoints = 4,changepoint.prior.scale = 0.0015,
          yearly.seasonality = TRUE,daily.seasonality = TRUE,weekly.seasonality = FALSE)
##even after specifying the n.changepoints as 8, the changepoints given are different than those of what we manually obtained. Thus, the method of selection of changepoints from this function is different than the mthod we adhered to.
##even though the automatically given changepoints are considered, the forecasts have the same plot as that of the model with us giving the changepoints

######## PROPHET MODEL MULTIPLICATIVE #############
multiplicative_prophet<- prophet(growth='linear',changepoints = changepointss, 
                                 seasonality.mode = "multiplicative",seasonality.prior.scale = 1,
                                 yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
multiplicative_prophet<- multiplicative_prophet %>% 
  add_seasonality(name='monthly',period=31,fourier.order = 12) %>%
  add_seasonality(name='daily',period = 1,fourier.order = 15) %>%
  add_seasonality(name='yearly',period=240,fourier.order = 20) %>%
  add_seasonality(name="weekly",period = 7,fourier.order = 20)
multiplicative_prophet <- fit.prophet(multiplicative_prophet,data1)
  
########### PREDICTION ############
future=make_future_dataframe(m,periods = 248,freq = "day") #future contains 827 data points, which means it contains, earlier data points and the new data points
future_n=make_future_dataframe(n,periods = 248,freq = "day") 
future_multiplicative<-make_future_dataframe(multiplicative_prophet,periods=248,freq="day")
# View(future)
# tail(future,n=248L) #shows the new data points
forecast = predict(m,future) #prediction is for the entire 827 data points
forecast_n = predict(n,future_n) #prediction is for the entire 827 data points
forecast_multiplicative<-predict(multiplicative_prophet,future_multiplicative)
# tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')],n=248L)

########## CHECKING CHANGEPOINTS #########
plot(n,forecast_n)+ add_changepoints_to_plot(n)
plot(m,forecast)+add_changepoints_to_plot(m)
plot(multiplicative_prophet,forecast_multiplicative)

prophet.pred=head(tail(forecast$yhat,n=248L),1) #1st forecast

###### PLOTTING #######
#plot
plot(m,forecast) + 
  labs(
    title = "Forecast for ....."
    , subtitle = paste0(
      "Model Desc - fbProphet - Forecast = "
      , round(prophet.pred, 3)
    )
    , x = ""
    , y = ""
  ) 

multiplicative_df <- full_set
multiplicative_df$yhat <- forecast_multiplicative$yhat
multiplicative_df <- gather(multiplicative_df,type,value,c("y","yhat"))
ggplot(multiplicative_df,aes(x=ds,y=value))+geom_line(aes(color=type),size=0.25)
#advanced plot
# dyplot.prophet(m,forecast)

#components plot
prophet_plot_components(m,forecast)
prophet_plot_components(multiplicative_prophet,forecast_multiplicative)
############### MAPE ##############
MAPE(y_pred = forecast$yhat[580:827],y_true=test_set$y)
MAPE(y_pred = forecast_n$yhat[580:827],y_true=test_set$y)
MAPE(y_pred = forecast_multiplicative$yhat[580:827],y_true = test_set$y)
######### CROSS VALIDATION #########
# results_m=cross_validation(m,horizon=30,units="days") ##check
#performance_metrics(results_m)

#results_multiplicative = cross_validation(multiplicative_prophet,horizon=30,units="days")
#performance_metrics(results_multiplicative)


