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


summary(data1)
str(data1)


library(prophet)
library(MLmetrics)

changepointss=c("2017-07-14","2017-11-05","2017-12-05","2018-01-21","2018-04-28","2018-09-08","2018-10-18","2018-11-13")
m=prophet(growth = "linear",changepoints = changepointss ,yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
m=add_seasonality(m,name='monthly',period=31,fourier.order=12) 
m=add_seasonality(m,name='daily',period=1,fourier.order=15) 
m=add_seasonality(m,name='yearly',period=240,fourier.order=20) 
m=add_seasonality(m,name = 'weekly',period = 7,fourier.order = 20)
m=fit.prophet(m,data1)
m

##check whether the changepoints given automatically by the function are the same as that of what we have given
n=prophet(data1,growth = "linear",n.changepoints = 8,yearly.seasonality = TRUE,daily.seasonality = TRUE,weekly.seasonality = FALSE)
n
##even after specifying the n.changepoints as 8, the changepoints given are different than those of what we manually obtained. Thus, the method of selection of changepoints from this function is different than the mthod we adhered to.
##even though the automatically given changepoints are considered, the forecasts have the same plot as that of the model with us giving the changepoints

future=make_future_dataframe(m,periods = 248,freq = "day") #future contains 827 data points, which means it contains, earlier data points and the new data points
View(future)
tail(future,n=248L) #shows the new data points
forecast = predict(m,future) #prediction is for the entire 827 data points
tail(forecast[c('ds','yhat','yhat_lower','yhat_upper')],n=248L)

prophet.pred=head(tail(forecast$yhat,n=248L),1) #1st forecast

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

#advanced plot
#################
df_for_plotting <- function(m, fcst) {
  # Make sure there is no y in fcst
  fcst$y <- NULL
  df <- m$history %>%
    dplyr::select(ds, y) %>%
    dplyr::full_join(fcst, by = "ds") %>%
    dplyr::arrange(ds)
  return(df)
}
dyplot.prophet <- function(x, fcst, uncertainty=TRUE, 
                           ...) 
{
  forecast.label='Predicted'
  actual.label='Actual'
  # create data.frame for plotting
  df <- df_for_plotting(x, fcst)
  
  # build variables to include, or not, the uncertainty data
  if(uncertainty && x$uncertainty.samples && exists("yhat_lower", where = df))
  {
    colsToKeep <- c('y', 'yhat', 'yhat_lower', 'yhat_upper')
    forecastCols <- c('yhat_lower', 'yhat', 'yhat_upper')
  } else
  {
    colsToKeep <- c('y', 'yhat')
    forecastCols <- c('yhat')
  }
  # convert to xts for easier date handling by dygraph
  dfTS <- xts::xts(df %>% dplyr::select_(.dots=colsToKeep), order.by = df$ds)
  
  # base plot
  dyBase <- dygraphs::dygraph(dfTS,...)
  
  presAnnotation <- function(dygraph, x, text) {
    dygraph %>%
      dygraphs::dyAnnotation(x, text, text, attachAtBottom = TRUE)
  }
  
  dyBase <- dyBase %>%
    # plot actual values
    dygraphs::dySeries(
      'y', label=actual.label, color='black', drawPoints=TRUE, strokeWidth=0
    ) %>%
    # plot forecast and ribbon
    dygraphs::dySeries(forecastCols, label=forecast.label, color='blue') %>%
    # allow zooming
    dygraphs::dyRangeSelector() %>% 
    # make unzoom button
    dygraphs::dyUnzoom()
  if (!is.null(x$holidays)) {
    for (i in 1:nrow(x$holidays)) {
      # make a gray line
      dyBase <- dyBase %>% dygraphs::dyEvent(
        x$holidays$ds[i],color = "rgb(200,200,200)", strokePattern = "solid")
      dyBase <- dyBase %>% dygraphs::dyAnnotation(
        x$holidays$ds[i], x$holidays$holiday[i], x$holidays$holiday[i],
        attachAtBottom = TRUE)
    }
  }
  return(dyBase)
}

#################
dyplot.prophet(m,forecast,main="Insert your Title here")

#components plot
prophet_plot_components(m,forecast)

##plot test set and test set forecasts
test_df <- read_excel("svm testing set.xlsx")
##MAPE
MAPE(forecast$yhat[580:827], test_df$Close)
results1=cross_validation(m,horizon=30,units="days") ##check
performance_metrics(results1)
