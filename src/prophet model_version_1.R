library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)
library(prophet)
library(magrittr)
library(MLmetrics)
Bitcoin <- read_excel("data/Bitcoin.xlsx")
data1 <- Bitcoin %>% 
  select(Date, Close) %>% 
  rename(date = Date, y = Close) %>% 
  mutate(ds = date) %>% 
  column_to_rownames(var = "date")

changepointss=c("2017-07-14","2017-11-05","2017-12-05","2018-01-21","2018-04-28","2018-09-08","2018-10-18","2018-11-13")
m=prophet(growth = "linear",changepoints = changepointss ,yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
m=add_seasonality(m,name='monthly',period=31,fourier.order=12) 
m=add_seasonality(m,name='daily',period=1,fourier.order=15) 
m=add_seasonality(m,name='yearly',period=240,fourier.order=20) 
m=add_seasonality(m,name = 'weekly',period = 7,fourier.order = 20)
m=fit.prophet(m,data1[1:579,])

future_m=make_future_dataframe(m,periods = 248,freq = "day") 
forecast_m = predict(m,future_m) 

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

#advanced plot
dyplot.prophet(m,forecast_m,
               #main="Plot of Actual vs Fitted - Prophet Model 1",
               xlab="Date",ylab="Value")

#components plot
prophet_plot_components(m,forecast_m)

Close.ts=ts(Bitcoin$Close,frequency = 31)
testset=Close.ts[580:length(Close.ts)]

prophet1=data.frame(Bitcoin$Date[580:827],forecast_m$yhat[580:827],testset)
colnames(prophet1)[1]="Date"
colnames(prophet1)[2]="Predicted"
colnames(prophet1)[3]="Actual"


library(ggplot2)
ggplot(prophet1,aes(x=Date))+geom_line(aes(y=Predicted,colour="Predicted"))+
  geom_line(aes(y=Actual,colour="Actual"),lwd=0.2)+labs(colour="Series")+
  #ggtitle(paste("Plot of Forecasts - Prophet Model 1"))+
  theme(#plot.title = element_text(hjust=0.5,face = "bold",size=12),
    axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())


prophet_bitcoin_predicted=forecast_m$yhat[580:827]

mape_prophet1=MAPE(prophet_bitcoin_predicted,testset) ##0.7578042
mape_prophet1
