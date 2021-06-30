library(MLmetrics)
library(prophet)
library(readxl)
data2 <- read_excel("data2.xlsx")
data2 <-subset(data2,select=-ethereum)
Litecoin_prophet <- read_excel("Litecoin_prophet.xlsx")
colnames(Litecoin_prophet)[2] <- "litecoin"
#View(data2)

library(dplyr)
data2=mutate(data2,ds=date,y=close)
#data2

library(tibble)
data2=column_to_rownames(data2,var="date")
data2


library(ggplot2)
#ggplot(data2,aes(x=ds,y=y))+geom_line()



##lagging data

lag_dataset<-function(dataframe,steps){
  library(tis)
  library(dplyr)
  for(col in colnames(dataframe)){
    for(i in 1:steps){
      column_name <- paste(col,i,sep="_t_")
      # print(class(dplyr::pull(dataframe,col)))
      dataframe <- dataframe %>% mutate(
        # !!column_name := list(dataframe[,col])
        !!column_name := lag(dplyr::pull(dataframe,col),i)
        # !!column_name := lag(dataframe[,col],i)
      ) %>%
        dplyr::select(sort(names(.)))
      # print(dataframe)
    }
  }
  
  na.omit(dataframe)
  #dataframe
}



# for(lag_steps in 1:8){
# step=1
# lag_steps = step
lag_steps = 2
print("lagging data")
print("##############")
print(lag_steps)
print("################")
data2_lagged <- lag_dataset(subset(data2,select=-ds),steps = lag_steps)
data2_lagged$ds <- data2$ds[(lag_steps+1):nrow(data2)]
Litecoin_prophet_lagged <- lag_dataset(subset(Litecoin_prophet,select = -Date),steps = lag_steps)

changepointss=c("2017-07-14","2017-11-05","2017-12-05","2018-01-21","2018-04-28","2018-09-08","2018-10-18","2018-11-13")

p=prophet(growth = "linear",changepoints = changepointss,yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
p=add_seasonality(p,name='monthly',period=31,fourier.order=12) 
p=add_seasonality(p,name='daily',period=1,fourier.order=15) 
p=add_seasonality(p,name='yearly',period=240,fourier.order=20) 
for(i in 1:lag_steps){
  regressor_name <- paste("litecoin_t_",i,sep="")
  print("adding regressor")
  print(regressor_name)
  p=add_regressor(p,regressor_name,standardize = FALSE)
}
#p=add_regressor(p,'ethereum',standardize = FALSE)
p=fit.prophet(p,data2_lagged)
print("Made model")
future_p=make_future_dataframe(p,periods = 248,freq = "day")
for(i in 1:lag_steps){
  regressor_name <- paste("litecoin_t_",i,sep="")
  print("filling future p")
  print(regressor_name)
  future_p <- future_p %>%
    left_join((p$history%>% dplyr::select(ds,!!regressor_name)),by=c('ds'='ds'))
}
#%>% left_join((p$history%>% dplyr::select(ds,ethereum)),by=c('ds'='ds'))
View(future_p)
# forecast_p=predict(p,future_p) #error
#View(forecast_p)
print("Adding litecoin training data")
for(i in 1:lag_steps){
  regressor_name <- paste("litecoin_t_",i,sep="")
  future_p[regressor_name]<- Litecoin_prophet_lagged[regressor_name]
  # future_p$litecoin_t_1=Litecoin_prophet_lagged$L_Close_t_1
}
# future_p$litecoin_t_1[1:(580-lag_steps-1)]=Litecoin_prophet_lagged$L_Close_t_1
View(future_p)


forecast_p=predict(p,future_p)


#dyplot.prophet(p,forecast_p,main="Prophet 2 Actual Set vs Forecasts")
#prophet_plot_components(p,forecast_p)

#p$params$beta%*% 
  
  #as.matrix(p$train.component.cols)

##MAPE
##additive
Bitcoin<-read_excel("Bitcoin.xlsx")
Close.ts <- ts(Bitcoin$Close,frequency=31)
testset <- Close.ts[580:length(Close.ts)]
prophet_regressors2_predicted=forecast_p$yhat[(580-lag_steps):(827-lag_steps)]
mape_prophet2=MAPE(prophet_regressors2_predicted,testset) #0.7539401
print("MAPE FOR STEP >>>>>>>>>>>>>>>>")
print(lag_steps)
print(mape_prophet2)
print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
# }

