library(MLmetrics)
library(prophet)
library(readxl)
library(tidyr)
data2 <- read_excel("data2.xlsx")
data2 <-subset(data2,select=-ethereum)
Litecoin_prophet <- read_excel("Litecoin_prophet.xlsx")
ethereum_prophet <- read_excel("Ethereum_prophet.xlsx")
colnames(Litecoin_prophet)[2] <- "litecoin"
colnames(ethereum_prophet)[1] <- "ds"
colnames(ethereum_prophet)[2] <- "ethereum"
#View(data2)

library(dplyr)
data2=mutate(data2,ds=date,y=close)
#data2
data2 <- left_join(data2,ethereum_prophet,by="ds")
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


test_df <- read_excel("svm testing set.xlsx")
step_series_df <- data.frame(
  ds = test_df$Date,
  Close = test_df$Close
)

for(lag_steps in 1:8){
# step=1
# lag_steps = step
# lag_steps = 3

print("lagging data")
print("##############")
print(lag_steps)
print("################")
data2_lagged <- lag_dataset(subset(data2,select=-ds),steps = lag_steps)
data2_lagged$ds <- data2$ds[(lag_steps+1):nrow(data2)]
Litecoin_prophet_lagged <- lag_dataset(subset(Litecoin_prophet,select = -Date),steps = lag_steps)
ethereum_prophet_lagged <- lag_dataset(subset(ethereum_prophet,select= -ds),steps = lag_steps)

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
for(i in 1:lag_steps){
  regressor_name <- paste("ethereum_t_",i,sep="")
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
for(i in 1:lag_steps){
  regressor_name <- paste("ethereum_t_",i,sep="")
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
for(i in 1:lag_steps){
  regressor_name <- paste("ethereum_t_",i,sep="")
  future_p[regressor_name]<- ethereum_prophet_lagged[regressor_name]
  # future_p$litecoin_t_1=Litecoin_prophet_lagged$L_Close_t_1
}
# future_p$litecoin_t_1[1:(580-lag_steps-1)]=Litecoin_prophet_lagged$L_Close_t_1
View(future_p)


forecast_p=predict(p,future_p)


print(dyplot.prophet(p,forecast_p,main=paste("Step",lag_steps)))
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
df_long_best<-test_df# [(lag_steps+1):nrow(test_df),]
df_long_best$predicted<-prophet_regressors2_predicted
series_df <- data.frame(
  ds=df_long_best$Date,
  Predicted=df_long_best$predicted
)
colnames(series_df)[2] <- paste("Predicted_step",lag_steps,sep="_")
step_series_df <<- left_join(step_series_df,series_df,by=c("ds"))
}

step_series_long_df <- gather(step_series_df,type,value,colnames(step_series_df)[2:length(colnames(step_series_df))])
#step_series_long_df <- gather(step_series_df,type,value,c("Predicted_step_3","Predicted_step_4"))
ggplot(na.omit(step_series_long_df),aes(x=ds,y=value))+
  geom_line(aes(color=type),size=0.15)+
  geom_point(aes(color=type),size=0.1)+
  theme_minimal()
