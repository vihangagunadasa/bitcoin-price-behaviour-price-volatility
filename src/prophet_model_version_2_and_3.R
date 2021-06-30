library(MLmetrics)
library(prophet)
library(readxl)
library(tidyr)
library(dplyr)
library(tibble)
library(ggplot2)
source("src/lag_dataset.R")
source("src/dyplot_prophet.R")

train_df <- read_excel("data/training set.xlsx")
test_df <- read_excel("data/testing set.xlsx")
colnames(train_df)[1] <- "Date"
colnames(train_df)[4] <- "Market_Cap"
colnames(test_df)[4] <- "Market_Cap"

volume_divisor<- 10^7
mc_divisor<-10^8

need_to_add_close_lagged<-TRUE
need_to_add_litecoin_ethereum_lagged<-FALSE
need_to_add_mc_volume_divided<-FALSE
perform_divide_mutation <- FALSE
divide_mutation <- function(df){
  mutate(df,
         ds=Date, y=Close,
         Volume_divided=Volume/volume_divisor,
         Market_Cap_divided=Market_Cap/mc_divisor)
}
if(perform_divide_mutation){
train_df <- train_df %>% divide_mutation() %>% subset(select=-Date)
  
test_df <- test_df %>% divide_mutation() %>% subset(select=-Date)
}else{
  train_df <- train_df %>% mutate(ds=Date, y=Close) %>% subset(select=-Date)
  test_df <- test_df %>% mutate(ds=Date, y=Close) %>% subset(select=-Date)
}
train_df <-subset(train_df,select=-Variance)
test_df <-subset(test_df,select=-Variance)
df <- rbind(train_df,test_df)

start_lag_step<-1
end_lag_step<-8

mape_metrics <- data.frame(
  Step = seq(start_lag_step,end_lag_step),
  MAPE = seq(start_lag_step,end_lag_step)
)

for(lag_steps in start_lag_step:end_lag_step){
  
  #first we lag then we add the dates and then we split
  df_lagged <- lag_dataset(subset(df,select=-ds),
                           steps=lag_steps)
  df_lagged$ds <- df$ds[(lag_steps+1):nrow(df)]
  
  testing_indice = (579-lag_steps)
  train_df_lagged <- df_lagged[1:testing_indice,]
  test_df_lagged <- df_lagged[(testing_indice+1):nrow(df_lagged),]
  
  changepointss=c("2017-07-14","2017-11-05",
                  "2017-12-05","2018-01-21",
                  "2018-04-28","2018-09-08",
                  "2018-10-18","2018-11-13")
  
  main_model=prophet(growth = "linear",
                     changepoints = changepointss,
                     yearly.seasonality = FALSE,
                     daily.seasonality = FALSE,
                     weekly.seasonality = FALSE)
  main_model=add_seasonality(main_model,name='monthly',period=31,fourier.order=12) 
  main_model=add_seasonality(main_model,name='daily',period=1,fourier.order=15) 
  main_model=add_seasonality(main_model,name='yearly',period=240,fourier.order=20) 
  
  if(need_to_add_litecoin_ethereum_lagged){
    for(i in 1:lag_steps){
      regressor_name <- paste("Litecoin_t_",i,sep="")
      print("adding regressor")
      print(regressor_name)
      main_model=add_regressor(main_model,regressor_name,standardize = F)
    }
    for(i in 1:lag_steps){
      regressor_name <- paste("Ethereum_t_",i,sep="")
      print("adding regressor") 
      print(regressor_name)
      main_model=add_regressor(main_model,regressor_name,standardize = F)
    }
  }
  
  if(need_to_add_mc_volume_divided){
    if(perform_divide_mutation){
    for(i in 1:lag_steps){
      regressor_name <- paste("Market_Cap_divided_t_",i,sep="")
      print("adding regressor")
      print(regressor_name)
      main_model=add_regressor(main_model,regressor_name,standardize = F)
    }
    for(i in 1:lag_steps){
      regressor_name <- paste("Volume_divided_t_",i,sep="")
      print("adding regressor")
      print(regressor_name)
      main_model=add_regressor(main_model,regressor_name,standardize = F)
    }
    }else{
      for(i in 1:lag_steps){
        regressor_name <- paste("Market_Cap_t_",i,sep="")
        print("adding regressor")
        print(regressor_name)
        main_model=add_regressor(main_model,regressor_name,standardize = F)
      }
      for(i in 1:lag_steps){
        regressor_name <- paste("Volume_t_",i,sep="")
        print("adding regressor")
        print(regressor_name)
        main_model=add_regressor(main_model,regressor_name,standardize = F)
      }
    }
  }
  
  if(need_to_add_close_lagged){
    for(i in 1:lag_steps){
      regressor_name <- paste("Close_t_",i,sep="")
      print("adding regressor")
      print(regressor_name)
      main_model=add_regressor(main_model,regressor_name,standardize = F)
    }
  }
  
  main_model=fit.prophet(main_model,train_df_lagged)
  print("Made model")
  
  main_model_future_df=make_future_dataframe(main_model,periods = 248,freq = "day")
  
  if(need_to_add_litecoin_ethereum_lagged){
    for(i in 1:lag_steps){
      regressor_name <- paste("Litecoin_t_",i,sep="")
      print("filling df with regressor")
      print(regressor_name)
      main_model_future_df<- main_model_future_df %>%
        left_join(df_lagged[,c("ds",regressor_name)],by='ds')
    }
    
    for(i in 1:lag_steps){
      regressor_name <- paste("Ethereum_t_",i,sep="")
      print("filling df with regressor")
      print(regressor_name)
      main_model_future_df<- main_model_future_df %>%
        left_join(df_lagged[,c("ds",regressor_name)],by='ds')
    }
  }
  
  if(need_to_add_mc_volume_divided){
    if(perform_divide_mutation){
    for(i in 1:lag_steps){
      regressor_name <- paste("Market_Cap_divided_t_",i,sep="")
      print("filling df with regressor")
      print(regressor_name)
      main_model_future_df<- main_model_future_df %>%
        left_join(df_lagged[,c("ds",regressor_name)],by='ds')
    }
    
    for(i in 1:lag_steps){
      regressor_name <- paste("Volume_divided_t_",i,sep="")
      print("filling df with regressor")
      print(regressor_name)
      main_model_future_df<- main_model_future_df %>%
        left_join(df_lagged[,c("ds",regressor_name)],by='ds')
    }
    }else{
      for(i in 1:lag_steps){
        regressor_name <- paste("Market_Cap_t_",i,sep="")
        print("filling df with regressor")
        print(regressor_name)
        main_model_future_df<- main_model_future_df %>%
          left_join(df_lagged[,c("ds",regressor_name)],by='ds')
      }
      
      for(i in 1:lag_steps){
        regressor_name <- paste("Volume_t_",i,sep="")
        print("filling df with regressor")
        print(regressor_name)
        main_model_future_df<- main_model_future_df %>%
          left_join(df_lagged[,c("ds",regressor_name)],by='ds')
      }
    }
  }
  
  if(need_to_add_close_lagged){
    for(i in 1:lag_steps){
      regressor_name <- paste("Close_t_",i,sep="")
      print("filling df with regressor")
      print(regressor_name)
      main_model_future_df<- main_model_future_df %>%
        left_join(df_lagged[,c("ds",regressor_name)],by='ds')
    }
  }
  
  # View(main_model_future_df)
  
  main_model_forecast<-predict(main_model,main_model_future_df)
  
  print(dyplot.prophet(main_model,main_model_forecast,main=paste("Step",lag_steps)))
  
  
  #doing it for the entire dataframe for now
  # predicted_values <- main_model_forecast$yhat
  predicted_values<-main_model_forecast$yhat[(580-lag_steps):(827-lag_steps)]
  mape<-MAPE(predicted_values,df_lagged$Close[(580-lag_steps):(827-lag_steps)]) #0.7539401
  print(lag_steps)
  print(mape)
  
  df_to_plot <- df_lagged[(580-lag_steps):(827-lag_steps),]
  df_to_plot$Predicted <- predicted_values
  df_to_plot <- gather(df_to_plot,type,value,c("Close","Predicted"))
  print(ggplot(na.omit(df_to_plot),aes(x=ds,y=value))+
    geom_line(aes(color=type),size=0.15)+
    geom_point(aes(color=type),size=0.1)+
    ggtitle(paste("Plot for step",lag_steps))+
    theme_minimal())
    
  mape_metrics[mape_metrics$Step == lag_steps,"MAPE"]<-mape
  
}
print(mape_metrics)
