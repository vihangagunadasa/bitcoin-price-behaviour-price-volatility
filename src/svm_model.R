library(e1071)
library(ggplot2)
library(dplyr)
library(tidyr)
library(MLmetrics)
library(readxl)
source("lag_dataset.R")

train_df <- read_excel("svm training set.xlsx")
test_df <- read_excel("svm testing set.xlsx")
colnames(train_df)[1] <- "Date"
colnames(train_df)[4] <- "Market_Cap"
colnames(test_df)[4] <- "Market_Cap"

volume_divisor<- 10^7
mc_divisor<-10^8

divide_mutation <- function(df){
  mutate(df,
         Volume_divided=Volume/volume_divisor,
         Market_Cap_divided=Market_Cap/mc_divisor)
}

need_to_add_mc_volume_divided<-FALSE
need_to_add_close_lagged <- TRUE
need_to_add_litecoin_ethereum_lagged<-TRUE

train_df <-subset(train_df,select=-Variance)
test_df <-subset(test_df,select=-Variance)
df <- rbind(train_df,test_df)


# train_df <- train_df %>% divide_mutation()
# test_df <- test_df %>% divide_mutation()
df <- df %>% divide_mutation()
df <- subset(df,select=-c(Market_Cap,Volume))
# train_df_wo_date <- subset(train_df,select=-Date)
# test_df_wo_date <- subset(test_df,select=-Date)
# df_wo_date <- subset(df,select=-Date)

# validation_indice <- floor(nrow(train_df) * 0.85)
# train_df_wo_validation <- train_df_wo_date[1:validation_indice,]
# validation_df_wo_date <- train_df_wo_date[(validation_indice+1):nrow(train_df),]

start_lag_step <- 1
end_lag_step <- 8

mape_metrics <- data.frame(
  Step = seq(start_lag_step,end_lag_step),
  MAPE = seq(start_lag_step,end_lag_step)
)

step_series_df <- data.frame(
  Date = test_df$Date,
  Close = test_df$Close
)

for(lag_steps in start_lag_step:end_lag_step){
  
  df_wo_date <- subset(df,select=-Date)
  
  
  if(need_to_add_close_lagged){
    if(need_to_add_litecoin_ethereum_lagged){
      if(need_to_add_mc_volume_divided){
        
        print("#add close lagegd 
        #add litecoin ethereum
        #add mc volume")
        df_wo_date <- lag_dataset(df_wo_date,steps=lag_steps)
      }else{
        
        print("#add close lagegd 
        #add litecoin ethereum
        #do not add mc volume")
        df_wo_date <- lag_dataset(subset(df_wo_date,
                                         select=-c(Market_Cap_divided,Volume_divided)),
                                  steps=lag_steps)
      }
    }else{
      if(need_to_add_mc_volume_divided){
        
        print(" #add close lagegd 
        #do not add litecoin ethereum
        #add mc volume")
        df_wo_date <- lag_dataset(subset(df_wo_date,
                                         select=-c(Litecoin,Ethereum)),
                                  steps=lag_steps)
      }else{
       
        print("#add close lagegd 
        #do not add litecoin ethereum
        #do not add mc volume")
        df_wo_date <- lag_dataset(subset(df_wo_date,
                                         select=-c(Litecoin,Ethereum,
                                                   Market_Cap_divided,Volume_divided)),
                                  steps=lag_steps)
      }
    }
  }else{
    if(need_to_add_litecoin_ethereum_lagged){
      if(need_to_add_mc_volume_divided){
        
        print(" #do not add close lagegd 
        #add litecoin ethereum
        #add mc volume")
        df_wo_date <- lag_dataset(subset(df_wo_date,
                                         select=-c(Close)),
                                  steps=lag_steps)
        df_wo_date$Close <- df$Close[(lag_steps+1):nrow(df)]
      }else{
        
        print("#do not add close lagegd 
        #add litecoin ethereum
        #do not add mc volume")
        df_wo_date <- lag_dataset(subset(df_wo_date,
                                         select=-c(Close,
                                                   Market_Cap_divided,Volume_divided)),
                                  steps=lag_steps)
        df_wo_date$Close <- df$Close[(lag_steps+1):nrow(df)]
      }
    }else{
      if(need_to_add_mc_volume_divided){
        
        print("#do not add close lagegd 
        #do not add litecoin ethereum
        #add mc volume")
        df_wo_date <- lag_dataset(subset(df_wo_date,
                                         select=-c(Close,
                                                   Litecoin,Ethereum)),
                                  steps=lag_steps)
        df_wo_date$Close <- df$Close[(lag_steps+1):nrow(df)]
      }else{
        
        print("#do not add close lagegd 
        #do not add litecoin ethereum
        #do not add mc volume")
        df_wo_date <- lag_dataset(subset(df_wo_date,
                                         select=-c(Close,
                                                   Litecoin,Ethereum,
                                                   Market_Cap_divided,Volume_divided)),
                                  steps=lag_steps)
        df_wo_date$Close <- df$Close[(lag_steps+1):nrow(df)]
      }
    }
  }
  
  print(setdiff(colnames(df_wo_date),"Close"))
  
  testing_indice = (579 - lag_steps)
  train_df_wo_date = df_wo_date[1:testing_indice,]
  test_df_wo_date = df_wo_date[(testing_indice+1):nrow(df_wo_date),]
  
  validation_indice <- floor(nrow(train_df_wo_date) * 0.85)
  validation_df_wo_date <- train_df_wo_date[(validation_indice+1):nrow(train_df_wo_date),]
  train_df_wo_validation <- train_df_wo_date[1:validation_indice,]
  
  print(colnames(train_df_wo_validation))
  
  svm_tune <- tune.svm(Close ~ ., 
                       data=train_df_wo_validation,
                       type="eps-regression",
                       kernel="radial",
                       gamma = 2^(-9:10), cost = 2^(-9:10),
                       validation.x = subset(validation_df_wo_date,select=-Close),
                       validation.y = validation_df_wo_date$Close,
                       tunecontrol=tune.control(sampling="fix",error.fun = MAPE))
  print(svm_tune$best.parameter)
  
  svmodel_best <- svm(Close ~ .,
                      data=train_df_wo_date, 
                      type="eps-regression",kernel="radial",
                      cost=svm_tune$best.parameter$cost, 
                      gamma=svm_tune$best.parameter$gamma)
  
  #test_df_wo_date is already lagged but we need the date column
  #hence we use test df 
  df_long_best <- df_wo_date[(580-lag_steps):(827-lag_steps),]
  df_long_best$Date <- df$Date[(580-lag_steps):(827-lag_steps)]
  df_long_best$predicted <- predict(svmodel_best,
                                    subset(df_wo_date[(580-lag_steps):(827-lag_steps),],
                                           select=-Close))
  
  print(lag_steps)
  print(paste("Tuned result for step:",lag_steps," "))
  mape<-MAPE(df_long_best$predicted,df_long_best$Close)
  print(mape)
  
  mape_metrics[mape_metrics$Step == lag_steps,"MAPE"]<-mape
  
  series_df <- data.frame(
    Date=df_long_best$Date,
    Predicted=df_long_best$predicted
  )
  colnames(series_df)[2] <- paste("Predicted_step",lag_steps,sep="_")
  step_series_df <<- left_join(step_series_df,series_df,by=c("Date"))
  
  df_long_best <- gather(df_long_best,type,value,c(Close,predicted))
  #plotting ggplot
  print(ggplot(df_long_best,aes(x=Date,y=value))+
          geom_line(aes(color=type),size=0.25)+
          scale_color_manual(values = c("#00AFBB", "#E7B800")) +
          ggtitle(paste("Predicted_step",lag_steps,sep="_"))+
          theme_minimal())
}

print(mape_metrics)

step_series_long_df <- gather(step_series_df,type,value,
                              colnames(step_series_df)[2:length(colnames(step_series_df))])

ggplot(na.omit(step_series_long_df),aes(x=Date,y=value))+
  geom_line(aes(color=type),size=0.15)+
  geom_point(aes(color=type),size=0.1)+
  theme_minimal()
