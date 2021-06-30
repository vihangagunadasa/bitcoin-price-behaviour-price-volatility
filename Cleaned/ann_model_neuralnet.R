library(neuralnet)
library(e1071)
library(ggplot2)
library(tidyr)
library(dplyr)
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

standardize_mutation<-function(df){
  df %>% mutate(Market_Cap_stndrd=(Market_Cap-mean(Market_Cap))/sd(Market_Cap),
                Close_stndrd=(Close-mean(Close))/sd(Close),
                Litecoin_stndrd=(Litecoin-mean(Litecoin))/sd(Litecoin),
                Ethereum_stndrd=(Ethereum-mean(Ethereum))/sd(Ethereum),
                Volume_stndrd=(Volume-mean(Volume))/sd(Volume)
         )
}

divide_mutation<-function(df){
  df %>% mutate(Volume_divided=Volume/volume_divisor,
                Market_Cap_divided=Market_Cap/mc_divisor)
}

need_to_add_mc_volume_divided<-TRUE
need_to_add_close_lagged <- TRUE
need_to_add_litecoin_ethereum_lagged<-TRUE

train_df <-subset(train_df,select=-Variance)
test_df <-subset(test_df,select=-Variance)

start_lag_step <- 1
end_lag_step <- 8

df <- rbind(train_df,test_df)
df <- df %>% standardize_mutation() %>% divide_mutation()

mape_metrics <- data.frame(
  Step = seq(start_lag_step,end_lag_step),
  MAPE = seq(start_lag_step,end_lag_step)
)

for (lag_steps in start_lag_step:end_lag_step){
  
  print("start lag steps")
  print(lag_steps)
  df_lagged <- lag_dataset(df,steps = lag_steps)
  
  testing_indice = (579 - lag_steps)
  train_df_lagged = df_lagged[1:testing_indice,]
  test_df_lagged = df_lagged[(testing_indice+1):nrow(df_lagged),]
  
  validation_indice <- floor(nrow(train_df_lagged) * 0.85)
  validation_df<- train_df_lagged[(validation_indice+1):nrow(train_df_lagged),]
  train_df_wo_validation <- train_df_lagged[1:validation_indice,]
  
  response <- "Close_stndrd"
  predictors <- c()
  if(need_to_add_close_lagged){
    for(i in 1:lag_steps){
      regressor_name <- paste("Close_stndrd_t_",i,sep="")
      predictors <- append(predictors,regressor_name)  
    }
  }
  
  if(need_to_add_litecoin_ethereum_lagged){
    for(i in 1:lag_steps){
      regressor_name <- paste("Litecoin_stndrd_t_",i,sep="")
      predictors <- append(predictors,regressor_name)
    }
    for(i in 1:lag_steps){
      regressor_name <- paste("Ethereum_stndrd_t_",i,sep="")
      predictors <- append(predictors,regressor_name)
    }
  }
  
  if(need_to_add_mc_volume_divided){
    for(i in 1:lag_steps){
      regressor_name <- paste("Market_Cap_divided_t_",i,sep="")
      predictors <- append(predictors,regressor_name)
    }
    for(i in 1:lag_steps){
      regressor_name <- paste("Volume_divided_t_",i,sep="")
      predictors <- append(predictors,regressor_name)
    }
  }
  print("predictors")
  print(predictors)
  print("train df")
  train_df_wo_validation <- train_df_wo_validation %>% subset(select= append(predictors,response))
  print(colnames(train_df_wo_validation))
  print("validation df")
  validation_df <- validation_df %>% subset(select= append(predictors,response))
  print(colnames(validation_df))
  
  if(!need_to_add_close_lagged && !need_to_add_litecoin_ethereum_lagged && !need_to_add_mc_volume_divided){
    print("ERROR")
    break
  }
  
  #configuration
  epochs <- 100
  hidden <- c(20,40)
  stopping_rounds <- 0
  standardize <- TRUE
  loss <- "Absolute"
  
  print("building base model")
  
 
  ##TODO Add Base model
  nn<-neuralnet(Close_stndrd~.,
                rep=20,
                hidden=10,
                data=train_df_wo_validation,
                act.fct='tanh',
                linear.output = TRUE, # err.fct = MAPE,
                algorithm = "backprop",
                learningrate = 0.0001,
                thresh = 0.1,
                lifesign="full"
  )
  print(summary(nn))
  
  
  
  print("predicting from base model")
  
  print("collecting results")
  df_long_best <- df_lagged[(580-lag_steps):(827-lag_steps),] %>% subset(select= predictors)
  
  ann_predicted <- predict(nn,df_long_best) #TODO Add predictions here# h2o.predict(model,as.h2o(df_long_best))
  
  df_long_best$Actual <- df_lagged$Close[(580-lag_steps):(827-lag_steps)]
  # df_long_best$Actual <- df_lagged$Close
  df_long_best$Predicted <- (as.vector(ann_predicted) * sd(df$Close)) + mean(df$Close)
  
  df_long_best$Date <- df_lagged$Date[(580-lag_steps):(827-lag_steps)]
  # df_long_best$Date <- df_lagged$Date
  
  print("calculating MAPEs")
  mape <- MAPE(df_long_best$Predicted,df_long_best$Actual)
  
  print(lag_steps)
  print(mape)
  
  mape_metrics[mape_metrics$Step == lag_steps,"MAPE"]<-mape
  
  
  df_long_best <- gather(df_long_best,type,value,c(Actual,Predicted))
  
  #plotting ggplot
  print(ggplot(df_long_best,aes(x=Date,y=value))+
          geom_line(aes(color=type),size=0.25)+
          geom_point(aes(color=type),size=0.15)+
          ggtitle(paste("Predicted_step",lag_steps,sep="_"))+
          theme_minimal())
  
  
}

print(mape_metrics)

