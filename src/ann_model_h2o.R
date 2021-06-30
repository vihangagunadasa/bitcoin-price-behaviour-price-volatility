library(h2o)
library(e1071)
library(ggplot2)
library(tidyr)
library(MLmetrics)
library(readxl)
source("lag_dataset.R")

h2o.init(nthreads = -1,max_mem_size = '2G')
h2o.removeAll()

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
need_to_add_close_lagged <- FALSE
need_to_add_litecoin_ethereum_lagged<-TRUE
need_grid_search <- TRUE

train_df <-subset(train_df,select=-Variance)
test_df <-subset(test_df,select=-Variance)

start_lag_step <- 1
end_lag_step <- 8

df <- rbind(train_df,test_df)
df <- df %>% standardize_mutation() %>% divide_mutation()

mape_metrics <- data.frame(
  Step = seq(start_lag_step,end_lag_step),
  MAPE = seq(start_lag_step,end_lag_step),
  MAPE_best = seq(start_lag_step,end_lag_step)
)

for (lag_steps in start_lag_step:end_lag_step){
  
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
  
  model_name <- paste("dl_model_step_",lag_steps,sep="")
  model <- h2o.deeplearning(
    model_id=model_name, 
    training_frame=as.h2o(train_df_wo_validation), 
    validation_frame=as.h2o(validation_df),   ## validation dataset: used for scoring and early stopping
    x=predictors,
    y=response,
    hidden=hidden,          ## more hidden layers -> more complex interactions
    epochs=epochs,
    standardize = TRUE,
    verbose = TRUE,
    activation="Tanh",  ## default
    variable_importances=T,## not enabled by default
    score_each_iteration = T,
    loss="Absolute",
    reproducible = T,
    # distribution = "Laplace",
    stopping_rounds = stopping_rounds,
    seed = 2020,
    score_training_samples = 0
  )
  
  summary(model)
  plot(model)
  hyper_params <- list(
    activation=c("Tanh","Rectifier"),
    hidden=list(c(20,20),c(50,10),c(10),c(25,25,25,25)),
    epochs=seq(100,900,100),
    loss=c("Absolute","Quadratic"),
    distribution=c("Laplace","Gaussian")
  )
  search_criteria = list(strategy = "RandomDiscrete", 
                         max_runtime_secs = 120, 
                         max_models = 20,
                         seed=1234567, 
                         stopping_rounds=5, 
                         stopping_tolerance=1e-3)
  best_model <- NULL
  
  if(need_grid_search){
    print("Doing grid search")
    grid <- h2o.grid(
      algorithm="deeplearning",
      grid_id=paste("dl_grid",lag_steps), 
      training_frame=as.h2o(train_df_wo_validation),
      validation_frame=as.h2o(validation_df), 
      x=predictors, 
      y=response,
      # epochs=100,
      stopping_metric="mse",
      stopping_tolerance=1e-3,        ## stop when misclassification does not improve by >=0.1% for 2 scoring events
      stopping_rounds=5,
      score_validation_samples=0, ## downsample validation set for faster scoring
      score_duty_cycle=0.025,         ## don't score more than 2.5% of the wall time
      adaptive_rate=F,                ## manually tuned learning rate
      standardize=T,
      momentum_start=0.5,             ## manually tuned momentum
      momentum_stable=0.9,
      momentum_ramp=1e7,
      l1=1e-5,
      l2=1e-5,
      # activation="Tanh",
      max_w2=10,                      ## can help improve stability for Rectifier
      hyper_params=hyper_params,
      search_criteria=search_criteria
    )
    print("sorting errors")
    grid <- h2o.getGrid(paste("dl_grid",lag_steps),sort_by="mse",decreasing=FALSE)
    print(grid@summary_table[1,])
    print("Found best model")
    best_model <<- h2o.getModel(grid@model_ids[[1]])
  }
  print("predicting from base and best models")
  
  print("collecting results")
  df_long_best <- df_lagged[(580-lag_steps):(827-lag_steps),] %>% subset(select= predictors)
  # df_long_best <- df_lagged %>% subset(select=predictors)
  
  ann_predicted <- h2o.predict(model,as.h2o(df_long_best))
  ann_predicted_best <- h2o.predict(best_model,as.h2o(df_long_best))
  
  df_long_best$Actual <- df_lagged$Close[(580-lag_steps):(827-lag_steps)]
  # df_long_best$Actual <- df_lagged$Close
  df_long_best$Predicted <- (as.vector(ann_predicted) * sd(df$Close)) + mean(df$Close)
  
  if(need_grid_search){
    df_long_best$Predicted_best <- (as.vector(ann_predicted_best) * sd(df$Close)) + mean(df$Close)
  }else{
    df_long_best$Predicted_best <- df_long_best$Predicted
  }
  df_long_best$Date <- df_lagged$Date[(580-lag_steps):(827-lag_steps)]
  # df_long_best$Date <- df_lagged$Date
  
  print("calculating MAPEs")
  mape <- MAPE(df_long_best$Predicted,df_long_best$Actual)
  if(need_grid_search){
    mape_best <- MAPE(df_long_best$Predicted_best,df_long_best$Actual)
  }else{
    mape_best <- mape
  }
  
  print(lag_steps)
  print(mape)
  print(mape_best)
  
  mape_metrics[mape_metrics$Step == lag_steps,"MAPE"]<-mape
  mape_metrics[mape_metrics$Step == lag_steps,"MAPE_best"]<-mape_best
  
  if(need_grid_search){
    df_long_best <- gather(df_long_best,type,value,c(Actual,Predicted,Predicted_best))
  }else{
    df_long_best <- gather(df_long_best,type,value,c(Actual,Predicted))
  }
  #plotting ggplot
  print(ggplot(df_long_best,aes(x=Date,y=value))+
          geom_line(aes(color=type),size=0.25)+
          geom_point(aes(color=type),size=0.15)+
          ggtitle(paste("Predicted_step",lag_steps,sep="_"))+
          theme_minimal())
  
  
}

print(mape_metrics)

