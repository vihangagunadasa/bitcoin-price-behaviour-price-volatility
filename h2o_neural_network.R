library(h2o)
library(e1071)
library(ggplot2)
library(tidyr)
library(MLmetrics)
library(readxl)
h2o.init(nthreads = -1,max_mem_size = '2G')
h2o.removeAll()

train_df <-read.csv("train_df_standardized.csv")
test_df <-read.csv("test_df_standardized.csv")
colnames(train_df)[1] <- "Date"
colnames(train_df)[4] <- "Market_Cap"
colnames(test_df)[4] <- "Market_Cap"
#fixing the date issue
train_df <-subset(train_df,select=-Variance)
test_df <-subset(test_df,select=-Variance)
train_df$Date <- as.Date(train_df$Date)
test_df$Date <- as.Date(test_df$Date)


df <- rbind(train_df,test_df)
train_df_wo_date <- subset(train_df,select=-Date)
test_df_wo_date <- subset(test_df,select=-Date)
df_wo_date <- subset(df,select=-Date)

validation_indice <- floor(nrow(train_df) * 0.85)
validation_df_wo_date <- train_df_wo_date[validation_indice:nrow(train_df),]
train_df_wo_validation <- train_df_wo_date[1:validation_indice,]

lag_dataset<-function(dataframe,steps){
  library(tis)
  library(dplyr)
  for(col in colnames(dataframe)){
    for(i in 1:steps){
      column_name <- paste(col,i,sep="_t-")
      # print(class(dplyr::pull(dataframe,col)))
      dataframe <- dataframe %>% mutate(
        # !!column_name := list(dataframe[,col])
        !!column_name := lag(dplyr::pull(dataframe,col),i)
        # !!column_name := lag(dataframe[,col],i)
      ) %>%
        select(sort(names(.)))
      # print(dataframe)
    }
  }
  na.omit(dataframe)
  # dataframe
}

step = 6
#32 64
#step1 1.14
#step2 1.089
#step3 1.06
#step4 1.04
#step5 0.917
#step6 0.88
#step7 0.95
#step8 1.092


lag_steps = step

# train_df_wo_validation <- subset(lag_dataset(train_df_wo_validation,steps=lag_steps),
#                                  select=-c(Volume,Market_Cap,Ethereum,Litecoin))
# validation_df_wo_date <- subset(lag_dataset(validation_df_wo_date,steps=lag_steps),
#                                 select=-c(Volume,Market_Cap,Ethereum,Litecoin))
# train_df_wo_date <- rbind(train_df_wo_validation,validation_df_wo_date)
# test_df_wo_date <- subset(lag_dataset(test_df_wo_date,steps=lag_steps),
#                           select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin))
# df_wo_date <- subset(lag_dataset(df_wo_date,steps=lag_steps),
#                      select=-c(Volume,Market_Cap,Ethereum,Litecoin))
train_df_wo_validation <- subset(train_df_wo_validation,
                                 select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin))
train_df_wo_validation <- subset(lag_dataset(train_df_wo_validation,steps=lag_steps),
                                 select=-c(Volume_stndrd,Market_Cap_stndrd,
                                           Ethereum_stndrd,Litecoin_stndrd))
validation_df_wo_date <- subset(validation_df_wo_date,
                                select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin))
validation_df_wo_date <- subset(lag_dataset(validation_df_wo_date,steps=lag_steps),
                                select=-c(Volume_stndrd,Market_Cap_stndrd,
                                          Ethereum_stndrd,Litecoin_stndrd))

train_df_wo_date <- rbind(train_df_wo_validation,validation_df_wo_date)

test_df_wo_date <- subset(test_df_wo_date,
                          select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin))
actual_test_values <- test_df_wo_date$Close_stndrd
test_df_wo_date <- subset(lag_dataset(test_df_wo_date,steps=lag_steps),
                          select=-c(Close_stndrd,
                                    Volume_stndrd,Market_Cap_stndrd,
                                    Ethereum_stndrd,Litecoin_stndrd))

df_wo_date <- subset(lag_dataset(df_wo_date,steps=lag_steps),
                     select=-c(Volume_stndrd,Market_Cap_stndrd,
                               Ethereum_stndrd,Litecoin_stndrd))

# splits <- h2o.splitFrame(as.h2o(df_wo_date), c(0.6,0.2), seed=1234)
# train  <- h2o.assign(splits[[1]], "train.hex") # 60%
# valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%
# test   <- h2o.assign(splits[[3]], "test.hex")  # 20%

response <- "Close_stndrd"
predictors <- setdiff(names(train_df_wo_date), response)
predictors

m1 <- h2o.deeplearning(
  model_id="dl_timeseries_model", 
  training_frame=as.h2o(train_df_wo_validation), 
  validation_frame=as.h2o(validation_df_wo_date),   ## validation dataset: used for scoring and early stopping
  x=predictors,
  y=response,
  hidden=c(4,8,4),          ## more hidden layers -> more complex interactions
  epochs=10,
  standardize = TRUE,
  verbose = TRUE,
  activation="Tanh",  ## default
  variable_importances=T,## not enabled by default
  score_each_iteration = T,
  loss="Absolute",
  reproducible = T,
  distribution = "Laplace",
  stopping_rounds = 0,
  seed = 2020,
  score_training_samples = 0
)
summary(m1)
plot(m1)

h2o.performance(m1,train = T)
h2o.performance(m1,valid = T)
h2o.performance(m1,newdata = as.h2o(dplyr::mutate(test_df_wo_date,
                                                  Close_stndrd=actual_test_values[(step+1):length(actual_test_values)])))

ann_predicted <- h2o.predict(m1,as.h2o(test_df_wo_date))
ann_predicted$predict
MAPE(ann_predicted$predict,actual_test_values[(step+1):nrow(test_df)])

df_to_plot <- data.frame(
  "Date" = test_df$Date[(step+1):nrow(test_df)],
  "Actual" = actual_test_values[(step+1):nrow(test_df)],
  "Predicted" = as.data.frame(ann_predicted$predict)[,1]
)
df_to_plot <- gather(df_to_plot,type,value,c("Actual","Predicted"))
ggplot(data = df_to_plot,aes(x = Date,y=value))+geom_line(aes(color=type),size=0.25)+theme_minimal()
