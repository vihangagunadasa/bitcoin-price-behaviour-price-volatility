library(neuralnet)
library(ggplot2)
library(tidyr)
library(MLmetrics)
library(readxl)
# train_df <- read_excel("svm training set.xlsx")
# colnames(train_df)[1] <- "Date"
# colnames(train_df)[4] <- "Market_Cap"
# test_df <- read_excel("svm testing set.xlsx")
# colnames(test_df)[4] <- "Market_Cap"

train_df <-read.csv("train_df_standardized.csv")
test_df <-read.csv("test_df_standardized.csv")
#fixing the date issue
train_df$Date <- as.Date(train_df$Date)
test_df$Date <- as.Date(test_df$Date)

#gonna drop variance from train and test
train_df <-subset(train_df,select=-Variance)
test_df <-subset(test_df,select=-Variance)

#combining train and test together
df <- rbind(train_df,test_df)

train_df_wo_date <- subset(train_df,select=-Date)
test_df_wo_date <- subset(test_df,select=-Date)
df_wo_date <- subset(df,select=-Date)

validation_indice <- floor(nrow(train_df_wo_date) * 0.85)
validation_df_wo_date <- train_df_wo_date[validation_indice:nrow(train_df),]
train_df_wo_validation <- train_df_wo_date[1:validation_indice,]

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
        select(sort(names(.)))
      # print(dataframe)
    }
  }
  dataframe<-na.omit(dataframe)
  dataframe
}
# for(lag_steps in 1:4){
lag_steps<- 3
train_df <-read.csv("train_df_standardized.csv")
test_df <-read.csv("test_df_standardized.csv")
#fixing the date issue
train_df$Date <- as.Date(train_df$Date)
test_df$Date <- as.Date(test_df$Date)

#gonna drop variance from train and test
train_df <-subset(train_df,select=-Variance)
test_df <-subset(test_df,select=-Variance)

#combining train and test together
df <- rbind(train_df,test_df)

train_df_wo_date <- subset(train_df,select=-Date)
test_df_wo_date <- subset(test_df,select=-Date)
df_wo_date <- subset(df,select=-Date)

validation_indice <- floor(nrow(train_df_wo_date) * 0.85)
validation_df_wo_date <- train_df_wo_date[validation_indice:nrow(train_df),]
train_df_wo_validation <- train_df_wo_date[1:validation_indice,]
# removing the data that is not standardized
train_df_wo_validation <- subset(lag_dataset(train_df_wo_validation,steps=lag_steps),
                                 select=-c(Volume,Market_Cap,Ethereum,Litecoin))
validation_df_wo_date <- subset(lag_dataset(validation_df_wo_date,steps=lag_steps),
                                select=-c(Volume,Market_Cap,Ethereum,Litecoin))
test_df_wo_date <- subset(lag_dataset(test_df_wo_date,steps=lag_steps),
                          select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin))
df_wo_date <- subset(lag_dataset(df_wo_date,steps=lag_steps),
                          select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin))
print("About to train model")
nn<-neuralnet(Close_stndrd~.,
              hidden=c(4,8,4),
              rep=20,
              data=subset(train_df_wo_validation,
                          select=-c(Volume_stndrd,Market_Cap_stndrd,
                                    Ethereum_stndrd,Litecoin_stndrd)), 
              act.fct='tanh',linear.output = TRUE, # err.fct = MAPE,
              learningrate = 0.001
              )
print(summary(nn))
print("Trained Model")
df_predicted <- predict(nn,
                        subset(df_wo_date,
                               select=-c(Volume_stndrd,Market_Cap_stndrd,
                                         Ethereum_stndrd,Litecoin_stndrd))
                        )
print("Predicted Values")
df<-df[(lag_steps+1):nrow(df),]
df$Predicted_stndrd <- df_predicted[,1]
df <- df %>% mutate(Predicted = (Predicted_stndrd * sd(Close)) + mean(Close))
df_long <- gather(df,type,value,c(Close,Predicted))
print(
  ggplot(df_long,aes(x=Date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  ggtitle(paste("Plot for step",lag_steps))
)
print(paste("Step number",lag_steps))
print(MAPE(df$Predicted,df$Close))
# prediction(nn)
# }
