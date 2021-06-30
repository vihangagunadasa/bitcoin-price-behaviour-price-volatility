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

# lag_steps<-3
# # removing the data that is not standardized
# train_df_wo_validation <- lag_dataset(subset(train_df_wo_validation,
#                                              select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin)),steps=lag_steps)
# validation_df_wo_date <- lag_dataset(subset(validation_df_wo_date,
#                                             select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin)),steps=lag_steps)
# test_df_wo_date <- lag_dataset(subset(test_df_wo_date,
#                                       select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin)),steps=lag_steps)
nn<-neuralnet(Close_stndrd~.,
              hidden=c(4,8,4),rep=10,
              data=subset(train_df_wo_validation,
                          select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin)), 
              act.fct='tanh',linear.output = TRUE, # err.fct = MAPE,
              learningrate = 0.001)
# plot(nn)
# predicted<-predict(nn,
#                    subset(train_df,select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin))
#                    )
# train_df$Predicted <- predicted[,1]
# train_df_long<-gather(train_df,type,value,c(Close,Predicted))
# ggplot(train_df_long,aes(x=Date,y=value))+geom_line(aes(color=type),size=0.25)

df_predicted <- predict(nn,
                        subset(df,select=-c(Close,Volume,Market_Cap,Ethereum,Litecoin))
                        )
df$Predicted_stndrd <- df_predicted[,1]
df <- df %>% mutate(Predicted = (Predicted_stndrd * sd(Close)) + mean(Close))
df_long <- gather(df,type,value,c(Close,Predicted))
ggplot(df_long,aes(x=Date,y=value))+geom_line(aes(color=type),size=0.25)
MAPE(df$Predicted,df$Close)
# prediction(nn)

