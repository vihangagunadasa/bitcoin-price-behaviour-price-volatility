library(e1071)
library(ggplot2)
library(tidyr)
library(MLmetrics)
library(readxl)
train_df <- read_excel("svm training set.xlsx")
colnames(train_df)[1] <- "Date"
test_df <- read_excel("svm testing set.xlsx")
df<-rbind(train_df,test_df)
validation_df<-train_df[floor(nrow(train_df)*0.85):nrow(train_df),]
train_df_wo_validation<-train_df[1:floor(nrow(train_df)*0.85),]

#base model
Litecoin_svmodel <- svm(Litecoin ~ .,data=subset(train_df_wo_validation,select=c(Litecoin,Date)), 
                        type="eps-regression",kernel="radial",cost=10000, gamma=10)

Litecoin_predicted <- predict(Litecoin_svmodel,subset(df,select=c(Litecoin,Date)))
Litecoin_df <- subset(df,select=c(Litecoin,Date))
Litecoin_df$Predicted <- Litecoin_predicted
MAPE(Litecoin_df$Predicted,Litecoin_df$Litecoin)
Litecoin_df_long<-gather(Litecoin_df,type,value,c(Litecoin,Predicted))
ggplot(Litecoin_df_long,aes(x=Date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

#tune 
svm_tune <- tune.svm(Litecoin ~ ., data=subset(train_df_wo_validation,select=c(Litecoin,Date)),
                     type="eps-regression",
                     kernel="radial",gamma = 2^(-9:10), cost = 2^(-9:10),
                     validation.x = subset(validation_df,select=c(Litecoin,Date)),
                     validation.y = validation_df$Litecoin,
                     tunecontrol=tune.control(sampling="fix",error.fun = MAPE))
svm_tune$best.parameter
Litecoin_svmodel_best <- svm(Litecoin ~ .,data=subset(df,select=c(Litecoin,Date)), 
                    type="eps-regression",kernel="radial",cost=svm_tune$best.parameter$cost, 
                    gamma=svm_tune$best.parameter$gamma)
Litecoin_df <- subset(df,select=c(Litecoin,Date))
Litecoin_predicted <- predict(Litecoin_svmodel_best,Litecoin_df)
Litecoin_df$Predicted <- Litecoin_predicted
MAPE(Litecoin_df$Predicted,Litecoin_df$Litecoin)
Litecoin_df_long <- gather(Litecoin_df,type,value,c(Litecoin,Predicted))

ggplot(Litecoin_df_long,aes(x=Date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
