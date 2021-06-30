library(e1071)
library(ggplot2)
library(tidyr)
library(MLmetrics)
library(readxl)
train_df <- read_excel("svm training set.xlsx")
colnames(train_df)[1] <- "Date"
test_df <- read_excel("svm testing set.xlsx")
#gonna drop variance from train and test
#lol guess not
ggplot(train_df,aes(x=Date,y=Variance))+
  geom_line()
train_df <-subset(train_df,select=-Variance)
test_df <-subset(test_df,select=-Variance)


View(train_df)
View(test_df)
df <- rbind(train_df,test_df)
train_df_wo_date <- subset(train_df,select=-Date)
test_df_wo_date <- subset(test_df,select=-Date)
df_wo_date <- subset(df,select=-Date)
svmodel <- svm(Close ~ .,data=train_df_wo_date, type="eps-regression",kernel="radial",cost=10000, gamma=10)
df_long <- test_df
df_long$predicted <- predict(svmodel,subset(test_df_wo_date,select=-Close))
MAPE(df_long$predicted,df_long$Close)
df_long <- gather(df_long,type,value,c(Close,predicted))
ggplot(df_long,aes(x=Date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

MAPE_tune<-function(y_true,y_pred){
  MAPE(y_pred = y_pred,y_true = y_true)
}
validation_indice <- floor(nrow(train_df) * 0.85)
validation_df <- train_df_wo_date[validation_indice:nrow(train_df),]
validation_df_wo_date <- validation_df
train_df_wo_validation <- train_df_wo_date[1:validation_indice,]
svm_tune <- tune.svm(Close ~ ., data=train_df_wo_validation,type="eps-regression",
                     kernel="radial",gamma = 2^(-9:10), cost = 2^(-9:10),
                     validation.x = subset(validation_df_wo_date,select=-Close),
                     validation.y = validation_df_wo_date$Close,
                     tunecontrol=tune.control(sampling="fix",error.fun = MAPE))
svm_tune$best.parameter
svmodel_best <- svm(Close ~ .,data=train_df_wo_validation, 
                    type="eps-regression",kernel="radial",
                    cost=svm_tune$best.parameter$cost, 
                    gamma=svm_tune$best.parameter$gamma)
df_long_best<-test_df
df_long_best$predicted <- predict(svmodel_best,subset(test_df_wo_date,select=-Close))
MAPE(df_long_best$predicted,df_long_best$Close)
df_long_best <- gather(df_long_best,type,value,c(Close,predicted))
#plotting ggplot
ggplot(df_long_best,aes(x=Date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
