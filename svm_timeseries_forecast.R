library(e1071)
library(ggplot2)
library(tidyr)
library(MLmetrics)
df <- read_excel("data2.xlsx")
View(df)
summary(df)
str(df)
# train an svm model, consider further tuning parameters for lower MSE
svmodel <- svm(close ~ .,data=df, type="eps-regression",kernel="radial",cost=10000, gamma=10)
df_long <- df
df_long$predicted <- predict(svmodel,subset(df,select=-close))
View(df)
MAPE(df_long$predicted,df_long$close)
#wide to long
df_long <- gather(df_long,type,value,c(close,predicted))
View(df_long)

#plotting ggplot
ggplot(df_long,aes(x=date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()
MAPE_tune<-function(y_true,y_pred){
  MAPE(y_pred = y_pred,y_true = y_true)
}
svm_tune <- tune.svm(close ~ ., data=df,type="eps-regression",
                 kernel="radial",gamma = 2^(-9:10), cost = 2^(-9:10),tunecontrol=tune.control(error.fun = MAPE_tune))
svm_tune$best.parameter
svmodel_best <- svm(close ~ .,data=df, type="eps-regression",kernel="radial",cost=svm_tune$best.parameter$cost, 
                    gamma=svm_tune$best.parameter$gamma)
df_long_best<-df
df_long_best$predicted <- predict(svmodel_best,subset(df,select=-close))
MAPE(df_long_best$predicted,df_long_best$close)
df_long_best <- gather(df_long_best,type,value,c(close,predicted))
#plotting ggplot
ggplot(df_long_best,aes(x=date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()