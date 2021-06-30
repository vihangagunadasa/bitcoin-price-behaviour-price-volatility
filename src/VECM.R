library(readxl)
var_training_set <- read_excel("data/vecm_training set.xlsx")
View(var_training_set)

var_training_set$Volume=var_training_set$Volume/(10^7)
var_training_set$Market_Cap=var_training_set$Market_Cap/(10^8)
View(var_training_set)

ts.plot(var_training_set$Close)
ts.plot(var_training_set$Volume)
ts.plot(var_training_set$Market_Cap)
ts.plot(var_training_set$Variance)
ts.plot(var_training_set$Ethereum)
ts.plot(var_training_set$Litecoin)

library(fpp)

kpss.test(var_training_set$Close)
kpss.test(var_training_set$Volume)
kpss.test(var_training_set$Market_Cap)
kpss.test(var_training_set$Variance)
kpss.test(var_training_set$Ethereum)
kpss.test(var_training_set$Litecoin)

##all are not stationary, (p-value=0.01)

##stationarity of first differenced series

kpss.test(diff(var_training_set$Close,1))
kpss.test(diff(var_training_set$Volume,1))
kpss.test(diff(var_training_set$Market_Cap,1))
kpss.test(diff(var_training_set$Variance,1))
kpss.test(diff(var_training_set$Ethereum,1))
kpss.test(diff(var_training_set$Litecoin,1))


var_training_set3=data.frame(var_training_set
                             $Close,var_training_set$Variance,var_training_set$Litecoin)


var_training_set3

library(vars)
VARselect(var_training_set3,lag.max = 8,type = "const")$selection


library(urca)
johtest7=ca.jo(var_training_set3,type = "trace",K = 2)
summary(johtest7) #if test>0.5, reject H0
##There is 1 cointegrating relationship

##check for serial correlation 
varmodel7=vec2var(johtest7,r = 1)
serial.test(varmodel7) #assumption of serial correlation is violated

##Fitting a VEC model
VECmodel7=cajorls(johtest7,r=1,reg.number = NULL)
VECmodel7


summary(VECmodel7$rlm)



library(fma)
varmodel7_forecast=predict(varmodel7,n.ahead = 248)


library(MLmetrics)
VEC_predicted=varmodel7_forecast$fcst$var_training_set.Close[,1]

Bitcoin <- read_excel("data/Bitcoin_dataframe.xlsx")
Close.ts=ts(Bitcoin$Close,frequency = 31)
testset=Close.ts[580:length(Close.ts)] #(30% of the dataset) #as mentioned in the research paper

vec_df=data.frame(Bitcoin$Date[580:827],VEC_predicted,testset)
colnames(vec_df)[1]="Date"
colnames(vec_df)[2]="Predicted"
colnames(vec_df)[3]="Actual"


library(ggplot2)
ggplot(vec_df,aes(x=Date))+geom_line(aes(y=Predicted,colour="Predicted"))+
  geom_line(aes(y=Actual,colour="Actual"))+labs(colour="Series")+
  #ggtitle(paste("Plot of Forecasts - Vector Error Correction Model"))+
  theme(#plot.title = element_text(hjust=0.5,face = "bold",size=12),
    axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())


mape_VEC=MAPE(VEC_predicted,testset)  ##0.2618906
mape_VEC
