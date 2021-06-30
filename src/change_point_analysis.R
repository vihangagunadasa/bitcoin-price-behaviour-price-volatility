##mention that from the first dataset, data points were removed after seeing the time series plot


library(fpp)
library(fma)
library(vars)
library(fUnitRoots)
library(TSA)

library(readxl)
Bitcoin <- read_excel("data/Bitcoin_dataframe.xlsx")
#View(Bitcoin)

attach(Bitcoin)
length(Close)

Close.ts=ts(Bitcoin$Close,frequency = 31)
dev.off()
plot(Close.ts)

library(stats)
plot(stl(Close.ts,s.window="periodic"),
     #main="Components of Bitcoin Close price"
     main="")
##827/30 = 28, thus, 28 months. monthly seasonality can be seen


trainset=Close.ts[1:579]
trainset 
testset=Close.ts[580:length(Close.ts)] #(30% of the dataset) #as mentioned in the research paper
testset

ts.plot(trainset)
ts.plot(testset)

testset_date=Bitcoin$Date[580:length(Bitcoin$Date)]

##**********************************************Changepoint analysis
library(changepoint)

#######***********************Variance changepoints

ts.plot(diff(trainset))
hist(diff(trainset))
shapiro.test(diff(trainset)) #Not-normal. this can happen since the variances change throughout the series. 

##**The change in variance approaches within the cpt.var function require the data to have a fixed value mean over time and thus this periodic mean must be removed prior to analysis. Whilst there are a range of options for removing this mean, we choose to take first diferences as this does not require any modeling assumptions. Following this we assume that the diferences follow a Normal distribution with changing variance and thus use the cpt.var function.



#To obtain the number of changepoints
vvalue10=cpt.var(data = diff(trainset), penalty = "CROPS",method = "PELT",pen.value = c(5,500))
cpts.full(vvalue10)
plot(vvalue10,diagnostic = TRUE,#main="Elbow Plot of Number of Changepoints",
     col="red",lwd=3) #consider 8 changepoints

#plot(vvalue10,ncpts=8) #even though the plot is produced, variances for each section are not provided, thus moved on to a manual penalty approach that gives the same number of changepoints at the same place.
#pen.value.full(vvalue10)
#param.est(vvalue10)$variance  ##NULL

BC_train=data.frame(as.Date(Bitcoin$Date[2:579]),diff(trainset))
colnames(BC_train)[1]="Date"
colnames(BC_train)[2]="Training_set_Close_price"

#variance changepoints using PELT
vvalue16 = cpt.var(data = diff(trainset), penalty = "Manual",pen.value="2.5*log(n)",test.stat = "Normal",method = "PELT")
summary(vvalue16)
varcpt.point=cpts(vvalue16) 
ncpts(vvalue16) ##Gives the same 8 changepoints as of CROPS
#plot(vvalue16)
(BC_train$Date)[varcpt.point]

ggplot(BC_train,aes(x=Date, y=Training_set_Close_price))+geom_line(col="blue") + geom_vline(xintercept = (BC_train$Date)[varcpt.point],
                                                                                           col="red",lwd=0.9,linetype="dotted")+
  #ggtitle("Changepoint of variance with PELT")+
  ggtitle("")+
  theme(plot.title = element_text(hjust=0.5,face = "bold"),axis.line = element_line(colour = "black"),
                                                     panel.grid.major = element_line(), 
                                                     panel.grid.minor = element_line(),
                                                     panel.background = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5))+scale_x_date(breaks = as.Date(c("2017-07-14","2017-11-05","2017-12-05","2018-01-21","2018-04-28","2018-09-08","2018-10-15","2018-11-13")))+xlab("Date")

param.est(vvalue16)$variance #to obtain the variances of the sections 



#variance changepoints using BinSeg (binary segmentation)
vvalue26 = cpt.var(data = diff(trainset), penalty = "Manual",pen.value="2.5*log(n)",test.stat = "Normal",Q=50,method = "BinSeg")
summary(vvalue26)
varcpt.point.binseg=cpts(vvalue26) 
ncpts(vvalue26) ##Gives 8 changepoints but 2 are different than of CROPS
(BC_train$Date)[varcpt.point.binseg]
#plot(vvalue26)

ggplot(BC_train,aes(x=Date, y=Training_set_Close_price))+geom_line(col="blue") + geom_vline(xintercept = (BC_train$Date)[varcpt.point.binseg],
                                                                                            col="red",lwd=0.9,linetype="dotted")+
  #ggtitle("Changepoint of variance with BinSeg")+
  ggtitle("")+
  theme(plot.title = element_text(hjust=0.5,face = "bold"),axis.line = element_line(colour = "black"),
                                                     panel.grid.major = element_line(), 
                                                     panel.grid.minor = element_line(),
                                                     panel.background = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5))+scale_x_date(breaks = as.Date(c("2017-07-14","2017-11-05","2017-12-05","2018-01-21","2018-03-14","2018-08-10","2018-10-15","2018-11-13")))+xlab("Date")


#####The PELT and binary segmentation search methods provides a comparison between exact and alternative algorithms


##***************************COMPARING NEGATIVE LOG LIKELIHOODS
logLik(vvalue16)[1]
logLik(vvalue26)[1]


#******
#As we are considering the negative log-likelihood the smaller value provided by PELT is preferred.

###Changepoints given by PELT under manual penalty is accepted.


