#1. cross correlation among coins
#2. cross correlation among variables
#3. variables selection

#1. Cross-correlation

library(readxl)
##Full dataset
Bitcoin <- read_excel("data/Bitcoin_dataframe.xlsx")
#View(Bitcoin)

Ethereum <- read_excel("data/raw_data/Ethereum.xlsx")
#View(Ethereum)

Litecoin <- read_excel("data/raw_data/Litecoin.xlsx")
#View(Litecoin)

Ripple <- read_excel("data/raw_data/Ripple.xlsx")
#View(Ripple)

Tether <- read_excel("data/raw_data/Tether.xlsx")
#View(Tether)

BC=data.frame(Bitcoin$Date,Bitcoin$Close)
colnames(BC)[1]="Date"
colnames(BC)[2]="Close_price"

ETH = data.frame(Ethereum$Date,Ethereum$Close)
colnames(ETH)[1]="Date"
colnames(ETH)[2]="Close_price"

LTC=data.frame(Litecoin$Date,Litecoin$Close)
colnames(LTC)[1]="Date"
colnames(LTC)[2]="Close_price"

RLE=data.frame(Ripple$Date,Ripple$Close)
colnames(RLE)[1]="Date"
colnames(RLE)[2]="Close_price"

THR=data.frame(Tether$Date,Tether$Close)
colnames(THR)[1]="Date"
colnames(THR)[2]="Close_price"



library(ggplot2)
ggplot(BC,aes(x=as.Date(Date),y=Close_price))+geom_line(colour="#000099")+
  #ggtitle(paste("Time Series Plot of Bitcoin Close price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold"),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5))+scale_x_date(date_labels="%b %y",date_breaks  ="1 month")+xlab("Date")
  

ggplot(ETH,aes(x=Date,y=Close_price))+geom_line(colour="#000099")+
  ggtitle(paste("Time Series Plot of Ethereum Close price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold"),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())



ggplot(LTC,aes(x=Date,y=Close_price))+geom_line(colour="#000099")+
  ggtitle(paste("Time Series Plot of Litecoin Close price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold"),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey"),
        panel.background = element_blank())



ggplot(RLE,aes(x=Date,y=Close_price))+geom_line(colour="#000099")+
  ggtitle(paste("Time Series Plot of Ripple Close price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold"),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())



ggplot(THR,aes(x=Date,y=Close_price))+geom_line(colour="#000099")+
  ggtitle(paste("Time Series Plot of Tether Close price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold"),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())

#Bitcoi, Ethereum and Litecoin shows similar behaviour
#Tether is out of shape when compared to Bitcoin
#Ripple does not exactly have the same pattern yet bit similar

dev.off()


##Training Data with returns

Bitcoin_with_return <- read_excel("data/return_data/Bitcoin with return.xlsx")
#View(Bitcoin_with_return)

BitcoinR= Bitcoin_with_return

Ethereum_with_return <- read_excel("data/return_data/Ethereum with return.xlsx")
#View(Ethereum_with_return)

EthereumR = Ethereum_with_return

Litecoin_with_return <- read_excel("data/return_data/Litecoin with return.xlsx")
#View(Litecoin_with_return)

LitecoinR = Litecoin_with_return



#ts.plot(BitcoinR$`Return`[-1])
#Acf(BitcoinR$`Return`[-1])
#Pacf(BitcoinR$`Return`[-1])

RS_currency=data.frame(Bitcoin$Date[2:579],EthereumR$Return[2:579],LitecoinR$Return[2:579])
colnames(RS_currency)[1]="Date"
colnames(RS_currency)[2]="Return_Ethereum_Close_price"
colnames(RS_currency)[3]="Return_Litecoin_Close_price"



ggplot(RS_currency,aes(x=Date,y=Return_Ethereum_Close_price))+geom_line(colour="#D55E00")+
  ggtitle(paste("Time Series Plot of Return of Ethereum Close price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())


ggplot(RS_currency,aes(x=Date,y=Return_Litecoin_Close_price))+geom_line(colour="#D55E00")+
  ggtitle(paste("Time Series Plot of Return of Litecoin Close price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())


Acf(LitecoinR$`Return`[-1],main = "ACF Plot of Return of Litecoin Close price")
Pacf(LitecoinR$`Return`[-1],main = "PACF Plot of Return of Litecoin Close price")

Acf(EthereumR$`Return`[-1],main = "ACF Plot of Return of Ethereum Close price")
Pacf(EthereumR$`Return`[-1],main = "PACF Plot of Return of Ethereum Close price")

#Stationarity of the return series before applying cross correlation

library(tseries)
adf.test(BitcoinR$`Return`[-1])
adf.test(LitecoinR$`Return`[-1])
adf.test(EthereumR$`Return`[-1])

#kpss.test(BitcoinR$`Return`,null = "Level")

##series are stationary
##******* doesn't become stationary with kpss.test

ggCcf=function (x, y, lag.max = NULL, type = c("correlation", 
                                         "covariance"), plot = TRUE, na.action = na.contiguous, 
          ...) 
{
  cl <- match.call()
  if (plot) {
    cl$plot <- FALSE
  }
  cl[[1]] <- quote(ccf)
  object <- eval.parent(cl)
  object$snames <- paste(deparse(substitute(x)), "&", 
                         deparse(substitute(y)))
  object$ccf <- TRUE
  if (plot) {
    return(autoplot(object, ...))
  }
  else {
    return(object)
  }
}

#cor1=ccf(BitcoinR$`Return`[-1],EthereumR$`Return`[-1],lag.max = 50,main = "CCF between Bitcoin and Ethereum Close prices")
#cor1

library(forecast)
ggCcf(BitcoinR$`Return`[-1],EthereumR$`Return`[-1],lag.max = 50)+ggtitle("CCF between Bitcoin and Ethereum Close prices")+theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
                                                                                                                                                     panel.grid.major = element_line(), 
                                                                                                                                                     panel.grid.minor = element_line(),
                                                                                                                                                     panel.background = element_rect(colour = "grey"))+ylab("CCF")+geom_hline(yintercept = 0)

#cor2=ccf(BitcoinR$`Return`[-1],LitecoinR$`Return`[-1],lag.max = 50,main = "CCF between Bitcoin and Litecoin Close prices")
#cor2

ggCcf(BitcoinR$`Return`[-1],LitecoinR$`Return`[-1],lag.max = 50)+ggtitle("CCF between Bitcoin and Litecoin Close prices")+theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
                                                                                                                                panel.grid.major = element_line(), 
                                                                                                                                panel.grid.minor = element_line(),
                                                                                                                                panel.background = element_rect(colour = "grey"))+ylab("CCF")+geom_hline(yintercept = 0)


#We will consider the 0th lag of ethereum and litecoin and not other significant lags coz they are only marginally significant


##************************Just obtained differenced series, coz it becomes stationary from both adf and kpss tests

diffbitcoin=diff(Bitcoin$Close,1)
diffethereum=diff(Ethereum$Close,1)
difflitecoin=diff(Litecoin$Close,1)

adf.test(diffbitcoin)
adf.test(difflitecoin)
adf.test(diffethereum)

kpss.test(diffbitcoin)
kpss.test(difflitecoin)
kpss.test(diffethereum)

ccf(diffbitcoin,diffethereum,lag.max = 50)
ccf(diffbitcoin,difflitecoin,lag.max = 200)


##**ln(Pt+1/Pt) didn't show stationarity
##**ln(Pt-Pt-1/Pt-1) gave null values for those which produced negtives values for the ratio.
##should i consider all the significant lags??

###***********************************************************************


#2. cross correlation among variables (training set)

library(fma)

variables_with_returns <- read_excel("data/return_data/variables with returns.xlsx")
#View(variables_with_returns)

var_returns=variables_with_returns


RS=data.frame(Bitcoin$Date[2:579],var_returns$Return_Close,var_returns$Return_Open,var_returns$Return_High,var_returns$Return_Low,var_returns$Return_Volume,var_returns$Return_MarketCap)
colnames(RS)[1]="Date"
colnames(RS)[2]="Return_Close_price"
colnames(RS)[3]="Return_Open_price"
colnames(RS)[4]="Return_High_price"
colnames(RS)[5]="Return_Low_price"
colnames(RS)[6]="Return_Volume"
colnames(RS)[7]="Return_Market_Cap"

ggplot(RS,aes(x=Date,y=Return_Close_price))+geom_line(colour="#009E73")+
  ggtitle(paste("Time Series Plot of Return of Bitcoin Close price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())

ggplot(RS,aes(x=Date,y=Return_Open_price))+geom_line(colour="#009E73")+
  ggtitle(paste("Time Series Plot of Return of Open price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())


ggplot(RS,aes(x=Date,y=Return_High_price))+geom_line(colour="#009E73")+
  ggtitle(paste("Time Series Plot of Return of High price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())


ggplot(RS,aes(x=Date,y=Return_Low_price))+geom_line(colour="#009E73")+
  ggtitle(paste("Time Series Plot of Return of Low price"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())


ggplot(RS,aes(x=Date,y=Return_Volume))+geom_line(colour="#009E73")+
  ggtitle(paste("Time Series Plot of Return of Trading Volume"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())


ggplot(RS,aes(x=Date,y=Return_Market_Cap))+geom_line(colour="#009E73")+
  ggtitle(paste("Time Series Plot of Return of Market Capitalization"))+
  theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        panel.background = element_blank())



Acf(var_returns$Return_Close,main = "ACF Plot of Return of Close price")
Pacf(var_returns$Return_Close,main = "PACF Plot of Return of Close price")

Acf(var_returns$Return_Open,main = "ACF Plot of Return of Open price")
Pacf(var_returns$Return_Open,main = "PACF Plot of Return of Open price")

Acf(var_returns$Return_High,main = "ACF Plot of Return of High price")
Pacf(var_returns$Return_High,main = "PACF Plot of Return of High price")

Acf(var_returns$Return_Low,main = "ACF Plot of Return of Low price")
Pacf(var_returns$Return_Low,main = "PACF Plot of Return of Low price")

Acf(var_returns$Return_Volume,main = "ACF Plot of Return of Trading Volume")
Pacf(var_returns$Return_Volume,main = "PACF Plot of Return of Trading Volume")


Acf(var_returns$Return_MarketCap,main = "ACF Plot of Return of Market Capitalization")
Pacf(var_returns$Return_MarketCap,main = "PACF Plot of Return of Market Capitalization")

adf.test(var_returns$Return_Close)
adf.test(var_returns$Return_Open)
adf.test(var_returns$Return_High)
adf.test(var_returns$Return_Low)
adf.test(var_returns$Return_Volume)
adf.test(var_returns$Return_MarketCap)

#cor3=ccf(var_returns$Return_Close,var_returns$Return_Open,lag.max = 20,main = "CCF between Close price and Open price") #0th lag is insignificant

ggCcf(var_returns$Return_Close,var_returns$Return_Open,lag.max = 25)+ggtitle("CCF between Close price and Open price")+theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
       panel.grid.major = element_line(), 
       panel.grid.minor = element_line(),
       panel.background = element_rect(colour = "grey"))+ylab("CCF")+geom_hline(yintercept = 0)



#cor4=ccf(var_returns$Return_Close,var_returns$Return_High,lag.max = 20,main = "CCF between Close price and High price") #0th lag is significant

ggCcf(var_returns$Return_Close,var_returns$Return_High,lag.max = 25)+ggtitle("CCF between Close price and High price")+theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
                                                                                                                             panel.grid.major = element_line(), 
                                                                                                                             panel.grid.minor = element_line(),
                                                                                                                             panel.background = element_rect(colour = "grey"))+ylab("CCF")+geom_hline(yintercept = 0)

#cor5=ccf(var_returns$Return_Close,var_returns$Return_Low,lag.max = 20,main = "CCF between Close price and Low price")  #0th lag is significant

ggCcf(var_returns$Return_Close,var_returns$Return_Low,lag.max = 25)+ggtitle("CCF between Close price and Low price")+theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
                                                                                                                             panel.grid.major = element_line(), 
                                                                                                                             panel.grid.minor = element_line(),
                                                                                                                             panel.background = element_rect(colour = "grey"))+ylab("CCF")+geom_hline(yintercept = 0)


#par(cex.main=1)
#cor6=ccf(var_returns$Return_Close,var_returns$Return_Volume,lag.max = 25,main = "CCF between Close price and Trading Volume")#0th lag is significant

ggCcf(var_returns$Return_Close,var_returns$Return_Volume,lag.max = 25)+ggtitle("CCF between Close price and Trading Volume")+theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
                                                                                                                           panel.grid.major = element_line(), 
                                                                                                                           panel.grid.minor = element_line(),
                                                                                                                           panel.background = element_rect(colour = "grey"))+ylab("CCF")+geom_hline(yintercept = 0)



#cor7=ccf(var_returns$Return_Close,var_returns$Return_MarketCap,lag.max = 20,main = "CCF between Close price and Market Capitalization") #0th lag is significant

ggCcf(var_returns$Return_Close,var_returns$Return_MarketCap,lag.max = 25)+ggtitle("CCF between Close price and Market Capitalization")+theme(plot.title = element_text(hjust=0.5,face = "bold",size = 12),axis.line = element_line(colour = "black"),
                                                                                                                                   panel.grid.major = element_line(), 
                                                                                                                                   panel.grid.minor = element_line(),
                                                                                                                                   panel.background = element_rect(colour = "grey"))+ylab("CCF")+geom_hline(yintercept = 0)



##shows the correlation among the variables with close


#3. Variables selection

#since we are to forecast, we require lagged values of the variables. moreover, we will omit considering open, high and low variables as independent variables as those can be related to close directly and cause misinterpretations.

