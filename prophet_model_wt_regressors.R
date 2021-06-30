library(readxl)
library(dplyr)
library(tibble)
library(ggplot2)
library(prophet)

data2 <- read_excel("data2.xlsx")
data2=mutate(data2,ds=date,y=close)
data2=column_to_rownames(data2,var="date")

ggplot(data2,aes(x=ds,y=y))+geom_line()

changepointss=c("2017-07-14","2017-11-05",
                "2017-12-05","2018-01-21",
                "2018-04-28","2018-09-08",
                "2018-10-18","2018-11-13")

m=prophet(growth = "linear",seasonality.mode = "multiplicative",
          yearly.seasonality = FALSE,daily.seasonality = FALSE,weekly.seasonality = FALSE)
m=add_seasonality(m,name='monthly',period=31,fourier.order=12) 
m=add_seasonality(m,name='daily',period=1,fourier.order=15) 
m=add_seasonality(m,name='yearly',period=240,fourier.order=20) 
m=add_regressor(m,'litecoin',standardize = FALSE)
m=add_regressor(m,'ethereum',standardize = FALSE)
m=fit.prophet(m,data2)