#visualizations
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(MLmetrics)

train_df <- read_excel("svm training set.xlsx")
colnames(train_df)[1] <- "Date"
train_df$Date <- as.Date(train_df$Date)
colnames(train_df)[4] <- "Market_Cap"
test_df <- read_excel("svm testing set.xlsx")
colnames(test_df)[4] <- "Market_Cap"
test_df$Date <- as.Date(test_df$Date)
df <- rbind(train_df,test_df)
volume_divisor <- 1000000
mc_divisor <- 10000000
df <- df %>% mutate(Market_Cap_stndrd=(Market_Cap-mean(Market_Cap))/sd(Market_Cap),
                    Close_stndrd=(Close-mean(Close))/sd(Close),
                    Litecoin_stndrd=(Litecoin-mean(Litecoin))/sd(Litecoin),
                    Ethereum_stndrd=(Ethereum-mean(Ethereum))/sd(Ethereum),
                    Volume_stndrd=(Volume-mean(Volume))/sd(Volume),
                    Volume_divided=Volume/volume_divisor,
                    Market_Cap_divided=Market_Cap/mc_divisor
                    )
train_df <- train_df %>% mutate(Market_Cap_stndrd=(Market_Cap-mean(Market_Cap))/sd(Market_Cap),
                                Close_stndrd=(Close-mean(Close))/sd(Close),
                                Litecoin_stndrd=(Litecoin-mean(Litecoin))/sd(Litecoin),
                                Ethereum_stndrd=(Ethereum-mean(Ethereum))/sd(Ethereum),
                                Volume_stndrd=(Volume-mean(Volume))/sd(Volume),
                                Volume_divided=Volume/volume_divisor,
                                Market_Cap_divided=Market_Cap/mc_divisor
                                )
test_df <- test_df %>% mutate(Market_Cap_stndrd=(Market_Cap-mean(Market_Cap))/sd(Market_Cap),
                              Close_stndrd=(Close-mean(Close))/sd(Close),
                              Litecoin_stndrd=(Litecoin-mean(Litecoin))/sd(Litecoin),
                              Ethereum_stndrd=(Ethereum-mean(Ethereum))/sd(Ethereum),
                              Volume_stndrd=(Volume-mean(Volume))/sd(Volume),
                              Volume_divided=Volume/volume_divisor,
                              Market_Cap_divided=Market_Cap/mc_divisor
                              )
df_long <- gather(df,type,value,c(Close
                                  # Market_Cap_divided,
                                  # Litecoin,
                                  # Ethereum,
                                  # Volume_divided
                                  ))
ggplot(df_long,aes(x=Date,y=value))+
  geom_line(aes(color=type),size=0.25)+
  geom_point(aes(color=type),size=0.15)+
  theme_minimal()

## PLAYGROUND
MAPE(df$Market_Cap_stndrd,df$Close_stndrd)
write_csv(train_df,"train_df_standardized.csv")
write_csv(test_df,"test_df_standardized.csv")
write_csv(df,"df_standardized.csv")
