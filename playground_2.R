#playground lag
library(tis)
library(dplyr)

lag_dataset<-function(dataframe,steps){
  library(tis)
  library(dplyr)
  for(col in colnames(dataframe)){
    print(col)
    for(i in 1:steps){
      column_name <- paste(col,i,sep="_")
      
      dataframe <- dataframe %>% mutate(
        !!column_name := lag(dataframe[,col],i)
      ) %>%
        select(sort(names(.)))
      
      
    }
  }
  # na.omit(dataframe)
  dataframe
}

ds <- data.frame(
  A = seq(1,10),
  B = seq(21,30),
  C = seq(11,20)
)
ds

lag_dataset(ds,2)

