lag_dataset<-function(dataframe,steps){
  library(tis)
  library(dplyr)
  for(col in colnames(dataframe)){
    for(i in 1:steps){
      column_name <- paste(col,i,sep="_t_")
      # print(class(dplyr::pull(dataframe,col)))
      dataframe <- dataframe %>% mutate(
        # !!column_name := list(dataframe[,col])
        !!column_name := lag(dplyr::pull(dataframe,col),i)
        # !!column_name := lag(dataframe[,col],i)
      ) %>%
        select(sort(names(.)))
      # print(dataframe)
    }
  }
  
  na.omit(dataframe)
  # dataframe
}