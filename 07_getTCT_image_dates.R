library(tidyverse)

get_LS8_image_dates <- function() {
  
  setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets/TCT_outputs")
  
  df = data.frame(Year.Year=NA,month.Month=NA,DoY.Day=NA)
  
  temp = list.files()
  #print(temp)
  for (file in temp) {
    date <- substr(file,17,20)
    #print(date)
    date <- as.Date(paste0(substr(file,13,16),"-",substr(file,17,18),"-",substr(file,19,20)))
    
    row <- c(as.numeric(substr(file,13,16)),
             as.numeric(substr(file,17,18)),
             as.numeric(yday(date)))
    #print(row)
    df <- rbind(df,row)
  }
  
  print(df)
  #print(df$DoY.Day)
  
  setwd("/Users/benjaminglass/Downloads")
  hfm <- readRDS("hfmaster_0713.RDS")
  
  results <- merge(df,hfm,by=c("Year.Year","month.Month","DoY.Day"))
  
  summary(results$DoY.Day)

  print(unique(results$seq_day.90.Day.90))
  
  
  
  
  return(results)

}

