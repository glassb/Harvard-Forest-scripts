
# library(tidyverse)

setwd("/Users/benjaminglass/Downloads")

hfm <- readRDS("hf.master.RDS")

# print(names(hfm))

print(summary(hfm))

write.csv(masterResults,"/Users/benjaminglass/Desktop/HF21/00_Datasets/Analysis0707INCOMPLETE_18-19.csv", row.names = FALSE)

masterResults <- masterResults %>%
  mutate(Year.Year = paste0("20",year),
         DoY.Day = decDay,
         L_mean = as.numeric(L_mean),
         MGP_C_mean = as.numeric(MGP_C_mean),
         MGP_D_mean = as.numeric(MGP_D_mean),
         MGP_D_std = as.numeric(MGP_D_std),
         TCTb_mean = as.numeric(TCTb_mean),
         TCTg_mean = as.numeric(TCTb_mean),
         TCTw_mean = as.numeric(TCTw_mean))

set1819 <- merge(hfm,masterResults,by=c("Year.Year","DoY.Day"))


#THIS HAS ALL THE DATA
hfmaster_analysis <- rbind(hf_master_analysis,set1819)


hfmaster_analysis <- hfmaster_analysis %>%
  mutate(L_mean = as.numeric(L_mean),
         MGP_C_mean = as.numeric(MGP_C_mean),
         MGP_C_std = as.numeric(MGP_C_std),
         MGP_D_mean = as.numeric(MGP_D_mean),
         MGP_D_std = as.numeric(MGP_D_std),
         TCTb_mean = as.numeric(TCTb_mean),
         TCTg_mean = as.numeric(TCTb_mean),
         TCTw_mean = as.numeric(TCTw_mean))

hfmaster_analysis <- hfmaster_analysis %>%
   filter(DoY.Day <= 121)

# print(doneFiles)
# 
# print(names(hfmaster_analysis))

ggplot(data = hfmaster_analysis, aes(x = L_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point() +
  scale_x_continuous(name="",limits=c(20,25)) +
  scale_y_continuous(limits=c(-10,10)) +
  geom_smooth()

# ggplot(data = hfmaster_analysis, aes(x = MGP_D_mean,y=nee.e.6mol.m2.s)) + 
#   geom_point() +
#   scale_x_continuous() +
#   scale_y_continuous(limits = c(-25,25)) +
#   geom_smooth()


# summary(hfmaster_analysis$L_mean)
# summary(hfmaster_analysis$MGP_C_mean)


ggplot(data = hfmaster_analysis, aes(x = MGP_D_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_bar(stat="identity") +
  geom_smooth()










setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")

MGP_25 <- as.data.frame(read.csv("Analysis0708_MGP_RESULTS.csv"))

MGP_25 <- MGP_25 %>%
  mutate(Year.Year = paste0("20",year),
         DoY.Day = decDay,
         MGP_C_mean = as.numeric(MGP_C_mean),
         MGP_C_std = as.numeric(MGP_C_std),
         MGP_D_mean = as.numeric(MGP_D_mean),
         MGP_D_std = as.numeric(MGP_D_std))


MGP_analysis <- merge(hfm,MGP_25,by=c("Year.Year","DoY.Day"))

MGP_maturity <- MGP_analysis %>%
  filter(DoY.Day >= 125 & DoY.Day <= 275)



ggplot(data = MGP_analysis, aes(x = MGP_D_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()


ggplot(data = MGP_maturity, aes(x = MGP_C_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point() +
  scale_x_continuous() +
  scale_y_continuous(limits=c(-10,10)) +
  geom_smooth()



       


