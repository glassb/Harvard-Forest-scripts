
# library(tidyverse)

setwd("/Users/benjaminglass/Downloads")

hfm <- readRDS("hf.master.RDS")

# print(names(hfm))

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
         TCTw_mean = as.numeric(TCTw_mean),
         Year.Year = as.factor(Year.Year))

summary(hfmaster_analysis$PAR.28m.e.6mol.m2.s)

nighttime_analysis <- hfmaster_analysis %>%
  filter(PAR.28m.e.6mol.m2.s <= 50)

daytime_analysis <- hfmaster_analysis %>%
  filter(PAR.28m.e.6mol.m2.s > 50)

ggplot(data = nighttime_analysis, aes(x = L_mean,y=obs.FCO2.e.6mol.m2.s,color=Year.Year)) +
  geom_point(alpha=.3,shape=20) +
  scale_x_continuous(name="Lidar CHM",limits=c(15,25)) +
  scale_y_continuous(limits=c(-20,20)) +
  #geom_smooth() +
  theme_test()

ggplot(data = daytime_analysis, aes(x = L_mean,y=obs.FCO2.e.6mol.m2.s,color=Year.Year)) +
  geom_point(alpha=1,shape=20) +
  scale_x_continuous(name="Lidar CHM",limits=c(15,25)) +
  scale_y_continuous(limits=c(-20,20)) +
  #geom_smooth() +
  theme_test()



day_year2017 <- hfmaster_analysis %>%
  filter(Year.Year == 2017,
         PAR.28m.e.6mol.m2.s > 50)


day_year2018 <- hfmaster_analysis %>%
  filter(Year.Year == 2018,
         PAR.28m.e.6mol.m2.s > 50)

day_year2019 <- hfmaster_analysis %>%
  filter(Year.Year == 2019,
         PAR.28m.e.6mol.m2.s > 50)

ggplot(data = day_year2017, aes(x = TCTg_mean,y=obs.FCO2.e.6mol.m2.s,color=Year.Year)) +
  geom_point(alpha=.3,shape=20) +
  scale_x_continuous() +
  scale_y_continuous(limits=c(-20,20)) +
  geom_smooth() +
  theme_test()

ggplot(data = day_year2018, aes(x = TCTg_mean,y=obs.FCO2.e.6mol.m2.s,color=Year.Year)) +
  geom_point(alpha=.3,shape=20) +
  scale_x_continuous() +
  scale_y_continuous(limits=c(-20,20)) +
  geom_smooth() +
  theme_test()

ggplot(data = day_year2019, aes(x = TCTg_mean,y=obs.FCO2.e.6mol.m2.s,color=Year.Year)) +
  geom_point(alpha=.3,shape=20) +
  scale_x_continuous() +
  scale_y_continuous(limits=c(-20,20)) +
  geom_smooth() +
  theme_test()

summary(day_year2017)








# print(doneFiles)
# 
# print(names(hfmaster_analysis))

ggplot(data = hfmaster_analysis, aes(x = L_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(alpha=.3,shape=20) +
  scale_x_continuous(name="Lidar CHM",limits=c(15,25)) +
  scale_y_continuous(limits=c(-20,20)) +
  #geom_smooth() +
  theme_test()

ggplot(data = hfmaster_analysis,aes(x = TCTg_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(alpha=.3,shape=20) +
  scale_x_continuous(name="") +
  scale_y_continuous(limits=c(-20,20)) +
  #geom_smooth() +
  theme_test()


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





## ENTIRE DATA SET 


colnames(hfmaster_analysis)


hf_L_TCT <- hfmaster_analysis %>%
  select(-c(107,108,109,110))

colnames(hf_L_TCT)
  
colnames(MGP_25)

hf_analysis <- merge(hf_L_TCT,MGP_25,by=c("year","decDay"))

hf_analysis <- hf_analysis %>%
  mutate(Year.Year = as.numeric(paste0("20",year)),
         MGP_C_mean = as.numeric(MGP_C_mean),
         MGP_C_std = as.numeric(MGP_C_std),
         MGP_D_mean = as.numeric(MGP_D_mean),
         MGP_D_std = as.numeric(MGP_D_std))
       




MGP_2017 <- hf_analysis %>%
  filter(Year.Year == 2017,
         PAR.28m.e.6mol.m2.s > 50)

MGP_2018 <- hf_analysis %>%
  filter(Year.Year == 2018,
         PAR.28m.e.6mol.m2.s > 50)

MGP_2019 <- hf_analysis %>%
  filter(Year.Year == 2019,
         PAR.28m.e.6mol.m2.s > 50)



ggplot(data = MGP_2017, aes(x = MGP_D_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.3) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()

ggplot(data = MGP_2017, aes(x = MGP_C_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.3) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()

ggplot(data = MGP_2018, aes(x = MGP_D_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.3) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()

ggplot(data = MGP_2018, aes(x = MGP_C_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.3) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()


ggplot(data = MGP_2019, aes(x = MGP_D_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.3) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()

ggplot(data = MGP_2019, aes(x = MGP_C_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.3) +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()





