library(tidyverse)


setwd("/Users/benjaminglass/Downloads")
hfm <- readRDS("hf.master.RDS")


setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
MGP_25 <- as.data.frame(read.csv("Analysis0708_MGP_RESULTS.csv"))


MGP_25_mutated <- MGP_25 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay,
         MGP_C_mean = as.numeric(MGP_C_mean),
         MGP_C_std = as.numeric(MGP_C_std),
         MGP_D_mean = as.numeric(MGP_D_mean),
         MGP_D_std = as.numeric(MGP_D_std))


MGP_analysis <- merge(hfm,MGP_25_mutated,by=c("Year.Year","time_days"))

#MAY UNTIL SEPTEMBER
#2017 daytime
MGP_2017 <- MGP_analysis %>%
  filter(Year.Year == 2017,
         month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"Day","Night"))

#2018 daytime
MGP_2018 <- MGP_analysis %>%
  filter(Year.Year == 2018,
        month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"Day","Night"))

#2019 daytime
MGP_2019 <- MGP_analysis %>%
  filter(Year.Year == 2019,
          month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"Day","Night"))

MGP_total <- MGP_analysis %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"Day","Night"))


#2017 Decid
ggplot(data = MGP_2017, aes(x = MGP_D_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  geom_point(shape=20,alpha=.5) +
  facet_wrap(~ month.Month) +
  ggtitle("FCO2 vs. Decidious Proportion for 2017 measurements") +
  scale_x_continuous() +
  scale_y_continuous()
  #geom_smooth()

#2017 Coniferous
ggplot(data = MGP_2017, aes(x = MGP_C_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  geom_point(shape=20,alpha=.5) +
  facet_wrap(~ month.Month) +
  ggtitle("FCO2 vs. Coniferous Proportion for Daytime 2017 measurements") +
  scale_x_continuous() +
  scale_y_continuous()
  #geom_smooth()




#2018
ggplot(data = MGP_2018, aes(x = MGP_D_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. Decidious Proportion for Daytime 2018 measurements") +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()

ggplot(data = MGP_2018, aes(x = MGP_C_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. Coniferous Proportion for Daytime 2018 measurements") +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()


#2019
ggplot(data = MGP_2019, aes(x = MGP_D_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. Decidious Proportion for Daytime 2019 measurements") +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()

ggplot(data = MGP_2019, aes(x = MGP_C_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. Coniferous Proportion for Daytime 2019 measurements") +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()



# =============== AGGREGATES
#MGP aggregates
ggplot(data = MGP_total, aes(x = MGP_D_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  facet_wrap(~ Year.Year) +
  geom_point(shape=20) +
  ggtitle("FCO2 vs. Decidious Proportion for measurements May-Sept") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous()
  #geom_smooth()

ggplot(data = MGP_total, aes(x = MGP_C_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  facet_wrap(~ Year.Year) +
  geom_point(shape=20) +
  ggtitle("FCO2 vs. Coniferous Proportion for for measurements May-Sept") +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous()







