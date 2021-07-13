
library(tidyverse)

setwd("/Users/benjaminglass/Downloads")
hfm <- readRDS("hf.master.RDS")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
L_TCT_17 <- as.data.frame(read.csv("Analysis0706INCOMPLETE.csv"))
L_TCT_1819 <- as.data.frame(read.csv("Analysis0707INCOMPLETE_18-19.csv"))


r17_mutated <- L_TCT_17 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = as.numeric(decDay),
         L_mean = as.numeric(L_mean),
         TCTb_mean = as.numeric(TCTb_mean),
         TCTg_mean = as.numeric(TCTb_mean),
         TCTw_mean = as.numeric(TCTw_mean))

r1819_mutated <- L_TCT_1819 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = as.numeric(decDay),
         L_mean = as.numeric(L_mean),
         TCTb_mean = as.numeric(TCTb_mean),
         TCTg_mean = as.numeric(TCTb_mean),
         TCTw_mean = as.numeric(TCTw_mean))


L_TCT_results_17 <- merge(hfm,r17_mutated,by=c("Year.Year","time_days"))
L_TCT_results_1819 <- merge(hfm,r1819_mutated,by=c("Year.Year","time_days"))

L_TCT_results <- rbind(L_TCT_results_17,L_TCT_results_1819)

results_total <- L_TCT_results %>%
  filter(month.Month %in% c(5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"Day","Night"))


# ================ AGGREGATE
#lidar aggregate
ggplot(data = results_total, aes(x = L_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. Canopy Height for measurements May-Sept") +
  scale_x_continuous(limits=c(19,26)) +
  scale_y_continuous()


ggplot(data = results_total) +
  #facet_wrap(~ month.Month) +
  geom_point(aes(x=TCTw_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD),shape=20,alpha=.5) +
  #facet_wrap(~ month.Month) +
  #geom_point(aes(x=time_days,y=TCTg_mean),shape=20,alpha=.5,color="green") +
  #geom_point(aes(x=time_days,y=TCTw_mean),shape=20,alpha=.5,color="blue") +
  ggtitle("FCO2 vs. TCTb for measurements May-Sept") +
  scale_x_continuous() +
  scale_y_continuous()



#TCT brightness aggregate
ggplot(data = results_total, aes(x = TCTg_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. TCTg for measurements May-Sept") +
  scale_x_continuous(limits=c(0.1,0.4)) +
  scale_y_continuous()

ggplot(data = results_total, aes(x = TCTw_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. TCTw for measurements May-Sept") +
  scale_x_continuous() +
  scale_y_continuous()

#TCT wetness aggregate
ggplot(data = L_TCT_results, aes(x = TCTw_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. TCTw for 2017-2019 measurements") +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()

#TCT greenness aggregate
ggplot(data = L_TCT_results, aes(x = TCTg_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. TCTg for 2017-2019 measurements") +
  scale_x_continuous(limits=c(.1,.5)) +
  scale_y_continuous() +
  geom_smooth()


daytime17 <- L_TCT_results %>%
  filter(Year.Year == 2017,
         #month.Month %>% (5:9),
         PAR.28m.e.6mol.m2.s > 50)

daytime18 <- L_TCT_results %>%
  filter(Year.Year == 2018,
         PAR.28m.e.6mol.m2.s > 50)

daytime19 <- L_TCT_results %>%
  filter(Year.Year == 2019,
         PAR.28m.e.6mol.m2.s > 50)

ggplot(data = daytime17, aes(x = L_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.5) +
  facet_wrap(~ month.Month)
  ggtitle("FCO2 vs. Canopy Height for 2017 daytime measurements") +
  scale_x_continuous(limits=c(19,26)) +
  scale_y_continuous() +
  geom_smooth()

ggplot(data = daytime17, aes(x = TCTg_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.5) +
  facet_wrap(~ month.Month)
  ggtitle("FCO2 vs. TCT greenness for 2017 daytime measurements") +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()

ggplot(data = daytime19, aes(x = TCTw_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. Canopy Height for 2018 daytime measurements") +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()

print(daytime19$TCTw_mean)




ggplot(data = daytime19, aes(x = L_mean,y=obs.FCO2.e.6mol.m2.s)) +
  geom_point(shape=20,alpha=.5) +
  ggtitle("FCO2 vs. Canopy Height for 2019 daytime measurements") +
  scale_x_continuous() +
  scale_y_continuous() +
  geom_smooth()


summary(daytime19)
