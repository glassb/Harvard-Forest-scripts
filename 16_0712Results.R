library(tidyverse)
setwd("/Users/benjaminglass/Downloads")
hfm <- readRDS("hfmaster_0713.RDS")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
results0712 <- as.data.frame(read.csv("all_results_0712_weightedmean.csv"))

results_mutated <- results0712 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay)

results <- merge(hfm,results_mutated,by=c("Year.Year","time_days"))

results_prime <- results %>%
  filter(month.Month %in% (1:12)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))


# BY YEAR
ggplot(data = results_prime, aes(x=TCTg_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  facet_wrap(~ Year.Year) +
  geom_point(shape=20,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous()

# BY MONTH
ggplot(data = results_prime, aes(x=TCTb_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic()

results_prime <- results %>%
  filter(month.Month %in% (1:12)) %>%
         #PAR.28m.e.6mol.m2.s > 50) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"),
         MGP_highP = ifelse(MGP_D_mean >.75,1,0))

ggplot(data = results_prime) +
  #facet_wrap(~ month.Month) +
  geom_point(aes(y=obs.FCO2.e.6mol.m2.s,x=L_mean,color=as.factor(ToD)),shape=20,alpha=.5) +
  #geom_point(aes(y=obs.FCO2.e.6mol.m2.s,x=TCTg_mean),shape=20,alpha=.3,color="aquamarine3") +
  #geom_point(aes(y=obs.FCO2.e.6mol.m2.s,x=TCTw_mean),shape=20,alpha=.3,color="deepskyblue") +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic()




ggplot(data = results_prime) +
  #facet_wrap(~ month.Month) +
  geom_point(aes(x=time_days,y=TCTb_mean),shape=20,alpha=.3,color="coral1") +
  geom_point(aes(x=time_days,y=TCTg_mean),shape=20,alpha=.3,color="aquamarine3") +
  geom_point(aes(x=time_days,y=TCTw_mean),shape=20,alpha=.3,color="deepskyblue") +
  scale_x_continuous() +
  scale_y_continuous() +
  ylab("TCT components") +
  theme_classic()


# library(akima)
# akima::interp(x=results_prime$TCTb_mean,
#                 y=results_prime$TCTw_mean,
#                 z=results_prime$TCTw_mean)

library(scatterplot3d)
colors <- c("#999999")
  scatterplot3d::scatterplot3d(x=results_prime$TCTb_mean,
                               xlab = "TCTb",
                               y=results_prime$TCTg_mean,
                               ylab = "TCTg",
                               z=results_prime$TCTw_mean,
                               zlab="TCTw",
                               main="3D scatter plot",
                               color="Red",
                               col.axis="blue",
                               pch=21,
                               #box=FALSE,
                               angle=30)

  
  
  
  
  
library(car)
library(rgl)
  
scatter3d(x=results_prime$TCTb_mean,
            y=results_prime$TCTg_mean,
            z=results_prime$TCTw_mean)
  

install.packages("/Users/benjaminglass/Downloads/rgl_0.106.8.tgz", repos=NULL, type="source")




