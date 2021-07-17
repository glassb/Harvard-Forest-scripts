library(tidyverse)
setwd("/Users/benjaminglass/Downloads")
hfm <- readRDS("hfmaster_0713.RDS")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
results0712 <- as.data.frame(read.csv("NEON_all_results_0717_weightedmean.csv"))

summary(results0712)

results_mutated <- results0712 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay)

results <- merge(hfm,results_mutated,by=c("Year.Year","time_days"))


#=============== CANOPY HEIGHT BY MONTH =====================
results_prime <- results %>%
  filter(month.Month %in% (1:12)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

ggplot(data = results_prime, aes(x=L_mean,y=obs.FCO2.e.6mol.m2.s)) +
  facet_wrap(~ month.Month,ncol=4) +
  geom_point(shape=20,alpha=.9) +
  scale_x_continuous(limits=c(15,25)) +
  scale_y_continuous() +
  theme_classic() +
  labs(title = "Daytime CO2 Flux vs. Canopy Height",
       x = "Canopy Height Model (m)")



#=================== TCTb by month
results_prime <- results %>%
  filter(month.Month %in% (1:12),
         PAR.28m.e.6mol.m2.s > 50) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

ggplot(data = results_prime, aes(x=TCTb_mean,y=obs.FCO2.e.6mol.m2.s)) +
  facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5,color="coral1") +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() +
  labs(title = "Daytime CO2 Flux vs. TCT brightness index",
       x = "TCT brightness index)")



#===================== TCTg by month

ggplot(data = results_prime, aes(x=TCTg_mean,y=obs.FCO2.e.6mol.m2.s)) +
  facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5,color="aquamarine4") +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() +
  labs(title = "Daytime CO2 Flux vs. TCT greenness index",
       x = "TCT greenness index)")



#===================== TCTw by month

ggplot(data = results_prime, aes(x=TCTw_mean,y=obs.FCO2.e.6mol.m2.s)) +
  facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5,color="deepskyblue4") +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() +
  labs(title = "Daytime CO2 Flux vs. TCT wetness index",
       x = "TCT wetness index)")


# ========== TCT by time =================
results_prime <- results %>%
  filter(month.Month %in% (1:12)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

ggplot(data = results_prime) +
  #facet_wrap(~ month.Month) +
  geom_point(aes(x=time_days,y=TCTb_mean),shape=20,alpha=.3,color="coral1") +
  geom_point(aes(x=time_days,y=TCTg_mean),shape=20,alpha=.3,color="aquamarine3") +
  geom_point(aes(x=time_days,y=TCTw_mean),shape=20,alpha=.3,color="deepskyblue") +
  scale_x_continuous() +
  scale_y_continuous() +
  ylab("TCT components") +
  theme_classic()



#================ 3D plot ====================

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
                             pch=1,
                             cex.symbols = .1,
                             #box=FALSE,
                             angle=10)


library(car)
library(rgl)

scatter3d(x=results_prime$TCTb_mean,
          y=results_prime$TCTg_mean,
          z=results_prime$TCTw_mean)


install.packages("/Users/benjaminglass/Downloads/rgl_0.106.8.tgz", repos=NULL, type="source")




# =================== Megaplot Decid =====================
results_prime <- results %>%
  filter(month.Month %in% (1:12)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))


ggplot(data = results_prime, aes(x=MGP_D_mean,y=obs.FCO2.e.6mol.m2.s)) +
  facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5,color="orange") +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() +
  labs(title = "Daytime CO2 Flux vs. Megaplot decidious proportion",
       x = "% Decidious")


# =================== Conifer Decid =====================
results_prime <- results %>%
  filter(month.Month %in% (1:12)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

ggplot(data = results_prime, aes(x=MGP_C_mean,y=obs.FCO2.e.6mol.m2.s)) +
  facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5,color="aquamarine4") +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() +
  labs(title = "Daytime CO2 Flux vs. Megaplot conifer proportion",
       x = "% conifer")


#============== DTM
results_prime <- results %>%
  filter(month.Month %in% (1:12)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

ggplot(data = results_prime, aes(x=DTM_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  facet_wrap(~ month.Month) +
  geom_point(shape=20,cex=.5,alpha=.3) +
  scale_y_continuous() +
  geom_smooth() +
  scale_x_continuous(limits=c(335,345)) +
  theme_classic()
