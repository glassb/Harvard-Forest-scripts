library(ggpubr)
library(tidyverse)

########## ===================== FRONT MATTER


#=========== EMS
setwd("/Users/benjaminglass/Downloads")
hfm <- readRDS("hfmaster_0713.RDS")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
results0712 <- as.data.frame(read.csv("EMS_results_0719_weightedmean.csv"))


results_mutated <- results0712 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay)

results_EMS <- merge(hfm,results_mutated,by=c("Year.Year","time_days"))


#============= NEON
library(tidyverse)

setwd("/Users/benjaminglass/Downloads")
NEONmaster <- as.data.frame(read.csv(file="neon_hr.csv",header=TRUE,stringsAsFactors=FALSE))

#mutate resultsNEON

NEONmaster_mutated <- NEONmaster %>%
  mutate(Year.Year = year,
         month.Month = mon,
         time_days = dec.day)

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
results0712 <- as.data.frame(read.csv("NEON_results_0720_weightedmean.csv"))

summary(results0712)

results_mutated <- results0712 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay)

results_NEON <- merge(NEONmaster_mutated,results_mutated,by=c("Year.Year","time_days"))




summary(results_EMS)


########## ===================== DECIDIOUS 



results_prime <- results_EMS %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

EMS_D <- ggplot(data = results_prime, aes(x=MGP_D_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.6) +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous() +
  theme_classic()


results_prime_NEON <- results_NEON %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PPFD_IN_1_1_1 > 50,"day","night"))


NEON_D <- ggplot(data = results_prime_NEON, aes(x=MGP_D_mean,y=FC,color=ToD)) +
  #facet_wrap(~ month.Month) +
  geom_point(shape=20,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() 


figure <- ggarrange(EMS_D, NEON_D,
                    labels = c("EMS Tower","NEON Tower"),
                    ncol = 2, nrow = 1)

annotate_figure(figure,
                top = text_grob("CO2 Flux vs Decidious % of FFP during Maturity (May-Sept)",face="bold",size=16))






########## ===================== CONIFER




results_prime <- results_EMS %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

EMS_C <- ggplot(data = results_prime, aes(x=MGP_C_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous() +
  theme_classic()


results_prime_NEON <- results_NEON %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PPFD_IN_1_1_1 > 50,"day","night"))


NEON_C <- ggplot(data = results_prime_NEON, aes(x=MGP_C_mean,y=FC,color=ToD)) +
  #facet_wrap(~ month.Month) +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() 


figure <- ggarrange(EMS_C, NEON_C,
                    labels = c("EMS Tower","NEON Tower"),
                    ncol = 2, nrow = 1)

annotate_figure(figure,
                top = text_grob("CO2 Flux vs Coniferous % of FFP during Maturity (May-Sept)",face="bold",size=16))






########## ===================== Canopy Height Mean




results_prime <- results_EMS %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

EMS_C <- ggplot(data = results_prime, aes(x=L_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic()


results_prime_NEON <- results_NEON %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PPFD_IN_1_1_1 > 50,"day","night"))


NEON_C <- ggplot(data = results_prime_NEON, aes(x=L_mean,y=FC,color=ToD)) +
  #facet_wrap(~ month.Month) +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() 


figure <- ggarrange(EMS_C, NEON_C,
                    labels = c("EMS Tower","NEON Tower"),
                    ncol = 2, nrow = 1)

annotate_figure(figure,
                top = text_grob("CO2 Flux vs avg Canopy Height of FFP during Maturity (May-Sept)",face="bold",size=16))








