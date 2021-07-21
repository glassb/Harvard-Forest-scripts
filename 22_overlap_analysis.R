library(ggpubr)
library(tidyverse)



setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
results <- as.data.frame(read.csv("overlapScript_0720.csv"))

results_prime <- results %>%
  filter(SpatOverlapPC > 0) %>%
  mutate(flux_diff_abs = abs(obs.FCO2.e.6mol.m2.s-FC))


ggplot(data = results_prime, aes(x=SpatOverlapPC,y=flux_diff_abs)) +
  #facet_wrap(~ month.Month,ncol=4) +
  geom_point(shape=20,cex=1,alpha=.8) +
  scale_x_continuous(limits=c(0,.4)) +
  scale_y_continuous() +
  theme_classic() +
  labs(title="abs Difference in CO2 Flux (NEON and EMS towers) vs. spatial FFP overlap")




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




results_primo <- merge(merge(results_EMS,results_NEON,by=c("Year.Year","time_days")),results_prime,by=c("Year.Year","time_days"))

summary(results_primo)









results_prime <- results_primo %>%
  filter(!is.na(TCTb_std.x)) %>%
  mutate(D_mean_diff = abs(MGP_D_mean.x-MGP_D_mean.y),
         C_mean_diff = abs(MGP_C_mean.x-MGP_C_mean.y),
         L_mean_diff = abs(L_mean.x-L_mean.y),
         TCTb_mean_diff = abs(TCTb_mean.x-TCTb_mean.y),
         TCTg_mean_diff = abs(TCTg_mean.x-TCTg_mean.y),
         TCTw_mean_diff = abs(TCTw_mean.x-TCTw_mean.y),
         
  )

br <- ggplot(data = results_prime, aes(x=SpatOverlapPC,y=)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.6,color="coral3") +
  scale_x_continuous(limits=c(0,.75)) +
  scale_y_continuous() +
  theme_classic()

gr <- ggplot(data = results_prime, aes(x=SpatOverlapPC,y=TCTg_mean_diff)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.6,color="green") +
  scale_x_continuous(limits=c(0,.75)) +
  scale_y_continuous() +
  theme_classic()

wt <- ggplot(data = results_prime, aes(x=SpatOverlapPC,y=TCTw_mean_diff)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.6,color="deepskyblue3") +
  scale_x_continuous(limits=c(0,.75)) +
  scale_y_continuous() +
  theme_classic()



figure <- ggarrange(br, gr, wt,
                    
                    labels = c("Brightness", "Greenness", "Wetness"),
                    ncol = 3, nrow = 1)

figure







