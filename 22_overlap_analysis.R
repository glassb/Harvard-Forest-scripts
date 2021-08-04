library(ggpubr)
library(tidyverse)



setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
results <- as.data.frame(read.csv("overlapScript_0730_75influence_extrastats.csv"))

summary(results)

results_points <- results %>%
  dplyr::filter(SpatOverlapPC > 0) %>%
  dplyr::filter(PAR.28m.e.6mol.m2.s > 50) %>%
  mutate(flux_diff_perc = ((obs.FCO2.e.6mol.m2.s-FC)),
         flux_diff_abs = abs(obs.FCO2.e.6mol.m2.s-FC),
         total_overlap = ifelse(Poverlap_NEON> .5,"true","false"),
         SpatOverlapPC_perc = SpatOverlapPC*100)

results_prime <- results %>%
  dplyr::filter(SpatOverlapPC > 0) %>%
  dplyr::filter(PAR.28m.e.6mol.m2.s > 50) %>%
  mutate(flux_diff_perc = ((obs.FCO2.e.6mol.m2.s-FC)),
         flux_diff_abs = abs(obs.FCO2.e.6mol.m2.s-FC),
         total_overlap = ifelse(Poverlap_NEON> .5,"true","false"),
         SpatOverlapPC_perc = SpatOverlapPC*100) %>%
  mutate(tag = case_when(
          SpatOverlapPC_perc < 5 ~ "00 <5",
          SpatOverlapPC_perc >= 5 & SpatOverlapPC_perc < 10 ~ "01 [5,10)",
          SpatOverlapPC_perc >= 10 & SpatOverlapPC_perc < 15 ~ "02 [10,15)",
          SpatOverlapPC_perc >= 15 & SpatOverlapPC_perc < 20 ~ "03 [15,20)",
          SpatOverlapPC_perc >= 20 & SpatOverlapPC_perc < 25 ~ "04 [20,25)",
          SpatOverlapPC_perc >= 25 & SpatOverlapPC_perc < 30 ~ "05 [25,30)",
          SpatOverlapPC_perc >= 30 & SpatOverlapPC_perc < 35 ~ "06 [30,35)",
          SpatOverlapPC_perc >= 35 & SpatOverlapPC_perc < 40 ~ "07 [35,40)",
          SpatOverlapPC_perc >= 40 & SpatOverlapPC_perc < 45 ~ "08 [40,45)",
          SpatOverlapPC_perc >= 45 & SpatOverlapPC_perc < 50 ~ "09 [45,50)",
          SpatOverlapPC_perc >= 50 & SpatOverlapPC_perc < 55 ~ "10 [50,55)",
          SpatOverlapPC_perc >= 55 & SpatOverlapPC_perc < 60 ~ "11 [55,60)",
          SpatOverlapPC_perc >= 60 & SpatOverlapPC_perc < 65 ~ "12 [60,65)",
         ))

results_prime

results_prime <- results_prime[complete.cases(results_prime[,c("SpatOverlapPC_perc", "flux_diff_abs")]), ]

results_prime <- results_prime %>%
  group_by(tag) %>%
  summarise(PO_mean = mean(SpatOverlapPC_perc),
            FD_mean = mean(flux_diff_abs))
         
         #/obs.FCO2.e.6mol.m2.s*100

print(results_prime)

ggplot(data = results_prime, aes(x=SpatOverlapPC_perc,y=flux_diff_abs)) +
  #facet_wrap(~ month.Month,ncol=4) +
  geom_point(shape=20,cex=3,alpha=.5) +
  scale_x_continuous(limits=c(0,75)) +
  scale_y_continuous(limits=c(0,20),breaks=seq(-25,25,5)) +
  #geom_smooth() +
  xlab("Percentage Overlap (%)") +
  ylab("flux value difference (abs)") +
  theme_classic() +
  #labs(title="Overlap of Tower footprints vs. abs Difference in Flux Daytime measurement") +
  theme(text=element_text(size=30))


ggplot() +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_point(data=results_points,aes(x=SpatOverlapPC_perc,y=flux_diff_abs),fill="gray") +
  geom_boxplot(data = results_prime,aes(x=PO_mean,y=FD_mean),shape=20,cex=10,alpha=.5) +
  geom_line(data = results_prime,aes(x=PO_mean,y=FD_mean),cex=2) +
  scale_x_continuous(limits=c(0,75)) +
  scale_y_continuous(limits=c(0,20),breaks=seq(-25,25,5)) +
  #geom_smooth() +
  xlab("Percentage Overlap (%)") +
  ylab("flux value difference (abs)") +
  theme_classic() +
  #labs(title="Overlap of Tower footprints vs. abs Difference in Flux Daytime measurement") +
  theme(text=element_text(size=30))

ggplot(results_prime) +
  geom_boxplot(aes(x=tag,y=flux_diff_abs),
             show.legend=FALSE) +
  scale_y_continuous(limits=c(0,10)) +
  theme_classic()





hist(results_prime$SpatOverlapPC)
summary(results_prime$SpatOverlapPC)
hist(results_prime$flux_diff_perc,breaks=100)




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







