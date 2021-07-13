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
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"Day","Night"))


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
  scale_y_continuous()




