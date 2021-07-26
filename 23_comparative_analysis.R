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
         time_days = decDay,
         tower = "EMS")

results_EMS <- merge(hfm,results_mutated,by=c("Year.Year","time_days"))


#============= NEON
library(tidyverse)

setwd("/Users/benjaminglass/Downloads")
NEONmaster <- as.data.frame(read.csv(file="neon_hr.csv",header=TRUE,stringsAsFactors=FALSE))

#mutate resultsNEON

NEONmaster_mutated <- NEONmaster %>%
  mutate(Year.Year = year,
         month.Month = mon,
         time_days = dec.day,
         tower = "NEON")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
results0712 <- as.data.frame(read.csv("NEON_results_0720_weightedmean.csv"))

summary(results0712)

results_mutated <- results0712 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay)

results_NEON <- merge(NEONmaster_mutated,results_mutated,by=c("Year.Year","time_days"))




summary(results_EMS)
summary(results_NEON)



##### comp analysis--------


########## ===================== DECIDIOUS 



results_prime <- results_EMS %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

EMS_D <- ggplot(data = results_prime, aes(x=MGP_D_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous() +
  theme_classic() +
  theme(legend.position = "none")

EMS_D


results_prime_NEON <- results_NEON %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PPFD_IN_1_1_1 > 50,"day","night"))


NEON_D <- ggplot(data = results_prime_NEON, aes(x=MGP_D_mean,y=FC,color=ToD)) +
  #facet_wrap(~ month.Month) +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic()

NEON_D


figure <- ggarrange(EMS_D, NEON_D,
                    #labels = c("EMS Tower","NEON Tower"),
                    ncol = 2, nrow = 1)

annotate_figure(figure,
                top = text_grob("CO2 Flux vs Decidious % of FFP during Maturity (May-Sept)",face="bold",size=12))






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

EMS_L <- ggplot(data = results_prime, aes(x=L_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic()


results_prime_NEON <- results_NEON %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PPFD_IN_1_1_1 > 50,"day","night"))


NEON_L <- ggplot(data = results_prime_NEON, aes(x=L_mean,y=FC,color=ToD)) +
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






########## ===================== TCT transformations




results_prime <- results_EMS %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

EMS_TCTb <- ggplot(data = results_prime, aes(x=TCTb_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic()

EMS_TCTg <- ggplot(data = results_prime, aes(x=TCTg_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic()

EMS_TCTw <- ggplot(data = results_prime, aes(x=TCTw_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  #facet_wrap(~ month.Month,ncol=4) +
  #geom_smooth() +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic()



results_prime_NEON <- results_NEON %>%
  filter(month.Month %in% (5:9)) %>%
  mutate(ToD = ifelse(PPFD_IN_1_1_1 > 50,"day","night"))


NEON_TCTb <- ggplot(data = results_prime_NEON, aes(x=TCTb_mean,y=FC,color=ToD)) +
  #facet_wrap(~ month.Month) +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() 

NEON_TCTg <- ggplot(data = results_prime_NEON, aes(x=TCTg_mean,y=FC,color=ToD)) +
  #facet_wrap(~ month.Month) +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() 

NEON_TCTw <- ggplot(data = results_prime_NEON, aes(x=TCTw_mean,y=FC,color=ToD)) +
  #facet_wrap(~ month.Month) +
  geom_point(shape=20,cex=1,alpha=.5) +
  scale_x_continuous() +
  scale_y_continuous() +
  theme_classic() 


# figure <- ggarrange(EMS_D, NEON_D, 
#                     # EMS_C,NEON_C,
#                     # EMS_L,NEON_L,
#                     # EMS_TCTb, NEON_TCTb, 
#                     # EMS_TCTg, NEON_TCTg, 
#                     # EMS_TCTw, NEON_TCTw,
#                     labels = c("EMS Tower","NEON Tower"),
#                     ncol = 2, nrow = 1)
# 
# annotate_figure(figure,
#                 top = text_grob("CO2 Flux vs avg Canopy Height of FFP during Maturity (May-Sept)",face="bold",size=16))
# 






# ============ boxplot






library(tidyr)
master_res <- merge(results_EMS,results_NEON,by=c("Year.Year","time_days"))

summary(master_res)

master_res <- filter(master_res,PAR.28m.e.6mol.m2.s > 100)

master_res <- master_res %>%
                  select(
                     c(MGP_D_mean.x,
                     MGP_D_mean.y,
                     MGP_C_mean.x,
                     MGP_C_mean.y,
                     L_mean.x,
                     L_mean.y,
                     TCTb_mean.x,
                     TCTb_mean.y,
                     TCTg_mean.x,
                     TCTg_mean.y,
                     TCTw_mean.x,
                     TCTw_mean.y)
                     ) %>%
                  mutate(
                     EMS_D_mean =MGP_D_mean.x,
                     NEON_D_mean = MGP_D_mean.y,
                     EMS_C_mean = MGP_C_mean.x,
                     NEON_C_mean = MGP_C_mean.y,
                     EMS_TCTb = TCTb_mean.x,
                     NEON_TCTb = TCTb_mean.y,
                     EMS_TCTg = TCTg_mean.x,
                     NEON_TCTg = TCTg_mean.y,
                     EMS_TCTw = TCTw_mean.x,
                     NEON_TCTw = TCTw_mean.y,
                     EMS_L_mean = L_mean.x,
                     NEON_L_mean = L_mean.y
                  )

                
                    
                  
                    
  


boxplot_vis <- function() {
        
        #DECID 
        data_long <- gather(master_res, spatial_m, perc_of_footprint, EMS_D_mean:NEON_D_mean, factor_key=TRUE)
        data_long$title <- "Decidious %"
        
        #summary(master_res)
        
        MGP_D_bp <- ggplot(data=data_long) +
          facet_grid(. ~title) +
          geom_boxplot(aes(x=spatial_m,y=perc_of_footprint,fill=spatial_m),
                       show.legend=FALSE) +
          scale_y_continuous(limits=c(.5,.85)) +
          scale_x_discrete(labels=c("EMS_D_mean" = "EMS","NEON_D_mean"="NEON")) +
          labs(y="dec. %",x="") +
          theme_bw()
          #theme(axis.text.x = element_blank())
        
        MGP_D_bp
        
        
        
        
        #CONIFER
        data_long <- gather(master_res, spatial_m, perc_of_footprint, EMS_C_mean:NEON_C_mean, factor_key=TRUE)
        data_long$title <- "Conifer %"
        
        #summary(master_res)
        
        MGP_C_bp <- ggplot(data=data_long) +
          facet_grid(. ~title) +
          geom_boxplot(aes(x=spatial_m,y=perc_of_footprint,fill=spatial_m),
                       show.legend=FALSE) +
          scale_y_continuous(limits=c(.1,.45)) +
          scale_x_discrete(labels=c("EMS_C_mean" = "EMS","NEON_C_mean"="NEON")) +
          labs(y="dec. %",x="") +
          theme_bw()
        
        MGP_C_bp
        
        
        
        
        #CHM
        data_long <- gather(master_res, spatial_m, Canopy_Height_Model, EMS_L_mean:NEON_L_mean, factor_key=TRUE)
        data_long$title <- "Canopy Height Model"
        
        #summary(master_res)
        
        CHMbp <- ggplot(data=data_long) +
          facet_grid(. ~title) +
          geom_boxplot(aes(x=spatial_m,y=Canopy_Height_Model,fill=spatial_m),
                       show.legend = FALSE,
                       ) +
          scale_y_continuous(limits=c(20,22)) +
          scale_x_discrete(labels=c("EMS_L_mean" = "EMS","NEON_L_mean"="NEON")) +
          labs(y="meters",x="") +
          theme_bw()
        
        
        CHMbp
        
        
        
        #TCTb
        data_long <- gather(master_res, spatial_m, measurement, EMS_TCTb:NEON_TCTb, factor_key=TRUE)
        data_long$title <- "TCT Brightness"
        
        TCT_b_bp <- ggplot(data=data_long) +
          facet_grid(. ~title) +
          geom_boxplot(aes(x=spatial_m,y=measurement,fill=spatial_m),
                       show.legend = FALSE) +
          scale_y_continuous(limits=c(-.1,.4)) +
          scale_x_discrete(labels=c("EMS_TCTb" = "EMS","NEON_TCTb"="NEON")) +
          labs(y="Index value",x="") +
          theme_bw()
        
        TCT_b_bp
        
        
        #TCTg
        data_long <- gather(master_res, spatial_m, measurement, EMS_TCTg:NEON_TCTg, factor_key=TRUE)
        data_long$title <- "TCT Greenness"
        
        TCT_g_bp <- ggplot(data=data_long) +
          facet_grid(. ~title) +
          geom_boxplot(aes(x=spatial_m,y=measurement,fill=spatial_m),
                       show.legend = FALSE) +
          scale_y_continuous(limits=c(-.1,.4)) +
          scale_x_discrete(labels=c("EMS_TCTg" = "EMS","NEON_TCTg"="NEON")) +
          labs(y="Index value",x="") +
          theme_bw()
        
        TCT_g_bp
        
        
        #TCTw
        data_long <- gather(master_res, spatial_m, measurement, EMS_TCTw:NEON_TCTw, factor_key=TRUE)
        data_long$title <- "TCT Wetness"
        
        TCT_w_bp <- ggplot(data=data_long) +
          facet_grid(. ~title) +
          geom_boxplot(aes(x=spatial_m,y=measurement,fill=spatial_m),
                       show.legend = FALSE) +
          scale_y_continuous(limits=c(-.1,0)) +
          scale_x_discrete(labels=c("EMS_TCTw" = "EMS","NEON_TCTw"="NEON")) +
          labs(y="Index value",x="") +
          theme_bw()
        
        TCT_w_bp
        
        
        
        figure <- ggarrange(MGP_D_bp,MGP_C_bp,CHMbp,TCT_b_bp,TCT_g_bp,TCT_w_bp,
                            # labels = c("% Decidious in Megaplot",
                            #            "% Coniferous in Megaplot",
                            #            "Canopy Height Model",
                            #            "TCTb",
                            #            "TCTg",
                            #            "TCTw"),
                            ncol = 3, nrow = 2)
        
        figure
        
        
        
        # figure <- ggarrange(MGPbp,CHMbp,TCTbp,
        #                     labels = c("Megaplot","Canopy Height Model","TCT"),
        #                     ncol = 3, nrow = 1)
        # 
        #annotate_figure(figure, 
                        # top = text_grob("Value distribution of EMS (red) and NEON (teal) footprints for input spatial data",
                        #                 face="bold",
                        #                 size=12))

}


boxplot_vis()









##### binning examples -------


#=EMS
setwd("/Users/benjaminglass/Downloads")
hfm <- readRDS("hfmaster_0713.RDS")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
results0712 <- as.data.frame(read.csv("EMS_results_0719_weightedmean.csv"))


results_mutated <- results0712 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay,
         tower = "EMS")

results_EMS <- merge(hfm,results_mutated,by=c("Year.Year","time_days"))


#= NEON
library(tidyverse)

setwd("/Users/benjaminglass/Downloads")
NEONmaster <- as.data.frame(read.csv(file="neon_hr.csv",header=TRUE,stringsAsFactors=FALSE))

#mutate resultsNEON

NEONmaster_mutated <- NEONmaster %>%
  mutate(Year.Year = year,
         month.Month = mon,
         time_days = dec.day,
         tower = "NEON")

setwd("/Users/benjaminglass/Desktop/HF21/00_Datasets")
results0712 <- as.data.frame(read.csv("NEON_results_0720_weightedmean.csv"))

summary(results0712)

results_mutated <- results0712 %>%
  mutate(Year.Year = paste0("20",year),
         time_days = decDay)

results_NEON <- merge(NEONmaster_mutated,results_mutated,by=c("Year.Year","time_days"))




bin_results_EMS <- results_EMS %>%
  mutate(tag = case_when(
    PAR.28m.e.6mol.m2.s < 0 ~ "<0",
    PAR.28m.e.6mol.m2.s >= 0 & PAR.28m.e.6mol.m2.s < 50 ~ "0-50",
    PAR.28m.e.6mol.m2.s >= 50 & PAR.28m.e.6mol.m2.s < 100 ~ "50-100",
    PAR.28m.e.6mol.m2.s >= 100 & PAR.28m.e.6mol.m2.s < 150 ~ "100-150",
    PAR.28m.e.6mol.m2.s >= 150 & PAR.28m.e.6mol.m2.s < 200 ~ "150-200",
    PAR.28m.e.6mol.m2.s >= 200 & PAR.28m.e.6mol.m2.s < 250 ~ "200-250",
    PAR.28m.e.6mol.m2.s >= 250 & PAR.28m.e.6mol.m2.s < 300 ~ "250-300",
    PAR.28m.e.6mol.m2.s >= 300 & PAR.28m.e.6mol.m2.s < 350 ~ "300-350",
    PAR.28m.e.6mol.m2.s >= 350 & PAR.28m.e.6mol.m2.s < 400 ~ "350-400",
    PAR.28m.e.6mol.m2.s >= 400 & PAR.28m.e.6mol.m2.s < 450 ~ "400-450",
    PAR.28m.e.6mol.m2.s >= 450 & PAR.28m.e.6mol.m2.s < 500 ~ "450-500",
    PAR.28m.e.6mol.m2.s >= 500 & PAR.28m.e.6mol.m2.s < 550 ~ "500-550",
    PAR.28m.e.6mol.m2.s >= 550 & PAR.28m.e.6mol.m2.s < 600 ~ "550-600",
    PAR.28m.e.6mol.m2.s >= 600 & PAR.28m.e.6mol.m2.s < 650 ~ "600-650",
    PAR.28m.e.6mol.m2.s >= 650 & PAR.28m.e.6mol.m2.s < 700 ~ "650-700",
    PAR.28m.e.6mol.m2.s >= 700 & PAR.28m.e.6mol.m2.s < 750 ~ "700-750",
    PAR.28m.e.6mol.m2.s >= 750 & PAR.28m.e.6mol.m2.s < 800 ~ "750-800",
    PAR.28m.e.6mol.m2.s >= 800 & PAR.28m.e.6mol.m2.s < 850 ~ "800-850",
    PAR.28m.e.6mol.m2.s >= 850 & PAR.28m.e.6mol.m2.s < 900 ~ "850-900",
    PAR.28m.e.6mol.m2.s >= 900 & PAR.28m.e.6mol.m2.s < 950 ~ "900-950",
    PAR.28m.e.6mol.m2.s >= 950 & PAR.28m.e.6mol.m2.s < 1000 ~ "950-1000",
    PAR.28m.e.6mol.m2.s >= 1000 & PAR.28m.e.6mol.m2.s < 2000 ~ "1000-2000",
    PAR.28m.e.6mol.m2.s >= 2000 & PAR.28m.e.6mol.m2.s < 3000 ~ "2000-3000"
  ))



PAR_breaks <- c("<0","0-50","50-100",
                "100-150","150-200","200-250",
                "250-300","300-350","350-400",
                "400-450","450-500","500-550",
                "550-600","600-650","650-700",
                "700-750","750-800","800-900",
                "900-950","1000-2000","2000-3000")

bin_results_EMS$tag <- factor(bin_results_EMS$tag,
                          levels=PAR_breaks,
                          ordered=FALSE)

summary(bin_results_EMS$tag)

print(bin_results_EMS)

# group_tags <- cut(results_EMS$PAR.28m.e.6mol.m2.s, 
#                   breaks=PAR_breaks, 
#                   include.lowest=TRUE, 
#                   right=FALSE)
# # inspect bins
# summary(group_tags)


#summary(bin_results_EMS$obs.FCO2.e.6mol.m2.s)

EMS_norm <- bin_results_EMS %>%
  filter(!is.na(obs.FCO2.e.6mol.m2.s)) %>%
  group_by(tag) %>%
  summarise(mean_FCO2 = mean(obs.FCO2.e.6mol.m2.s))

summary(EMS_norm)

ggplot(data=bin_results_EMS,aes(x=tag,y=obs.FCO2.e.6mol.m2.s)) +
  geom_boxplot() +
  theme_classic()


ggplot(data=bin_results_EMS,
       aes(x=L_mean,y=obs.FCO2.e.6mol.m2.s)) +
  facet_wrap(~ tag) +
  geom_point(shape=20,cex=1,alpha=.5) +
  theme_classic()



colnames(results_EMS)


results_EMS$tag <- cut(results_EMS$PAR.28m.e.6mol.m2.s, 12)

ggplot(data=results_EMS,
       aes(x=L_mean,y=obs.FCO2.e.6mol.m2.s)) +
  facet_wrap(~ tag) +
  geom_point(shape=20,cex=1,alpha=.5) +
  theme_classic()




bin_results_EMS <- results_EMS %>%
  mutate(tag = case_when(
    month.Month >= 12 | month.Month < 4 ~ "winter",
    month.Month >= 4 & month.Month < 7 ~ "spring",
    month.Month >= 7 & month.Month < 10 ~ "summer",
    month.Month >= 10 & month.Month < 12 ~ "fall")) %>%
  mutate(ToD = ifelse(PAR.28m.e.6mol.m2.s > 50,"day","night"))

ggplot(data=bin_results_EMS,
       aes(x=MGP_D_mean,y=obs.FCO2.e.6mol.m2.s,color=ToD)) +
  facet_wrap(~ tag) +
  geom_point(shape=20,cex=1,alpha=.5) +
  theme_classic()




