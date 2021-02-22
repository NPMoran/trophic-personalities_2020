# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#### Guldborgsund 3. Behavioural Correlations ####

Sys.setenv(LANG = "en")
library(dplyr)



## G.3.1. Correlations between behavioural variables ----
GULD_phenotypes <- read.csv('~/trophicpersonalities_A/Data_Guldborgsund/GULD_phenotypes.csv', strip.white = TRUE)
# in: avespeed_tot
#     avespeed_mob
#     aveacceler
#     propmoving
#     dist        
#     frozenevents
#     timefrozen_tot
#     centrescore
#     centretime50        
#     centretime75
#     centretime100 
#     emergelat.bin
#     endpointlat.bin     
#     endpointspeed     
#     refugereturnlat


### 4.2. Spearman Correlations ----

#  Visualising correlations
pairs(GULD_phenotypes[,9:23])

cor01_02 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$avespeed_mob.ACTT1, method = 'spearman')     
cor01_03 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$aveacceler.ACTT1, method = 'spearman')       
cor01_04 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$propmoving.ACTT1, method = 'spearman')   
cor01_05 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$dist.ACTT1, method = 'spearman')             
cor01_06 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$frozenevents.ACTT1, method = 'spearman')
cor01_07 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$timefrozen_tot.ACTT1, method = 'spearman')
cor01_08 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$centrescore.ACTT1, method = 'spearman')    
cor01_09 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$centretime50.ACTT1, method = 'spearman')  
cor01_10 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$centretime75.ACTT1, method = 'spearman') 
cor01_11 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor01_12 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor01_13 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor01_14 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor01_15 <- cor.test(GULD_phenotypes$avespeed_tot.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor02_03 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$aveacceler.ACTT1, method = 'spearman')       
cor02_04 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$propmoving.ACTT1, method = 'spearman')   
cor02_05 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$dist.ACTT1, method = 'spearman')             
cor02_06 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$frozenevents.ACTT1, method = 'spearman')
cor02_07 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$timefrozen_tot.ACTT1, method = 'spearman')
cor02_08 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$centrescore.ACTT1, method = 'spearman')    
cor02_09 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$centretime50.ACTT1, method = 'spearman')  
cor02_10 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$centretime75.ACTT1, method = 'spearman') 
cor02_11 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor02_12 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor02_13 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor02_14 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor02_15 <- cor.test(GULD_phenotypes$avespeed_mob.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor03_04 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$propmoving.ACTT1, method = 'spearman')   
cor03_05 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$dist.ACTT1, method = 'spearman')             
cor03_06 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$frozenevents.ACTT1, method = 'spearman')
cor03_07 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$timefrozen_tot.ACTT1, method = 'spearman')
cor03_08 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$centrescore.ACTT1, method = 'spearman')    
cor03_09 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$centretime50.ACTT1, method = 'spearman')  
cor03_10 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$centretime75.ACTT1, method = 'spearman') 
cor03_11 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor03_12 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor03_13 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor03_14 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor03_15 <- cor.test(GULD_phenotypes$aveacceler.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor04_05 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$dist.ACTT1, method = 'spearman')             
cor04_06 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$frozenevents.ACTT1, method = 'spearman')
cor04_07 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$timefrozen_tot.ACTT1, method = 'spearman')
cor04_08 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$centrescore.ACTT1, method = 'spearman')    
cor04_09 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$centretime50.ACTT1, method = 'spearman')  
cor04_10 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$centretime75.ACTT1, method = 'spearman') 
cor04_11 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor04_12 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor04_13 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor04_14 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor04_15 <- cor.test(GULD_phenotypes$propmoving.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor05_06 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$frozenevents.ACTT1, method = 'spearman')
cor05_07 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$timefrozen_tot.ACTT1, method = 'spearman')
cor05_08 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$centrescore.ACTT1, method = 'spearman')    
cor05_09 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$centretime50.ACTT1, method = 'spearman')  
cor05_10 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$centretime75.ACTT1, method = 'spearman') 
cor05_11 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor05_12 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor05_13 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor05_14 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor05_15 <- cor.test(GULD_phenotypes$dist.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor06_07 <- cor.test(GULD_phenotypes$frozenevents.ACTT1, GULD_phenotypes$timefrozen_tot.ACTT1, method = 'spearman')
cor06_08 <- cor.test(GULD_phenotypes$frozenevents.ACTT1, GULD_phenotypes$centrescore.ACTT1, method = 'spearman')    
cor06_09 <- cor.test(GULD_phenotypes$frozenevents.ACTT1, GULD_phenotypes$centretime50.ACTT1, method = 'spearman')  
cor06_10 <- cor.test(GULD_phenotypes$frozenevents.ACTT1, GULD_phenotypes$centretime75.ACTT1, method = 'spearman') 
cor06_11 <- cor.test(GULD_phenotypes$frozenevents.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor06_12 <- cor.test(GULD_phenotypes$frozenevents.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor06_13 <- cor.test(GULD_phenotypes$frozenevents.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor06_14 <- cor.test(GULD_phenotypes$frozenevents.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor06_15 <- cor.test(GULD_phenotypes$frozenevents.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor07_08 <- cor.test(GULD_phenotypes$timefrozen_tot.ACTT1, GULD_phenotypes$centrescore.ACTT1, method = 'spearman')    
cor07_09 <- cor.test(GULD_phenotypes$timefrozen_tot.ACTT1, GULD_phenotypes$centretime50.ACTT1, method = 'spearman')  
cor07_10 <- cor.test(GULD_phenotypes$timefrozen_tot.ACTT1, GULD_phenotypes$centretime75.ACTT1, method = 'spearman') 
cor07_11 <- cor.test(GULD_phenotypes$timefrozen_tot.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor07_12 <- cor.test(GULD_phenotypes$timefrozen_tot.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor07_13 <- cor.test(GULD_phenotypes$timefrozen_tot.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor07_14 <- cor.test(GULD_phenotypes$timefrozen_tot.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor07_15 <- cor.test(GULD_phenotypes$timefrozen_tot.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor08_09 <- cor.test(GULD_phenotypes$centrescore.ACTT1, GULD_phenotypes$centretime50.ACTT1, method = 'spearman')  
cor08_10 <- cor.test(GULD_phenotypes$centrescore.ACTT1, GULD_phenotypes$centretime75.ACTT1, method = 'spearman') 
cor08_11 <- cor.test(GULD_phenotypes$centrescore.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor08_12 <- cor.test(GULD_phenotypes$centrescore.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor08_13 <- cor.test(GULD_phenotypes$centrescore.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor08_14 <- cor.test(GULD_phenotypes$centrescore.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor08_15 <- cor.test(GULD_phenotypes$centrescore.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor09_10 <- cor.test(GULD_phenotypes$centretime50.ACTT1, GULD_phenotypes$centretime75.ACTT1, method = 'spearman') 
cor09_11 <- cor.test(GULD_phenotypes$centretime50.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor09_12 <- cor.test(GULD_phenotypes$centretime50.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor09_13 <- cor.test(GULD_phenotypes$centretime50.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor09_14 <- cor.test(GULD_phenotypes$centretime50.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor09_15 <- cor.test(GULD_phenotypes$centretime50.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor10_11 <- cor.test(GULD_phenotypes$centretime75.ACTT1, GULD_phenotypes$centretime100.ACTT1, method = 'spearman')
cor10_12 <- cor.test(GULD_phenotypes$centretime75.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor10_13 <- cor.test(GULD_phenotypes$centretime75.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor10_14 <- cor.test(GULD_phenotypes$centretime75.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor10_15 <- cor.test(GULD_phenotypes$centretime75.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor11_12 <- cor.test(GULD_phenotypes$centretime100.ACTT1, GULD_phenotypes$emergelat.bin.EXPLT1, method = 'spearman')
cor11_13 <- cor.test(GULD_phenotypes$centretime100.ACTT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor11_14 <- cor.test(GULD_phenotypes$centretime100.ACTT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor11_15 <- cor.test(GULD_phenotypes$centretime100.ACTT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor12_13 <- cor.test(GULD_phenotypes$emergelat.bin.EXPLT1, GULD_phenotypes$endpointlat.bin.EXPLT1, method = 'spearman')
cor12_14 <- cor.test(GULD_phenotypes$emergelat.bin.EXPLT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor12_15 <- cor.test(GULD_phenotypes$emergelat.bin.EXPLT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor13_14 <- cor.test(GULD_phenotypes$endpointlat.bin.EXPLT1, GULD_phenotypes$endpointspeed.EXPLT1, method = 'spearman')
cor13_15 <- cor.test(GULD_phenotypes$endpointlat.bin.EXPLT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')

cor14_15 <- cor.test(GULD_phenotypes$endpointspeed.EXPLT1, GULD_phenotypes$refugereturnlat.EXPLT1, method = 'spearman')


GULD_behav_corr <- NULL
GULD_behav_corr$comparison <- c(cor01_02$data.name,cor01_03$data.name,cor01_04$data.name,cor01_05$data.name,cor01_06$data.name,cor01_07$data.name,cor01_08$data.name,cor01_09$data.name,cor01_10$data.name,cor01_11$data.name,cor01_12$data.name,cor01_13$data.name,cor01_14$data.name,cor01_15$data.name,cor02_03$data.name,cor02_04$data.name,cor02_05$data.name,cor02_06$data.name,cor02_07$data.name,cor02_08$data.name,cor02_09$data.name,cor02_10$data.name,cor02_11$data.name,cor02_12$data.name,cor02_13$data.name,cor02_14$data.name,cor02_15$data.name,cor03_04$data.name,cor03_05$data.name,cor03_06$data.name,cor03_07$data.name,cor03_08$data.name,cor03_09$data.name,cor03_10$data.name,cor03_11$data.name,cor03_12$data.name,cor03_13$data.name,cor03_14$data.name,cor03_15$data.name,cor04_05$data.name,cor04_06$data.name,cor04_07$data.name,cor04_08$data.name,cor04_09$data.name,cor04_10$data.name,cor04_11$data.name,cor04_12$data.name,cor04_13$data.name,cor04_14$data.name,cor04_15$data.name,cor05_06$data.name,cor05_07$data.name,cor05_08$data.name,cor05_09$data.name,cor05_10$data.name,cor05_11$data.name,cor05_12$data.name,cor05_13$data.name,cor05_14$data.name,cor05_15$data.name,cor06_07$data.name,cor06_08$data.name,cor06_09$data.name,cor06_10$data.name,cor06_11$data.name,cor06_12$data.name,cor06_13$data.name,cor06_14$data.name,cor06_15$data.name,cor07_08$data.name,cor07_09$data.name,cor07_10$data.name,cor07_11$data.name,cor07_12$data.name,cor07_13$data.name,cor07_14$data.name,cor07_15$data.name,cor08_09$data.name,cor08_10$data.name,cor08_11$data.name,cor08_12$data.name,cor08_13$data.name,cor08_14$data.name,cor08_15$data.name,cor09_10$data.name,cor09_11$data.name,cor09_12$data.name,cor09_13$data.name,cor09_14$data.name,cor09_15$data.name,cor10_11$data.name,cor10_12$data.name,cor10_13$data.name,cor10_14$data.name,cor10_15$data.name,cor11_12$data.name,cor11_13$data.name,cor11_14$data.name,cor11_15$data.name,cor12_13$data.name,cor12_14$data.name,cor12_15$data.name,cor13_14$data.name,cor13_15$data.name,cor14_15$data.name)
GULD_behav_corr <- as.data.frame(GULD_behav_corr)
GULD_behav_corr$method <-'Spearmans rank correlation rho' 
GULD_behav_corr$rho <- c(cor01_02$estimate,cor01_03$estimate,cor01_04$estimate,cor01_05$estimate,cor01_06$estimate,cor01_07$estimate,cor01_08$estimate,cor01_09$estimate,cor01_10$estimate,cor01_11$estimate,cor01_12$estimate,cor01_13$estimate,cor01_14$estimate,cor01_15$estimate,cor02_03$estimate,cor02_04$estimate,cor02_05$estimate,cor02_06$estimate,cor02_07$estimate,cor02_08$estimate,cor02_09$estimate,cor02_10$estimate,cor02_11$estimate,cor02_12$estimate,cor02_13$estimate,cor02_14$estimate,cor02_15$estimate,cor03_04$estimate,cor03_05$estimate,cor03_06$estimate,cor03_07$estimate,cor03_08$estimate,cor03_09$estimate,cor03_10$estimate,cor03_11$estimate,cor03_12$estimate,cor03_13$estimate,cor03_14$estimate,cor03_15$estimate,cor04_05$estimate,cor04_06$estimate,cor04_07$estimate,cor04_08$estimate,cor04_09$estimate,cor04_10$estimate,cor04_11$estimate,cor04_12$estimate,cor04_13$estimate,cor04_14$estimate,cor04_15$estimate,cor05_06$estimate,cor05_07$estimate,cor05_08$estimate,cor05_09$estimate,cor05_10$estimate,cor05_11$estimate,cor05_12$estimate,cor05_13$estimate,cor05_14$estimate,cor05_15$estimate,cor06_07$estimate,cor06_08$estimate,cor06_09$estimate,cor06_10$estimate,cor06_11$estimate,cor06_12$estimate,cor06_13$estimate,cor06_14$estimate,cor06_15$estimate,cor07_08$estimate,cor07_09$estimate,cor07_10$estimate,cor07_11$estimate,cor07_12$estimate,cor07_13$estimate,cor07_14$estimate,cor07_15$estimate,cor08_09$estimate,cor08_10$estimate,cor08_11$estimate,cor08_12$estimate,cor08_13$estimate,cor08_14$estimate,cor08_15$estimate,cor09_10$estimate,cor09_11$estimate,cor09_12$estimate,cor09_13$estimate,cor09_14$estimate,cor09_15$estimate,cor10_11$estimate,cor10_12$estimate,cor10_13$estimate,cor10_14$estimate,cor10_15$estimate,cor11_12$estimate,cor11_13$estimate,cor11_14$estimate,cor11_15$estimate,cor12_13$estimate,cor12_14$estimate,cor12_15$estimate,cor13_14$estimate,cor13_15$estimate,cor14_15$estimate)
GULD_behav_corr$p.value <- c(cor01_02$p.value,cor01_03$p.value,cor01_04$p.value,cor01_05$p.value,cor01_06$p.value,cor01_07$p.value,cor01_08$p.value,cor01_09$p.value,cor01_10$p.value,cor01_11$p.value,cor01_12$p.value,cor01_13$p.value,cor01_14$p.value,cor01_15$p.value,cor02_03$p.value,cor02_04$p.value,cor02_05$p.value,cor02_06$p.value,cor02_07$p.value,cor02_08$p.value,cor02_09$p.value,cor02_10$p.value,cor02_11$p.value,cor02_12$p.value,cor02_13$p.value,cor02_14$p.value,cor02_15$p.value,cor03_04$p.value,cor03_05$p.value,cor03_06$p.value,cor03_07$p.value,cor03_08$p.value,cor03_09$p.value,cor03_10$p.value,cor03_11$p.value,cor03_12$p.value,cor03_13$p.value,cor03_14$p.value,cor03_15$p.value,cor04_05$p.value,cor04_06$p.value,cor04_07$p.value,cor04_08$p.value,cor04_09$p.value,cor04_10$p.value,cor04_11$p.value,cor04_12$p.value,cor04_13$p.value,cor04_14$p.value,cor04_15$p.value,cor05_06$p.value,cor05_07$p.value,cor05_08$p.value,cor05_09$p.value,cor05_10$p.value,cor05_11$p.value,cor05_12$p.value,cor05_13$p.value,cor05_14$p.value,cor05_15$p.value,cor06_07$p.value,cor06_08$p.value,cor06_09$p.value,cor06_10$p.value,cor06_11$p.value,cor06_12$p.value,cor06_13$p.value,cor06_14$p.value,cor06_15$p.value,cor07_08$p.value,cor07_09$p.value,cor07_10$p.value,cor07_11$p.value,cor07_12$p.value,cor07_13$p.value,cor07_14$p.value,cor07_15$p.value,cor08_09$p.value,cor08_10$p.value,cor08_11$p.value,cor08_12$p.value,cor08_13$p.value,cor08_14$p.value,cor08_15$p.value,cor09_10$p.value,cor09_11$p.value,cor09_12$p.value,cor09_13$p.value,cor09_14$p.value,cor09_15$p.value,cor10_11$p.value,cor10_12$p.value,cor10_13$p.value,cor10_14$p.value,cor10_15$p.value,cor11_12$p.value,cor11_13$p.value,cor11_14$p.value,cor11_15$p.value,cor12_13$p.value,cor12_14$p.value,cor12_15$p.value,cor13_14$p.value,cor13_15$p.value,cor14_15$p.value)

write.csv(GULD_behav_corr, '~/trophicpersonalities_A/Output_Guldborgsund/GULD.behaviouralcorrelations.csv')


#Summary:
# Strong correlations between all ACT variables, 
#
# More active individual also are to be more likely to emerge and reach endpoint in EXPL trial

