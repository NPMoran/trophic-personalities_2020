# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#### Guldborgsund SIA 3. SIA Variance analysis #### 


Sys.setenv(LANG = "en")

#Loading required packages- 
library(dplyr); library(lme4); library(lmerTest); library(car); library(data.table)


#Uploading phenotypic data calculated in Guldborgsund_2_VarianceAnalysis
GULD_phenotypes <- read.csv('~/trophic-personalities_2020/Data_Guldborgsund/GULD_phenotypes.csv', strip.white = TRUE)

#Uploading stable isotope data processed 
GULD_processed.fins <- read.csv('~/trophic-personalities_2020/Data_GuldborgsundSIA/GULD_processed.fins.csv', strip.white = TRUE)

GULD_processed.fishfull <- merge(GULD_processed.fins, GULD_phenotypes, by = 'FishID', all.x = TRUE)
labels(GULD_processed.fishfull)


## SIA.4.1. Raw correlations between fish traits and d15N d13C ----
GULD_processed.fishmeans <- setDT(GULD_processed.fins)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                               d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                        by = .(FishID)]
GULD_processed.fishmeans <- merge(GULD_processed.fishmeans, GULD_phenotypes, by = 'FishID', all.x = TRUE)

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$TL, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$TL, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$ConditionFactor, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$ConditionFactor, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$InfectionScore, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$InfectionScore, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$avespeed_tot.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$avespeed_tot.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$avespeed_tot.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$avespeed_tot.ACTT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$avespeed_tot.MEAN,  method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$avespeed_tot.COEF, COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$avespeed_mob.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$avespeed_mob.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$avespeed_mob.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$avespeed_mob.ACTT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$avespeed_mob.MEAN,  method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$avespeed_mob.COEF, COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$aveacceler.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$aveacceler.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$aveacceler.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$aveacceler.MEAN, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$aveacceler.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$aveacceler.COEF, COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$propmoving.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$propmoving.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$propmoving.invlog.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$propmoving.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$propmoving.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$propmoving.invlog.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$dist.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$dist.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$dist.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$dist.ACTT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$dist.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$dist.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$frozenevents.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$frozenevents.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$frozenevents.sqrt.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$frozenevents.ACTT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$frozenevents.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$frozenevents.sqrt.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$timefrozen_tot.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$timefrozen_tot.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$timefrozen_tot.sqrt.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$timefrozen_tot.ACTT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$timefrozen_tot.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$timefrozen_tot.sqrt.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centrescore.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centrescore.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centrescore.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centrescore.ACTT1, method = 'spearman') #marginally significant
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centrescore.MEAN, method = 'spearman') #sig. effect
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centrescore.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centretime50.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centretime50.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centretime50.sqrt.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centretime50.ACTT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centretime50.MEAN, method = 'spearman') #sig. effect
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centretime50.sqrt.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centretime75.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centretime75.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centretime75.sqrt.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centretime75.ACTT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centretime75.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centretime75.sqrt.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centretime100.ACTT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centretime100.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$centretime100.sqrt.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centretime100.ACTT1, method = 'spearman') #marginally significant
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centretime100.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$centretime100.sqrt.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$emergelat.bin.EXPLT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$emergelat.bin.MEAN  , method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$emergelat.bin.COEF  , method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$emergelat.bin.EXPLT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$emergelat.bin.MEAN  , method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$emergelat.bin.COEF  , method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$endpointlat.bin.EXPLT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$endpointlat.bin.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$endpointlat.bin.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$endpointlat.bin.EXPLT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$endpointlat.bin.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$endpointlat.bin.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$endpointspeed.EXPLT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$endpointspeed.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$endpointspeed.ln.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$endpointspeed.EXPLT1, method = 'spearman') #marginally significant
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$endpointspeed.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$endpointspeed.ln.COEF, method = 'spearman')

cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$refugereturnlat.EXPLT1, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$refugereturnlat.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d15N_mean, GULD_processed.fishmeans$refugereturnlat.ln.COEF, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$refugereturnlat.EXPLT1, method = 'spearman') 
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$refugereturnlat.MEAN, method = 'spearman')
cor.test(GULD_processed.fishmeans$d13C_mean, GULD_processed.fishmeans$refugereturnlat.ln.COEF, method = 'spearman')





## SIA.4.2. Fixed effect models of d15N d13C ----
#Non behavioural variables:
#TL.C
#ConditionFactor.C
#InfectionScore.C 

##Z-transformation/scaling of continuous fixed effects
GULD_processed.fishfull$TL.C <- scale(GULD_processed.fishfull$TL)  
GULD_processed.fishfull$ConditionFactor.C <- scale(GULD_processed.fishfull$ConditionFactor)  
GULD_processed.fishfull$InfectionScore.C <- scale(GULD_processed.fishfull$InfectionScore)  

#d15N
GULD_N.nonbehav.mod <- lmer(d15N ~ TL.C + ConditionFactor.C + InfectionScore.C + (1|FishID), data=GULD_processed.fishfull)
Anova(GULD_N.nonbehav.mod) #sig effect of ConditionFactor.C
summary(GULD_N.nonbehav.mod) 
confint(GULD_N.nonbehav.mod) #small positive effect that just overlaps zero
plot(GULD_N.nonbehav.mod) 

#d13C
GULD_C.nonbehav.mod <- lmer(d13C ~ TL.C + ConditionFactor.C + InfectionScore.C + (1|FishID), data=GULD_processed.fishfull)
Anova(GULD_C.nonbehav.mod) #no sig effects
summary(GULD_C.nonbehav.mod)
confint(GULD_C.nonbehav.mod)
plot(GULD_C.nonbehav.mod)
#Summary: only a marginally significant positive effect of condition factor on d15N


#Behavioural variables (need to cut back, many are strongly correlated with each other)
#avespeed_tot: (mm/s) the average speed of the individual accross the full trial period
#avespeed_tot.ACTT1
#avespeed_tot.MEAN
#avespeed_tot.COEF
GULD_N.avespeed_tot.ACTT1.mod <- lmer(d15N ~ avespeed_tot.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.avespeed_tot.ACTT1.mod)
confint(GULD_N.avespeed_tot.ACTT1.mod)
plot(GULD_N.avespeed_tot.ACTT1.mod)

GULD_N.avespeed_tot.MEAN.mod <- lmer(d15N ~ avespeed_tot.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.avespeed_tot.MEAN.mod)
confint(GULD_N.avespeed_tot.MEAN.mod)
plot(GULD_N.avespeed_tot.MEAN.mod)

GULD_N.avespeed_tot.COEF.mod <- lmer(d15N ~ avespeed_tot.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.avespeed_tot.COEF.mod)
confint(GULD_N.avespeed_tot.COEF.mod)
plot(GULD_N.avespeed_tot.COEF.mod)

GULD_C.avespeed_tot.ACTT1.mod <- lmer(d13C ~ avespeed_tot.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.avespeed_tot.ACTT1.mod)
confint(GULD_C.avespeed_tot.ACTT1.mod)
plot(GULD_C.avespeed_tot.ACTT1.mod)

GULD_C.avespeed_tot.MEAN.mod <- lmer(d13C ~ avespeed_tot.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.avespeed_tot.MEAN.mod)
confint(GULD_C.avespeed_tot.MEAN.mod)
plot(GULD_C.avespeed_tot.MEAN.mod)

GULD_C.avespeed_tot.COEF.mod <- lmer(d13C ~ avespeed_tot.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.avespeed_tot.COEF.mod)
confint(GULD_C.avespeed_tot.COEF.mod)
plot(GULD_C.avespeed_tot.COEF.mod)
#Summary: No effects


#avespeed_mob: (mm/s) the average speed of the individual excluding periods when it was immobile
#avespeed_mob.ACTT1
#avespeed_mob.MEAN
#avespeed_mob.COEF
GULD_N.avespeed_mob.ACTT1.mod <- lmer(d15N ~ avespeed_mob.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.avespeed_mob.ACTT1.mod)
confint(GULD_N.avespeed_mob.ACTT1.mod)
plot(GULD_N.avespeed_mob.ACTT1.mod)

GULD_N.avespeed_mob.MEAN.mod <- lmer(d15N ~ avespeed_mob.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.avespeed_mob.MEAN.mod)
confint(GULD_N.avespeed_mob.MEAN.mod)
plot(GULD_N.avespeed_mob.MEAN.mod)

GULD_N.avespeed_mob.COEF.mod <- lmer(d15N ~ avespeed_mob.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.avespeed_mob.COEF.mod)
confint(GULD_N.avespeed_mob.COEF.mod)
plot(GULD_N.avespeed_mob.COEF.mod)

GULD_C.avespeed_mob.ACTT1.mod <- lmer(d13C ~ avespeed_mob.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.avespeed_mob.ACTT1.mod)
confint(GULD_C.avespeed_mob.ACTT1.mod)
plot(GULD_C.avespeed_mob.ACTT1.mod)

GULD_C.avespeed_mob.MEAN.mod <- lmer(d13C ~ avespeed_mob.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.avespeed_mob.MEAN.mod)
confint(GULD_C.avespeed_mob.MEAN.mod)
plot(GULD_C.avespeed_mob.MEAN.mod)

GULD_C.avespeed_mob.COEF.mod <- lmer(d13C ~ avespeed_mob.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.avespeed_mob.COEF.mod)
confint(GULD_C.avespeed_mob.COEF.mod)
plot(GULD_C.avespeed_mob.COEF.mod)
#Summary: No effects


#aveacceler: (mm/s^2) average rate of acceleration accross the trial
#aveacceler.MEAN
#aveacceler.ACTT1
#aveacceler.COEF
GULD_N.aveacceler.ACTT1.mod <- lmer(d15N ~ aveacceler.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.aveacceler.ACTT1.mod)
confint(GULD_N.aveacceler.ACTT1.mod)
plot(GULD_N.aveacceler.ACTT1.mod)

GULD_N.aveacceler.MEAN.mod <- lmer(d15N ~ aveacceler.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.aveacceler.MEAN.mod)
confint(GULD_N.aveacceler.MEAN.mod)
plot(GULD_N.aveacceler.MEAN.mod)

GULD_N.aveacceler.COEF.mod <- lmer(d15N ~ aveacceler.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.aveacceler.COEF.mod)
confint(GULD_N.aveacceler.COEF.mod)
plot(GULD_N.aveacceler.COEF.mod)

GULD_C.aveacceler.ACTT1.mod <- lmer(d13C ~ aveacceler.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.aveacceler.ACTT1.mod)
confint(GULD_C.aveacceler.ACTT1.mod)
plot(GULD_C.aveacceler.ACTT1.mod)

GULD_C.aveacceler.MEAN.mod <- lmer(d13C ~ aveacceler.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.aveacceler.MEAN.mod)
confint(GULD_C.aveacceler.MEAN.mod)
plot(GULD_C.aveacceler.MEAN.mod)

GULD_C.aveacceler.COEF.mod <- lmer(d13C ~ aveacceler.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.aveacceler.COEF.mod)
confint(GULD_C.aveacceler.COEF.mod)
plot(GULD_C.aveacceler.COEF.mod)
#Summary: No effects


#propmoving: (proportional) proportion of time mobile
#propmoving.MEAN
#propmoving.ACTT1
#propmoving.invlog.COEF
GULD_N.propmoving.ACTT1.mod <- lmer(d15N ~ log(1-propmoving.ACTT1) +  (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.propmoving.ACTT1.mod)
confint(GULD_N.propmoving.ACTT1.mod)
plot(GULD_N.propmoving.ACTT1.mod)

GULD_N.propmoving.MEAN.mod <- lmer(d15N ~ propmoving.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.propmoving.MEAN.mod)
confint(GULD_N.propmoving.MEAN.mod)
plot(GULD_N.propmoving.MEAN.mod)

GULD_N.propmoving.invlog.COEF.mod <- lmer(d15N ~ propmoving.invlog.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.propmoving.invlog.COEF.mod)
confint(GULD_N.propmoving.invlog.COEF.mod)
plot(GULD_N.propmoving.invlog.COEF.mod)

GULD_C.propmoving.ACTT1.mod <- lmer(d13C ~ propmoving.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.propmoving.ACTT1.mod)
confint(GULD_C.propmoving.ACTT1.mod)
plot(GULD_C.propmoving.ACTT1.mod)

GULD_C.propmoving.MEAN.mod <- lmer(d13C ~ propmoving.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.propmoving.MEAN.mod)
confint(GULD_C.propmoving.MEAN.mod)
plot(GULD_C.propmoving.MEAN.mod)

GULD_C.propmoving.invlog.COEF.mod <- lmer(d13C ~ propmoving.invlog.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.propmoving.invlog.COEF.mod)
confint(GULD_C.propmoving.invlog.COEF.mod)
plot(GULD_C.propmoving.invlog.COEF.mod)
#Summary: propmoving.ACTT1 arginal effect of d15N


#dist: (mm) total distance travelled during trial
#dist.ACTT1
#dist.MEAN
#dist.COEF
GULD_N.dist.ACTT1.mod <- lmer(d15N ~ dist.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.dist.ACTT1.mod)
confint(GULD_N.dist.ACTT1.mod)
plot(GULD_N.dist.ACTT1.mod)

GULD_N.dist.MEAN.mod <- lmer(d15N ~ dist.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.dist.MEAN.mod)
confint(GULD_N.dist.MEAN.mod)
plot(GULD_N.dist.MEAN.mod)

GULD_N.dist.COEF.mod <- lmer(d15N ~ dist.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.dist.COEF.mod)
confint(GULD_N.dist.COEF.mod)
plot(GULD_N.dist.COEF.mod)

GULD_C.dist.ACTT1.mod <- lmer(d13C ~ dist.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.dist.ACTT1.mod)
confint(GULD_C.dist.ACTT1.mod)
plot(GULD_C.dist.ACTT1.mod)

GULD_C.dist.MEAN.mod <- lmer(d13C ~ dist.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.dist.MEAN.mod)
confint(GULD_C.dist.MEAN.mod)
plot(GULD_C.dist.MEAN.mod)

GULD_C.dist.COEF.mod <- lmer(d13C ~ dist.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.dist.COEF.mod)
confint(GULD_C.dist.COEF.mod)
plot(GULD_C.dist.COEF.mod)
#Summary: No effects


#frozenevents: (count) the number of times spent frozen during trial (min. frozen period 3 secs)
#frozenevents.ACTT1
#frozenevents.MEAN
#frozenevents.sqrt.COEF
GULD_N.frozenevents.ACTT1.mod <- lmer(d15N ~ frozenevents.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.frozenevents.ACTT1.mod)
confint(GULD_N.frozenevents.ACTT1.mod)
plot(GULD_N.frozenevents.ACTT1.mod)

GULD_N.frozenevents.MEAN.mod <- lmer(d15N ~ frozenevents.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.frozenevents.MEAN.mod)
confint(GULD_N.frozenevents.MEAN.mod)
plot(GULD_N.frozenevents.MEAN.mod)

GULD_N.frozenevents.sqrt.COEF.mod <- lmer(d15N ~ frozenevents.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.frozenevents.sqrt.COEF.mod)
confint(GULD_N.frozenevents.sqrt.COEF.mod)
plot(GULD_N.frozenevents.sqrt.COEF.mod)

GULD_C.frozenevents.ACTT1.mod <- lmer(d13C ~ frozenevents.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.frozenevents.ACTT1.mod)
confint(GULD_C.frozenevents.ACTT1.mod)
plot(GULD_C.frozenevents.ACTT1.mod)

GULD_C.frozenevents.MEAN.mod <- lmer(d13C ~ frozenevents.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.frozenevents.MEAN.mod)
confint(GULD_C.frozenevents.MEAN.mod)
plot(GULD_C.frozenevents.MEAN.mod)

GULD_C.frozenevents.sqrt.COEF.mod <- lmer(d13C ~ frozenevents.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.frozenevents.sqrt.COEF.mod)
confint(GULD_C.frozenevents.sqrt.COEF.mod)
plot(GULD_C.frozenevents.sqrt.COEF.mod)
#Summary: No effects


#timefrozen_tot: (s) total time spent frozen during trial
#timefrozen_tot.ACTT1
#timefrozen_tot.MEAN
#timefrozen_tot.sqrt.COEF
GULD_N.timefrozen_tot.ACTT1.mod <- lmer(d15N ~ timefrozen_tot.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.timefrozen_tot.ACTT1.mod)
confint(GULD_N.timefrozen_tot.ACTT1.mod)
plot(GULD_N.timefrozen_tot.ACTT1.mod)

GULD_N.timefrozen_tot.MEAN.mod <- lmer(d15N ~ timefrozen_tot.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.timefrozen_tot.MEAN.mod)
confint(GULD_N.timefrozen_tot.MEAN.mod)
plot(GULD_N.timefrozen_tot.MEAN.mod)

GULD_N.timefrozen_tot.sqrt.COEF.mod <- lmer(d15N ~ timefrozen_tot.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.timefrozen_tot.sqrt.COEF.mod)
confint(GULD_N.timefrozen_tot.sqrt.COEF.mod)
plot(GULD_N.timefrozen_tot.sqrt.COEF.mod)

GULD_C.timefrozen_tot.ACTT1.mod <- lmer(d13C ~ timefrozen_tot.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.timefrozen_tot.ACTT1.mod)
confint(GULD_C.timefrozen_tot.ACTT1.mod)
plot(GULD_C.timefrozen_tot.ACTT1.mod)

GULD_C.timefrozen_tot.MEAN.mod <- lmer(d13C ~ timefrozen_tot.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.timefrozen_tot.MEAN.mod)
confint(GULD_C.timefrozen_tot.MEAN.mod)
plot(GULD_C.timefrozen_tot.MEAN.mod)

GULD_C.timefrozen_tot.sqrt.COEF.mod <- lmer(d13C ~ timefrozen_tot.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.timefrozen_tot.sqrt.COEF.mod)
confint(GULD_C.timefrozen_tot.sqrt.COEF.mod)
plot(GULD_C.timefrozen_tot.sqrt.COEF.mod)
#Summary: timefrozen_tot.ACTT1 marginal effect on N, timefrozen_tot.MEAN marginal effect on C


#centrescore: (NA) calculated from the proportion of time spent in each area
#centrescore.ACTT1
#centrescore.MEAN
#centrescore.COEF
GULD_N.centrescore.ACTT1.mod <- lmer(d15N ~ centrescore.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centrescore.ACTT1.mod)
confint(GULD_N.centrescore.ACTT1.mod)
plot(GULD_N.centrescore.ACTT1.mod)

GULD_N.centrescore.MEAN.mod <- lmer(d15N ~ centrescore.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centrescore.MEAN.mod)
confint(GULD_N.centrescore.MEAN.mod)
plot(GULD_N.centrescore.MEAN.mod)

GULD_N.centrescore.COEF.mod <- lmer(d15N ~ centrescore.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centrescore.COEF.mod)
confint(GULD_N.centrescore.COEF.mod)
plot(GULD_N.centrescore.COEF.mod)

GULD_C.centrescore.ACTT1.mod <- lmer(d13C ~ centrescore.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centrescore.ACTT1.mod)
confint(GULD_C.centrescore.ACTT1.mod)
plot(GULD_C.centrescore.ACTT1.mod)

GULD_C.centrescore.MEAN.mod <- lmer(d13C ~ centrescore.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centrescore.MEAN.mod)
confint(GULD_C.centrescore.MEAN.mod)
plot(GULD_C.centrescore.MEAN.mod)

GULD_C.centrescore.COEF.mod <- lmer(d13C ~ centrescore.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centrescore.COEF.mod)
confint(GULD_C.centrescore.COEF.mod)
plot(GULD_C.centrescore.COEF.mod)
#Summary: No effects


#centretime50: (s) time >5cm away from edge
#centretime50.ACTT1
#centretime50.MEAN
#centretime50.sqrt.COEF
GULD_N.centretime50.ACTT1.mod <- lmer(d15N ~ centretime50.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centretime50.ACTT1.mod)
confint(GULD_N.centretime50.ACTT1.mod)
plot(GULD_N.centretime50.ACTT1.mod)

GULD_N.centretime50.MEAN.mod <- lmer(d15N ~ centretime50.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centretime50.MEAN.mod)
confint(GULD_N.centretime50.MEAN.mod)
plot(GULD_N.centretime50.MEAN.mod)

GULD_N.centretime50.sqrt.COEF.mod <- lmer(d15N ~ centretime50.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centretime50.sqrt.COEF.mod)
confint(GULD_N.centretime50.sqrt.COEF.mod)
plot(GULD_N.centretime50.sqrt.COEF.mod)

GULD_C.centretime50.ACTT1.mod <- lmer(d13C ~ centretime50.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centretime50.ACTT1.mod)
confint(GULD_C.centretime50.ACTT1.mod)
plot(GULD_C.centretime50.ACTT1.mod)

GULD_C.centretime50.MEAN.mod <- lmer(d13C ~ centretime50.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centretime50.MEAN.mod)
confint(GULD_C.centretime50.MEAN.mod)
plot(GULD_C.centretime50.MEAN.mod)

GULD_C.centretime50.sqrt.COEF.mod <- lmer(d13C ~ centretime50.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centretime50.sqrt.COEF.mod)
confint(GULD_C.centretime50.sqrt.COEF.mod)
plot(GULD_C.centretime50.sqrt.COEF.mod)
#Summary: No effects


#centretime75: (s) time >7.5cm away from edge
#centretime75.ACTT1
#centretime75.MEAN
#centretime75.sqrt.COEF
GULD_N.centretime75.ACTT1.mod <- lmer(d15N ~ centretime75.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centretime75.ACTT1.mod)
confint(GULD_N.centretime75.ACTT1.mod)
plot(GULD_N.centretime75.ACTT1.mod)

GULD_N.centretime75.MEAN.mod <- lmer(d15N ~ centretime75.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centretime75.MEAN.mod)
confint(GULD_N.centretime75.MEAN.mod)
plot(GULD_N.centretime75.MEAN.mod)

GULD_N.centretime75.sqrt.COEF.mod <- lmer(d15N ~ centretime75.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centretime75.sqrt.COEF.mod)
confint(GULD_N.centretime75.sqrt.COEF.mod)
plot(GULD_N.centretime75.sqrt.COEF.mod)

GULD_C.centretime75.ACTT1.mod <- lmer(d13C ~ centretime75.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centretime75.ACTT1.mod)
confint(GULD_C.centretime75.ACTT1.mod)
plot(GULD_C.centretime75.ACTT1.mod)

GULD_C.centretime75.MEAN.mod <- lmer(d13C ~ centretime75.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centretime75.MEAN.mod)
confint(GULD_C.centretime75.MEAN.mod)
plot(GULD_C.centretime75.MEAN.mod)

GULD_C.centretime75.sqrt.COEF.mod <- lmer(d13C ~ centretime75.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centretime75.sqrt.COEF.mod)
confint(GULD_C.centretime75.sqrt.COEF.mod)
plot(GULD_C.centretime75.sqrt.COEF.mod)
#Summary: No effects


#centretime100: (s) time >10cm away from edge
#centretime100.ACTT1
#centretime100.MEAN
#centretime100.sqrt.COEF
GULD_N.centretime100.ACTT1.mod <- lmer(d15N ~ centretime100.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centretime100.ACTT1.mod)
confint(GULD_N.centretime100.ACTT1.mod)
plot(GULD_N.centretime100.ACTT1.mod)

GULD_N.centretime100.MEAN.mod <- lmer(d15N ~ centretime100.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centretime100.MEAN.mod)
confint(GULD_N.centretime100.MEAN.mod)
plot(GULD_N.centretime100.MEAN.mod)

GULD_N.centretime100.sqrt.COEF.mod <- lmer(d15N ~ centretime100.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.centretime100.sqrt.COEF.mod)
confint(GULD_N.centretime100.sqrt.COEF.mod)
plot(GULD_N.centretime100.sqrt.COEF.mod)

GULD_C.centretime100.ACTT1.mod <- lmer(d13C ~ centretime100.ACTT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centretime100.ACTT1.mod)
confint(GULD_C.centretime100.ACTT1.mod)
plot(GULD_C.centretime100.ACTT1.mod)

GULD_C.centretime100.MEAN.mod <- lmer(d13C ~ centretime100.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centretime100.MEAN.mod)
confint(GULD_C.centretime100.MEAN.mod)
plot(GULD_C.centretime100.MEAN.mod)

GULD_C.centretime100.sqrt.COEF.mod <- lmer(d13C ~ centretime100.sqrt.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.centretime100.sqrt.COEF.mod)
confint(GULD_C.centretime100.sqrt.COEF.mod)
plot(GULD_C.centretime100.sqrt.COEF.mod)
#Summary: centretime100.ACTT1 marginal effect on d13C (sig. if sqrt transformed)


#emergelat.bin: binomially converted latency to emerge from the shelter
#emergelat.bin.EXPLT1
#emergelat.bin.MEAN        
#emergelat.bin.COEF        
GULD_N.emergelat.bin.EXPLT1.mod <- lmer(d15N ~ emergelat.bin.EXPLT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.emergelat.bin.EXPLT1.mod)
confint(GULD_N.emergelat.bin.EXPLT1.mod)
plot(GULD_N.emergelat.bin.EXPLT1.mod)

GULD_N.emergelat.bin.MEAN.mod <- lmer(d15N ~ emergelat.bin.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.emergelat.bin.MEAN.mod)
confint(GULD_N.emergelat.bin.MEAN.mod)
plot(GULD_N.emergelat.bin.MEAN.mod)

GULD_N.emergelat.bin.COEF.mod <- lmer(d15N ~ emergelat.bin.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.emergelat.bin.COEF.mod)
confint(GULD_N.emergelat.bin.COEF.mod)
plot(GULD_N.emergelat.bin.COEF.mod)

GULD_C.emergelat.bin.EXPLT1.mod <- lmer(d13C ~ emergelat.bin.EXPLT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.emergelat.bin.EXPLT1.mod)
confint(GULD_C.emergelat.bin.EXPLT1.mod)
plot(GULD_C.emergelat.bin.EXPLT1.mod)

GULD_C.emergelat.bin.MEAN.mod <- lmer(d13C ~ emergelat.bin.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.emergelat.bin.MEAN.mod)
confint(GULD_C.emergelat.bin.MEAN.mod)
plot(GULD_C.emergelat.bin.MEAN.mod)

GULD_C.emergelat.bin.COEF.mod <- lmer(d13C ~ emergelat.bin.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.emergelat.bin.COEF.mod)
confint(GULD_C.emergelat.bin.COEF.mod)
plot(GULD_C.emergelat.bin.COEF.mod)
#Summary: No effects


#endpointlat: binomially converted latency to explore to the endpoint from trial start time
#endpointlat.bin.EXPLT1
#endpointlat.bin.MEAN
#endpointlat.bin.COEF
GULD_N.endpointlat.bin.EXPLT1.mod <- lmer(d15N ~ endpointlat.bin.EXPLT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.endpointlat.bin.EXPLT1.mod)
confint(GULD_N.endpointlat.bin.EXPLT1.mod)
plot(GULD_N.endpointlat.bin.EXPLT1.mod)

GULD_N.endpointlat.bin.MEAN.mod <- lmer(d15N ~ endpointlat.bin.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.endpointlat.bin.MEAN.mod)
confint(GULD_N.endpointlat.bin.MEAN.mod)
plot(GULD_N.endpointlat.bin.MEAN.mod)

GULD_N.endpointlat.bin.COEF.mod <- lmer(d15N ~ endpointlat.bin.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.endpointlat.bin.COEF.mod)
confint(GULD_N.endpointlat.bin.COEF.mod)
plot(GULD_N.endpointlat.bin.COEF.mod)

GULD_C.endpointlat.bin.EXPLT1.mod <- lmer(d13C ~ endpointlat.bin.EXPLT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.endpointlat.bin.EXPLT1.mod)
confint(GULD_C.endpointlat.bin.EXPLT1.mod)
plot(GULD_C.endpointlat.bin.EXPLT1.mod)

GULD_C.endpointlat.bin.MEAN.mod <- lmer(d13C ~ endpointlat.bin.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.endpointlat.bin.MEAN.mod)
confint(GULD_C.endpointlat.bin.MEAN.mod)
plot(GULD_C.endpointlat.bin.MEAN.mod)

GULD_C.endpointlat.bin.COEF.mod <- lmer(d13C ~ endpointlat.bin.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.endpointlat.bin.COEF.mod)
confint(GULD_C.endpointlat.bin.COEF.mod)
plot(GULD_C.endpointlat.bin.COEF.mod)
#Summary: No effects


#endpointspeed: (s) latency to explore to the endpoint from time of emergence (note: 46 NAs, where fish did not reach the end)
#endpointspeed.EXPLT1
#endpointspeed.MEAN
#endpointspeed.ln.COEF
GULD_N.endpointspeed.EXPLT1.mod <- lmer(d15N ~ endpointspeed.EXPLT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.endpointspeed.EXPLT1.mod)
confint(GULD_N.endpointspeed.EXPLT1.mod)
plot(GULD_N.endpointspeed.EXPLT1.mod)

GULD_N.endpointspeed.MEAN.mod <- lmer(d15N ~ endpointspeed.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.endpointspeed.MEAN.mod)
confint(GULD_N.endpointspeed.MEAN.mod)
plot(GULD_N.endpointspeed.MEAN.mod)

GULD_N.endpointspeed.ln.COEF.mod <- lmer(d15N ~ endpointspeed.ln.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.endpointspeed.ln.COEF.mod)
confint(GULD_N.endpointspeed.ln.COEF.mod)
plot(GULD_N.endpointspeed.ln.COEF.mod)

GULD_C.endpointspeed.EXPLT1.mod <- lmer(d13C ~ endpointspeed.EXPLT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.endpointspeed.EXPLT1.mod)
confint(GULD_C.endpointspeed.EXPLT1.mod)
plot(GULD_C.endpointspeed.EXPLT1.mod)

GULD_C.endpointspeed.MEAN.mod <- lmer(d13C ~ endpointspeed.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.endpointspeed.MEAN.mod)
confint(GULD_C.endpointspeed.MEAN.mod)
plot(GULD_C.endpointspeed.MEAN.mod)

GULD_C.endpointspeed.ln.mod <- lmer(d13C ~ endpointspeed.ln.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.endpointspeed.ln.mod)
confint(GULD_C.endpointspeed.ln.mod)
plot(GULD_C.endpointspeed.ln.mod)
#Summary: No effects


#refugereturnlat: (s) latency to return to refuge after first emergence (note: 30 NAs, where fish did not return, or emerge at all)
#refugereturnlat.EXPLT1
#refugereturnlat.MEAN
#refugereturnlat.ln.COEF
GULD_N.refugereturnlat.EXPLT1.mod <- lmer(d15N ~ refugereturnlat.EXPLT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.refugereturnlat.EXPLT1.mod)
confint(GULD_N.refugereturnlat.EXPLT1.mod)
plot(GULD_N.refugereturnlat.EXPLT1.mod)

GULD_N.refugereturnlat.MEAN.mod <- lmer(d15N ~ refugereturnlat.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.refugereturnlat.MEAN.mod)
confint(GULD_N.refugereturnlat.MEAN.mod)
plot(GULD_N.refugereturnlat.MEAN.mod)

GULD_N.refugereturnlat.ln.COEF.mod <- lmer(d15N ~ refugereturnlat.ln.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_N.refugereturnlat.ln.COEF.mod)
confint(GULD_N.refugereturnlat.ln.COEF.mod)
plot(GULD_N.refugereturnlat.ln.COEF.mod)

GULD_C.refugereturnlat.EXPLT1.mod <- lmer(d13C ~ refugereturnlat.EXPLT1 + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.refugereturnlat.EXPLT1.mod)
confint(GULD_C.refugereturnlat.EXPLT1.mod)
plot(GULD_C.refugereturnlat.EXPLT1.mod)

GULD_C.refugereturnlat.MEAN.mod <- lmer(d13C ~ refugereturnlat.MEAN + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.refugereturnlat.MEAN.mod)
confint(GULD_C.refugereturnlat.MEAN.mod)
plot(GULD_C.refugereturnlat.MEAN.mod)

GULD_C.refugereturnlat.ln.mod <- lmer(d13C ~ refugereturnlat.ln.COEF + (1|FishID), data=GULD_processed.fishfull)
summary(GULD_C.refugereturnlat.ln.mod)
confint(GULD_C.refugereturnlat.ln.mod)
plot(GULD_C.refugereturnlat.ln.mod)
#Summary: No effects



#Additional options: 
#excluding SIA outlier, G34)----

