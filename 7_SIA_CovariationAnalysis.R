# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG = "en")
library(data.table)


#7. Effects of fish traits on d15N d13C ----

### 7.1. Compiling individual behavioural and isotope datasets ----
#SIA dataset
GULD_SIA1 <- read.csv("~/trophicpersonalities_GULD/6_SIA_VarianceAnalysis/GULD_SIA1.processed.csv")
labels(GULD_SIA1)
GULD_SIA1 <- rename(GULD_SIA1, weight_packed = weight_packed..mg.)
GULD_SIA1 <- rename(GULD_SIA1, FishID = fishID)
GULD_SIA1 <- select(GULD_SIA1, -c(X, run.line, run.ID, date_packed, row, position, row.id))
GULD_SIA1


#Behavioural dataset
GULD_behav_merged <- read.csv("~/trophicpersonalities_GULD/5_Behaviour_CovariationAnalysis/GULD_behav_merged.csv")

#Separating each trial day out
GULD_behav_merged.1 <- subset(GULD_behav_merged, TrialDay == 1)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, Weight_1 = Weight)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, avespeed_tot_1 = avespeed_tot)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, avespeed_mob_1 = avespeed_mob)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, aveacceler_1 = aveacceler)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, dist_1 = dist)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, timefrozen_tot.ln_1 = timefrozen_tot.ln)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, propmoving.exp_1 = propmoving.exp)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, centretime.lnplus1_1 = centretime.lnplus1)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, emergelat.bin.B_1 = emergelat.bin.B)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, endpointlat.bin.B_1 = endpointlat.bin.B)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, PC1_1 = PC1)
GULD_behav_merged.1 <- rename(GULD_behav_merged.1, PC2_1 = PC2)

GULD_behav_merged.2 <- subset(GULD_behav_merged, TrialDay == 2)
GULD_behav_merged.2 <- select(GULD_behav_merged.2, -c(Weight, X, mergeID, TL, PITID, InfectionScore, Sex, TrialDay, TrialRound, TankID))
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, avespeed_tot_2 = avespeed_tot)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, avespeed_mob_2 = avespeed_mob)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, aveacceler_2 = aveacceler)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, dist_2 = dist)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, timefrozen_tot.ln_2 = timefrozen_tot.ln)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, propmoving.exp_2 = propmoving.exp)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, centretime.lnplus1_2 = centretime.lnplus1)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, emergelat.bin.B_2 = emergelat.bin.B)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, endpointlat.bin.B_2 = endpointlat.bin.B)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, PC1_2 = PC1)
GULD_behav_merged.2 <- rename(GULD_behav_merged.2, PC2_2 = PC2)

GULD_behav_merged.3 <- subset(GULD_behav_merged, TrialDay == 3)
GULD_behav_merged.3 <- select(GULD_behav_merged.3, -c(Weight, X, mergeID, TL, PITID, InfectionScore, Sex, TrialDay, TrialRound, TankID))
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, avespeed_tot_3 = avespeed_tot)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, avespeed_mob_3 = avespeed_mob)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, aveacceler_3 = aveacceler)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, dist_3 = dist)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, timefrozen_tot.ln_3 = timefrozen_tot.ln)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, propmoving.exp_3 = propmoving.exp)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, centretime.lnplus1_3 = centretime.lnplus1)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, emergelat.bin.B_3 = emergelat.bin.B)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, endpointlat.bin.B_3 = endpointlat.bin.B)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, PC1_3 = PC1)
GULD_behav_merged.3 <- rename(GULD_behav_merged.3, PC2_3 = PC2)


GULD_behav_merged.SIA <- merge(GULD_behav_merged.1, GULD_behav_merged.2, by = 'FishID', all.x = TRUE)
GULD_behav_merged.SIA <- merge(GULD_behav_merged.SIA, GULD_behav_merged.3, by = 'FishID', all.x = TRUE)

#Creating 3-trial averages
GULD_behav_merged.averages <-  setDT(GULD_behav_merged)[ , list(avespeed_tot_mean = mean(avespeed_tot),
                                                                avespeed_mob_mean = mean(avespeed_mob),
                                                                aveacceler_mean = mean(aveacceler),
                                                                dist_mean = mean(dist),
                                                                timefrozen_tot.ln_mean = mean(timefrozen_tot.ln),
                                                                propmoving.exp_mean = mean(propmoving.exp),
                                                                centretime.lnplus1_mean = mean(centretime.lnplus1),
                                                                emergelat.bin.B_mean = mean(emergelat.bin.B),
                                                                endpointlat.bin.B_mean = mean(endpointlat.bin.B),
                                                                PC1_mean = mean(PC1),
                                                                PC2_mean = mean(PC2)),
                                                         by = .(FishID)]
GULD_behav_merged.SIA <- merge(GULD_behav_merged.SIA, GULD_behav_merged.averages, by = 'FishID', all.x = TRUE) 


#Adding in SIA data
GULD_behav_merged.SIA.ready <- merge(GULD_SIA1, GULD_behav_merged.SIA, by = 'FishID', all.x = TRUE)
GULD_behav_merged.SIA.ready$InfectionScore <- as.factor(GULD_behav_merged.SIA.ready$InfectionScore)
GULD_behav_merged.SIA.ready$emergelat.bin.B_1 <- as.factor(GULD_behav_merged.SIA.ready$emergelat.bin.B_1)
GULD_behav_merged.SIA.ready$endpointlat.bin.B_1 <- as.factor(GULD_behav_merged.SIA.ready$endpointlat.bin.B_1)
GULD_behav_merged.SIA.ready$Sex <- as.factor(GULD_behav_merged.SIA.ready$Sex)


### 7.2. Effects of Non-behavioural variables ----

#Non behavioural variables to test:
#    TL
#    Weight_1 (just using first weight, as most likely to reflect size differences in the wild)
#    InfectionScore
#    Sex


#d15N
GULD_N.TL.mod <- lmer(d15N ~ TL + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.TL.mod)
confint(GULD_N.TL.mod)
plot(GULD_N.TL.mod)
plot(GULD_behav_merged.SIA.ready$TL, GULD_behav_merged.SIA.ready$d15N)

GULD_N.Weight_1.mod <- lmer(d15N ~ Weight_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.Weight_1.mod)
confint(GULD_N.Weight_1.mod)
plot(GULD_N.Weight_1.mod)
plot(GULD_behav_merged.SIA.ready$Weight_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.InfectionScore.mod <- lmer(d15N ~ InfectionScore + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.InfectionScore.mod)
confint(GULD_N.InfectionScore.mod)
plot(GULD_N.InfectionScore.mod)
plot(GULD_behav_merged.SIA.ready$InfectionScore, GULD_behav_merged.SIA.ready$d15N)

GULD_N.Sex.mod <- lmer(d15N ~ Sex + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.Sex.mod)
confint(GULD_N.Sex.mod)
plot(GULD_N.Sex.mod)
plot(GULD_behav_merged.SIA.ready$Sex, GULD_behav_merged.SIA.ready$d15N)


#d13C
GULD_C.TL.mod <- lmer(d13C ~ TL + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.TL.mod)
confint(GULD_C.TL.mod)
plot(GULD_C.TL.mod)
plot(GULD_behav_merged.SIA.ready$TL, GULD_behav_merged.SIA.ready$d13C)

GULD_C.Weight_1.mod <- lmer(d13C ~ Weight_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.Weight_1.mod) #Marginally significant effect of weight on #d15C
confint(GULD_C.Weight_1.mod)
plot(GULD_C.Weight_1.mod)
plot(GULD_behav_merged.SIA.ready$Weight_1, GULD_behav_merged.SIA.ready$d13C) 

GULD_C.InfectionScore.mod <- lmer(d13C ~ InfectionScore + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.InfectionScore.mod)
confint(GULD_C.InfectionScore.mod)
plot(GULD_C.InfectionScore.mod)
plot(GULD_behav_merged.SIA.ready$InfectionScore, GULD_behav_merged.SIA.ready$d13C)

GULD_C.Sex.mod <- lmer(d13C ~ Sex + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.Sex.mod)
confint(GULD_C.Sex.mod)
plot(GULD_C.Sex.mod)
plot(GULD_behav_merged.SIA.ready$Sex, GULD_behav_merged.SIA.ready$d13C)

#Summary: only a marginally signigficant negative effect of weight on d13C


### 7.3. Effects of Trial 1 behavioural variables ----

#Behavioural variables to test for effects on d15N and d13C
#     avespeed_tot_1
#     avespeed_mob_1
#     aveacceler_1
#     dist_1
#     timefrozen_tot.ln_1
#     propmoving.exp_1
#     centretime.lnplus1_1
#     emergelat.bin.B_1
#     endpointlat.bin.B_1
#     PC1_1
#     PC2_1

#d15N
GULD_N.avespeed_tot_1.mod <- lmer(d15N ~ avespeed_tot_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.avespeed_tot_1.mod)
confint(GULD_N.avespeed_tot_1.mod)
plot(GULD_N.avespeed_tot_1.mod)
plot(GULD_behav_merged.SIA.ready$avespeed_tot_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.avespeed_mob_1.mod <- lmer(d15N ~ avespeed_mob_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.avespeed_mob_1.mod)
confint(GULD_N.avespeed_mob_1.mod)
plot(GULD_N.avespeed_mob_1.mod)
plot(GULD_behav_merged.SIA.ready$avespeed_mob_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.aveacceler_1.mod <- lmer(d15N ~ aveacceler_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.aveacceler_1.mod)
confint(GULD_N.aveacceler_1.mod)
plot(GULD_N.aveacceler_1.mod)
plot(GULD_behav_merged.SIA.ready$aveacceler_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.dist_1.mod <- lmer(d15N ~ dist_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.dist_1.mod)
confint(GULD_N.dist_1.mod)
plot(GULD_N.dist_1.mod)
plot(GULD_behav_merged.SIA.ready$dist_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.timefrozen_tot.ln_1.mod <- lmer(d15N ~ timefrozen_tot.ln_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.timefrozen_tot.ln_1.mod)
confint(GULD_N.timefrozen_tot.ln_1.mod)
plot(GULD_N.timefrozen_tot.ln_1.mod)
plot(GULD_behav_merged.SIA.ready$timefrozen_tot.ln_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.propmoving.exp_1.mod <- lmer(d15N ~ propmoving.exp_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.propmoving.exp_1.mod) #close to signficant
confint(GULD_N.propmoving.exp_1.mod)
plot(GULD_N.propmoving.exp_1.mod)
plot(GULD_behav_merged.SIA.ready$propmoving.exp_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.centretime.lnplus1_1.mod <- lmer(d15N ~ centretime.lnplus1_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.centretime.lnplus1_1.mod)
confint(GULD_N.centretime.lnplus1_1.mod)
plot(GULD_N.centretime.lnplus1_1.mod)
plot(GULD_behav_merged.SIA.ready$centretime.lnplus1_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.emergelat.bin.B_1.mod <- lmer(d15N ~ emergelat.bin.B_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.emergelat.bin.B_1.mod)
confint(GULD_N.emergelat.bin.B_1.mod)
plot(GULD_N.emergelat.bin.B_1.mod)
plot(GULD_behav_merged.SIA.ready$emergelat.bin.B_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.endpointlat.bin.B_1.mod <- lmer(d15N ~ endpointlat.bin.B_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.endpointlat.bin.B_1.mod)
confint(GULD_N.endpointlat.bin.B_1.mod)
plot(GULD_N.endpointlat.bin.B_1.mod)
plot(GULD_behav_merged.SIA.ready$endpointlat.bin.B_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.PC1_1.mod <- lmer(d15N ~ PC1_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.PC1_1.mod)
confint(GULD_N.PC1_1.mod)
plot(GULD_N.PC1_1.mod)
plot(GULD_behav_merged.SIA.ready$PC1_1, GULD_behav_merged.SIA.ready$d15N)

GULD_N.PC2_1.mod <- lmer(d15N ~ PC2_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.PC2_1.mod)
confint(GULD_N.PC2_1.mod)
plot(GULD_N.PC2_1.mod)
plot(GULD_behav_merged.SIA.ready$PC2_1, GULD_behav_merged.SIA.ready$d15N)


#d13C
GULD_C.avespeed_tot_1.mod <- lmer(d13C ~ avespeed_tot_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.avespeed_tot_1.mod)
confint(GULD_C.avespeed_tot_1.mod)
plot(GULD_C.avespeed_tot_1.mod)
plot(GULD_behav_merged.SIA.ready$avespeed_tot_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.avespeed_mob_1.mod <- lmer(d13C ~ avespeed_mob_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.avespeed_mob_1.mod)
confint(GULD_C.avespeed_mob_1.mod)
plot(GULD_C.avespeed_mob_1.mod)
plot(GULD_behav_merged.SIA.ready$avespeed_mob_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.aveacceler_1.mod <- lmer(d13C ~ aveacceler_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.aveacceler_1.mod)
confint(GULD_C.aveacceler_1.mod)
plot(GULD_C.aveacceler_1.mod)
plot(GULD_behav_merged.SIA.ready$aveacceler_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.dist_1.mod <- lmer(d13C ~ dist_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.dist_1.mod)
confint(GULD_C.dist_1.mod)
plot(GULD_C.dist_1.mod)
plot(GULD_behav_merged.SIA.ready$dist_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.timefrozen_tot.ln_1.mod <- lmer(d13C ~ timefrozen_tot.ln_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.timefrozen_tot.ln_1.mod)
confint(GULD_C.timefrozen_tot.ln_1.mod)
plot(GULD_C.timefrozen_tot.ln_1.mod)
plot(GULD_behav_merged.SIA.ready$timefrozen_tot.ln_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.propmoving.exp_1.mod <- lmer(d13C ~ propmoving.exp_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.propmoving.exp_1.mod)
confint(GULD_C.propmoving.exp_1.mod)
plot(GULD_C.propmoving.exp_1.mod)
plot(GULD_behav_merged.SIA.ready$propmoving.exp_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.centretime.lnplus1_1.mod <- lmer(d13C ~ centretime.lnplus1_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.centretime.lnplus1_1.mod) #significant negative effect of centre area use and d13C
confint(GULD_C.centretime.lnplus1_1.mod)
plot(GULD_C.centretime.lnplus1_1.mod)
plot(GULD_behav_merged.SIA.ready$centretime.lnplus1_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.emergelat.bin.B_1.mod <- lmer(d13C ~ emergelat.bin.B_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.emergelat.bin.B_1.mod)
confint(GULD_C.emergelat.bin.B_1.mod)
plot(GULD_C.emergelat.bin.B_1.mod)
plot(GULD_behav_merged.SIA.ready$emergelat.bin.B_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.endpointlat.bin.B_1.mod <- lmer(d13C ~ endpointlat.bin.B_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.endpointlat.bin.B_1.mod)
confint(GULD_C.endpointlat.bin.B_1.mod)
plot(GULD_C.endpointlat.bin.B_1.mod)
plot(GULD_behav_merged.SIA.ready$endpointlat.bin.B_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.PC1_1.mod <- lmer(d13C ~ PC1_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.PC1_1.mod)
confint(GULD_C.PC1_1.mod)
plot(GULD_C.PC1_1.mod)
plot(GULD_behav_merged.SIA.ready$PC1_1, GULD_behav_merged.SIA.ready$d13C)

GULD_C.PC2_1.mod <- lmer(d13C ~ PC2_1 + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.PC2_1.mod)
confint(GULD_C.PC2_1.mod)
plot(GULD_C.PC2_1.mod)
plot(GULD_behav_merged.SIA.ready$PC2_1, GULD_behav_merged.SIA.ready$d13C)



#Summary:
#Significant effect of centretime.lnplus1_1 on d13C
#propmoving.exp looks like there may be a trend in d13C, but for one major outlier


### 7.4. Effects of 3 trial averages of behavioural variables ----

#Behavioural variables to test for effects on d15N and d13C
#     avespeed_tot_mean
#     avespeed_mob_mean
#     aveacceler_mean
#     dist_mean
#     timefrozen_tot.ln_mean
#     propmoving.exp_mean
#     centretime.lnplus1_mean
#     emergelat.bin.B_mean
#     endpointlat.bin.B_mean
#     PC1_mean
#     PC2_mean


#d15N
GULD_N.avespeed_tot_mean.mod <- lmer(d15N ~ avespeed_tot_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.avespeed_tot_mean.mod)
confint(GULD_N.avespeed_tot_mean.mod)
plot(GULD_N.avespeed_tot_mean.mod)
plot(GULD_behav_merged.SIA.ready$avespeed_tot_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.avespeed_mob_mean.mod <- lmer(d15N ~ avespeed_mob_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.avespeed_mob_mean.mod)
confint(GULD_N.avespeed_mob_mean.mod)
plot(GULD_N.avespeed_mob_mean.mod)
plot(GULD_behav_merged.SIA.ready$avespeed_mob_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.aveacceler_mean.mod <- lmer(d15N ~ aveacceler_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.aveacceler_mean.mod)
confint(GULD_N.aveacceler_mean.mod)
plot(GULD_N.aveacceler_mean.mod)
plot(GULD_behav_merged.SIA.ready$aveacceler_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.dist_mean.mod <- lmer(d15N ~ dist_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.dist_mean.mod)
confint(GULD_N.dist_mean.mod)
plot(GULD_N.dist_mean.mod)
plot(GULD_behav_merged.SIA.ready$dist_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.timefrozen_tot.ln_mean.mod <- lmer(d15N ~ timefrozen_tot.ln_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.timefrozen_tot.ln_mean.mod)
confint(GULD_N.timefrozen_tot.ln_mean.mod)
plot(GULD_N.timefrozen_tot.ln_mean.mod)
plot(GULD_behav_merged.SIA.ready$timefrozen_tot.ln_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.propmoving.exp_mean.mod <- lmer(d15N ~ propmoving.exp_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.propmoving.exp_mean.mod) #close to signficant
confint(GULD_N.propmoving.exp_mean.mod)
plot(GULD_N.propmoving.exp_mean.mod)
plot(GULD_behav_merged.SIA.ready$propmoving.exp_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.centretime.lnplus1_mean.mod <- lmer(d15N ~ centretime.lnplus1_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.centretime.lnplus1_mean.mod)
confint(GULD_N.centretime.lnplus1_mean.mod)
plot(GULD_N.centretime.lnplus1_mean.mod)
plot(GULD_behav_merged.SIA.ready$centretime.lnplus1_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.emergelat.bin.B_mean.mod <- lmer(d15N ~ emergelat.bin.B_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.emergelat.bin.B_mean.mod)
confint(GULD_N.emergelat.bin.B_mean.mod)
plot(GULD_N.emergelat.bin.B_mean.mod)
plot(GULD_behav_merged.SIA.ready$emergelat.bin.B_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.endpointlat.bin.B_mean.mod <- lmer(d15N ~ endpointlat.bin.B_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.endpointlat.bin.B_mean.mod)
confint(GULD_N.endpointlat.bin.B_mean.mod)
plot(GULD_N.endpointlat.bin.B_mean.mod)
plot(GULD_behav_merged.SIA.ready$endpointlat.bin.B_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.PC1_mean.mod <- lmer(d15N ~ PC1_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.PC1_mean.mod)
confint(GULD_N.PC1_mean.mod)
plot(GULD_N.PC1_mean.mod)
plot(GULD_behav_merged.SIA.ready$PC1_mean, GULD_behav_merged.SIA.ready$d15N)

GULD_N.PC2_mean.mod <- lmer(d15N ~ PC2_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_N.PC2_mean.mod)
confint(GULD_N.PC2_mean.mod)
plot(GULD_N.PC2_mean.mod)
plot(GULD_behav_merged.SIA.ready$PC2_mean, GULD_behav_merged.SIA.ready$d15N)


#d13C
GULD_C.avespeed_tot_mean.mod <- lmer(d13C ~ avespeed_tot_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.avespeed_tot_mean.mod)
confint(GULD_C.avespeed_tot_mean.mod)
plot(GULD_C.avespeed_tot_mean.mod)
plot(GULD_behav_merged.SIA.ready$avespeed_tot_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.avespeed_mob_mean.mod <- lmer(d13C ~ avespeed_mob_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.avespeed_mob_mean.mod) #Marginally significant positive effect
confint(GULD_C.avespeed_mob_mean.mod)
plot(GULD_C.avespeed_mob_mean.mod)
plot(GULD_behav_merged.SIA.ready$avespeed_mob_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.aveacceler_mean.mod <- lmer(d13C ~ aveacceler_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.aveacceler_mean.mod)
confint(GULD_C.aveacceler_mean.mod)
plot(GULD_C.aveacceler_mean.mod)
plot(GULD_behav_merged.SIA.ready$aveacceler_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.dist_mean.mod <- lmer(d13C ~ dist_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.dist_mean.mod)
confint(GULD_C.dist_mean.mod)
plot(GULD_C.dist_mean.mod)
plot(GULD_behav_merged.SIA.ready$dist_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.timefrozen_tot.ln_mean.mod <- lmer(d13C ~ timefrozen_tot.ln_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.timefrozen_tot.ln_mean.mod)
confint(GULD_C.timefrozen_tot.ln_mean.mod)
plot(GULD_C.timefrozen_tot.ln_mean.mod)
plot(GULD_behav_merged.SIA.ready$timefrozen_tot.ln_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.propmoving.exp_mean.mod <- lmer(d13C ~ propmoving.exp_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.propmoving.exp_mean.mod)
confint(GULD_C.propmoving.exp_mean.mod)
plot(GULD_C.propmoving.exp_mean.mod)
plot(GULD_behav_merged.SIA.ready$propmoving.exp_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.centretime.lnplus1_mean.mod <- lmer(d13C ~ centretime.lnplus1_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.centretime.lnplus1_mean.mod) 
confint(GULD_C.centretime.lnplus1_mean.mod)
plot(GULD_C.centretime.lnplus1_mean.mod)
plot(GULD_behav_merged.SIA.ready$centretime.lnplus1_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.emergelat.bin.B_mean.mod <- lmer(d13C ~ emergelat.bin.B_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.emergelat.bin.B_mean.mod)
confint(GULD_C.emergelat.bin.B_mean.mod)
plot(GULD_C.emergelat.bin.B_mean.mod)
plot(GULD_behav_merged.SIA.ready$emergelat.bin.B_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.endpointlat.bin.B_mean.mod <- lmer(d13C ~ endpointlat.bin.B_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.endpointlat.bin.B_mean.mod)
confint(GULD_C.endpointlat.bin.B_mean.mod)
plot(GULD_C.endpointlat.bin.B_mean.mod)
plot(GULD_behav_merged.SIA.ready$endpointlat.bin.B_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.PC1_mean.mod <- lmer(d13C ~ PC1_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.PC1_mean.mod)
confint(GULD_C.PC1_mean.mod)
plot(GULD_C.PC1_mean.mod)
plot(GULD_behav_merged.SIA.ready$PC1_mean, GULD_behav_merged.SIA.ready$d13C)

GULD_C.PC2_mean.mod <- lmer(d13C ~ PC2_mean + (1|FishID), data=GULD_behav_merged.SIA.ready)
summary(GULD_C.PC2_mean.mod)
confint(GULD_C.PC2_mean.mod)
plot(GULD_C.PC2_mean.mod)
plot(GULD_behav_merged.SIA.ready$PC2_mean, GULD_behav_merged.SIA.ready$d13C)



#Summary:
#No significant effect of behaviour
#Marginally significant positive effect of aveacceler on d13C



### 7.5. Effects of Trial 1 behavioural variables (Excluding SIA outlier, G34)----
GULD_behav_merged.SIA.excl <- subset(GULD_behav_merged.SIA.ready, FishID != 'G34')


#d15N
GULD_N.avespeed_tot_1.mod <- lmer(d15N ~ avespeed_tot_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.avespeed_tot_1.mod)
confint(GULD_N.avespeed_tot_1.mod)
plot(GULD_N.avespeed_tot_1.mod)
plot(GULD_behav_merged.SIA.excl$avespeed_tot_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.avespeed_mob_1.mod <- lmer(d15N ~ avespeed_mob_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.avespeed_mob_1.mod)
confint(GULD_N.avespeed_mob_1.mod)
plot(GULD_N.avespeed_mob_1.mod)
plot(GULD_behav_merged.SIA.excl$avespeed_mob_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.aveacceler_1.mod <- lmer(d15N ~ aveacceler_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.aveacceler_1.mod)
confint(GULD_N.aveacceler_1.mod)
plot(GULD_N.aveacceler_1.mod)
plot(GULD_behav_merged.SIA.excl$aveacceler_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.dist_1.mod <- lmer(d15N ~ dist_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.dist_1.mod)
confint(GULD_N.dist_1.mod)
plot(GULD_N.dist_1.mod)
plot(GULD_behav_merged.SIA.excl$dist_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.timefrozen_tot.ln_1.mod <- lmer(d15N ~ timefrozen_tot.ln_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.timefrozen_tot.ln_1.mod)
confint(GULD_N.timefrozen_tot.ln_1.mod)
plot(GULD_N.timefrozen_tot.ln_1.mod)
plot(GULD_behav_merged.SIA.excl$timefrozen_tot.ln_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.propmoving.exp_1.mod <- lmer(d15N ~ propmoving.exp_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.propmoving.exp_1.mod) #significant positive effect
confint(GULD_N.propmoving.exp_1.mod)
plot(GULD_N.propmoving.exp_1.mod)
plot(GULD_behav_merged.SIA.excl$propmoving.exp_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.centretime.lnplus1_1.mod <- lmer(d15N ~ centretime.lnplus1_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.centretime.lnplus1_1.mod)
confint(GULD_N.centretime.lnplus1_1.mod)
plot(GULD_N.centretime.lnplus1_1.mod)
plot(GULD_behav_merged.SIA.excl$centretime.lnplus1_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.emergelat.bin.B_1.mod <- lmer(d15N ~ emergelat.bin.B_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.emergelat.bin.B_1.mod)
confint(GULD_N.emergelat.bin.B_1.mod)
plot(GULD_N.emergelat.bin.B_1.mod)
plot(GULD_behav_merged.SIA.excl$emergelat.bin.B_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.endpointlat.bin.B_1.mod <- lmer(d15N ~ endpointlat.bin.B_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.endpointlat.bin.B_1.mod)
confint(GULD_N.endpointlat.bin.B_1.mod)
plot(GULD_N.endpointlat.bin.B_1.mod)
plot(GULD_behav_merged.SIA.excl$endpointlat.bin.B_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.PC1_1.mod <- lmer(d15N ~ PC1_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.PC1_1.mod)
confint(GULD_N.PC1_1.mod)
plot(GULD_N.PC1_1.mod)
plot(GULD_behav_merged.SIA.excl$PC1_1, GULD_behav_merged.SIA.excl$d15N)

GULD_N.PC2_1.mod <- lmer(d15N ~ PC2_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.PC2_1.mod)
confint(GULD_N.PC2_1.mod)
plot(GULD_N.PC2_1.mod)
plot(GULD_behav_merged.SIA.excl$PC2_1, GULD_behav_merged.SIA.excl$d15N)


#d13C
GULD_C.avespeed_tot_1.mod <- lmer(d13C ~ avespeed_tot_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.avespeed_tot_1.mod)
confint(GULD_C.avespeed_tot_1.mod)
plot(GULD_C.avespeed_tot_1.mod)
plot(GULD_behav_merged.SIA.excl$avespeed_tot_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.avespeed_mob_1.mod <- lmer(d13C ~ avespeed_mob_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.avespeed_mob_1.mod)
confint(GULD_C.avespeed_mob_1.mod)
plot(GULD_C.avespeed_mob_1.mod)
plot(GULD_behav_merged.SIA.excl$avespeed_mob_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.aveacceler_1.mod <- lmer(d13C ~ aveacceler_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.aveacceler_1.mod)
confint(GULD_C.aveacceler_1.mod)
plot(GULD_C.aveacceler_1.mod)
plot(GULD_behav_merged.SIA.excl$aveacceler_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.dist_1.mod <- lmer(d13C ~ dist_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.dist_1.mod)
confint(GULD_C.dist_1.mod)
plot(GULD_C.dist_1.mod)
plot(GULD_behav_merged.SIA.excl$dist_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.timefrozen_tot.ln_1.mod <- lmer(d13C ~ timefrozen_tot.ln_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.timefrozen_tot.ln_1.mod)
confint(GULD_C.timefrozen_tot.ln_1.mod)
plot(GULD_C.timefrozen_tot.ln_1.mod)
plot(GULD_behav_merged.SIA.excl$timefrozen_tot.ln_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.propmoving.exp_1.mod <- lmer(d13C ~ propmoving.exp_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.propmoving.exp_1.mod)
confint(GULD_C.propmoving.exp_1.mod)
plot(GULD_C.propmoving.exp_1.mod)
plot(GULD_behav_merged.SIA.excl$propmoving.exp_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.centretime.lnplus1_1.mod <- lmer(d13C ~ centretime.lnplus1_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.centretime.lnplus1_1.mod) #significant negative effect of centre area use and d13C
confint(GULD_C.centretime.lnplus1_1.mod)
plot(GULD_C.centretime.lnplus1_1.mod)
plot(GULD_behav_merged.SIA.excl$centretime.lnplus1_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.emergelat.bin.B_1.mod <- lmer(d13C ~ emergelat.bin.B_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.emergelat.bin.B_1.mod)
confint(GULD_C.emergelat.bin.B_1.mod)
plot(GULD_C.emergelat.bin.B_1.mod)
plot(GULD_behav_merged.SIA.excl$emergelat.bin.B_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.endpointlat.bin.B_1.mod <- lmer(d13C ~ endpointlat.bin.B_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.endpointlat.bin.B_1.mod)
confint(GULD_C.endpointlat.bin.B_1.mod)
plot(GULD_C.endpointlat.bin.B_1.mod)
plot(GULD_behav_merged.SIA.excl$endpointlat.bin.B_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.PC1_1.mod <- lmer(d13C ~ PC1_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.PC1_1.mod)
confint(GULD_C.PC1_1.mod)
plot(GULD_C.PC1_1.mod)
plot(GULD_behav_merged.SIA.excl$PC1_1, GULD_behav_merged.SIA.excl$d13C)

GULD_C.PC2_1.mod <- lmer(d13C ~ PC2_1 + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.PC2_1.mod)
confint(GULD_C.PC2_1.mod)
plot(GULD_C.PC2_1.mod)
plot(GULD_behav_merged.SIA.excl$PC2_1, GULD_behav_merged.SIA.excl$d13C)



#Summary:
#Marginally significant effects of some activity variables on d15N, signfificant effect of propmoving.exp_1 on d15N
#Significant effect of some centre area use variables on d13C



### 7.6. Effects of 3 trial averages of behavioural variables (Excluding SIA outlier, G34)----

#d15N
GULD_N.avespeed_tot_mean.mod <- lmer(d15N ~ avespeed_tot_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.avespeed_tot_mean.mod)
confint(GULD_N.avespeed_tot_mean.mod)
plot(GULD_N.avespeed_tot_mean.mod)
plot(GULD_behav_merged.SIA.excl$avespeed_tot_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.avespeed_mob_mean.mod <- lmer(d15N ~ avespeed_mob_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.avespeed_mob_mean.mod)
confint(GULD_N.avespeed_mob_mean.mod)
plot(GULD_N.avespeed_mob_mean.mod)
plot(GULD_behav_merged.SIA.excl$avespeed_mob_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.aveacceler_mean.mod <- lmer(d15N ~ aveacceler_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.aveacceler_mean.mod)
confint(GULD_N.aveacceler_mean.mod)
plot(GULD_N.aveacceler_mean.mod)
plot(GULD_behav_merged.SIA.excl$aveacceler_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.dist_mean.mod <- lmer(d15N ~ dist_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.dist_mean.mod)
confint(GULD_N.dist_mean.mod)
plot(GULD_N.dist_mean.mod)
plot(GULD_behav_merged.SIA.excl$dist_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.timefrozen_tot.ln_mean.mod <- lmer(d15N ~ timefrozen_tot.ln_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.timefrozen_tot.ln_mean.mod)
confint(GULD_N.timefrozen_tot.ln_mean.mod)
plot(GULD_N.timefrozen_tot.ln_mean.mod)
plot(GULD_behav_merged.SIA.excl$timefrozen_tot.ln_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.propmoving.exp_mean.mod <- lmer(d15N ~ propmoving.exp_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.propmoving.exp_mean.mod) #close to signficant
confint(GULD_N.propmoving.exp_mean.mod)
plot(GULD_N.propmoving.exp_mean.mod)
plot(GULD_behav_merged.SIA.excl$propmoving.exp_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.centretime.lnplus1_mean.mod <- lmer(d15N ~ centretime.lnplus1_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.centretime.lnplus1_mean.mod)
confint(GULD_N.centretime.lnplus1_mean.mod)
plot(GULD_N.centretime.lnplus1_mean.mod)
plot(GULD_behav_merged.SIA.excl$centretime.lnplus1_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.emergelat.bin.B_mean.mod <- lmer(d15N ~ emergelat.bin.B_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.emergelat.bin.B_mean.mod)
confint(GULD_N.emergelat.bin.B_mean.mod)
plot(GULD_N.emergelat.bin.B_mean.mod)
plot(GULD_behav_merged.SIA.excl$emergelat.bin.B_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.endpointlat.bin.B_mean.mod <- lmer(d15N ~ endpointlat.bin.B_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.endpointlat.bin.B_mean.mod)
confint(GULD_N.endpointlat.bin.B_mean.mod)
plot(GULD_N.endpointlat.bin.B_mean.mod)
plot(GULD_behav_merged.SIA.excl$endpointlat.bin.B_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.PC1_mean.mod <- lmer(d15N ~ PC1_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.PC1_mean.mod)
confint(GULD_N.PC1_mean.mod)
plot(GULD_N.PC1_mean.mod)
plot(GULD_behav_merged.SIA.excl$PC1_mean, GULD_behav_merged.SIA.excl$d15N)

GULD_N.PC2_mean.mod <- lmer(d15N ~ PC2_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_N.PC2_mean.mod)
confint(GULD_N.PC2_mean.mod)
plot(GULD_N.PC2_mean.mod)
plot(GULD_behav_merged.SIA.excl$PC2_mean, GULD_behav_merged.SIA.excl$d15N)


#d13C
GULD_C.avespeed_tot_mean.mod <- lmer(d13C ~ avespeed_tot_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.avespeed_tot_mean.mod)
confint(GULD_C.avespeed_tot_mean.mod)
plot(GULD_C.avespeed_tot_mean.mod)
plot(GULD_behav_merged.SIA.excl$avespeed_tot_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.avespeed_mob_mean.mod <- lmer(d13C ~ avespeed_mob_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.avespeed_mob_mean.mod) #Marginally significant positive effect
confint(GULD_C.avespeed_mob_mean.mod)
plot(GULD_C.avespeed_mob_mean.mod)
plot(GULD_behav_merged.SIA.excl$avespeed_mob_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.aveacceler_mean.mod <- lmer(d13C ~ aveacceler_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.aveacceler_mean.mod)
confint(GULD_C.aveacceler_mean.mod)
plot(GULD_C.aveacceler_mean.mod)
plot(GULD_behav_merged.SIA.excl$aveacceler_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.dist_mean.mod <- lmer(d13C ~ dist_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.dist_mean.mod)
confint(GULD_C.dist_mean.mod)
plot(GULD_C.dist_mean.mod)
plot(GULD_behav_merged.SIA.excl$dist_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.timefrozen_tot.ln_mean.mod <- lmer(d13C ~ timefrozen_tot.ln_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.timefrozen_tot.ln_mean.mod)
confint(GULD_C.timefrozen_tot.ln_mean.mod)
plot(GULD_C.timefrozen_tot.ln_mean.mod)
plot(GULD_behav_merged.SIA.excl$timefrozen_tot.ln_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.propmoving.exp_mean.mod <- lmer(d13C ~ propmoving.exp_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.propmoving.exp_mean.mod)
confint(GULD_C.propmoving.exp_mean.mod)
plot(GULD_C.propmoving.exp_mean.mod)
plot(GULD_behav_merged.SIA.excl$propmoving.exp_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.centretime.lnplus1_mean.mod <- lmer(d13C ~ centretime.lnplus1_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.centretime.lnplus1_mean.mod) 
confint(GULD_C.centretime.lnplus1_mean.mod)
plot(GULD_C.centretime.lnplus1_mean.mod)
plot(GULD_behav_merged.SIA.excl$centretime.lnplus1_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.emergelat.bin.B_mean.mod <- lmer(d13C ~ emergelat.bin.B_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.emergelat.bin.B_mean.mod)
confint(GULD_C.emergelat.bin.B_mean.mod)
plot(GULD_C.emergelat.bin.B_mean.mod)
plot(GULD_behav_merged.SIA.excl$emergelat.bin.B_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.endpointlat.bin.B_mean.mod <- lmer(d13C ~ endpointlat.bin.B_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.endpointlat.bin.B_mean.mod)
confint(GULD_C.endpointlat.bin.B_mean.mod)
plot(GULD_C.endpointlat.bin.B_mean.mod)
plot(GULD_behav_merged.SIA.excl$endpointlat.bin.B_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.PC1_mean.mod <- lmer(d13C ~ PC1_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.PC1_mean.mod)
confint(GULD_C.PC1_mean.mod)
plot(GULD_C.PC1_mean.mod)
plot(GULD_behav_merged.SIA.excl$PC1_mean, GULD_behav_merged.SIA.excl$d13C)

GULD_C.PC2_mean.mod <- lmer(d13C ~ PC2_mean + (1|FishID), data=GULD_behav_merged.SIA.excl)
summary(GULD_C.PC2_mean.mod)
confint(GULD_C.PC2_mean.mod)
plot(GULD_C.PC2_mean.mod)
plot(GULD_behav_merged.SIA.excl$PC2_mean, GULD_behav_merged.SIA.excl$d13C)


#Marginally significant effects of some activity variables on d15N
#Marginally significant effects of some centre area use variables on d13C
