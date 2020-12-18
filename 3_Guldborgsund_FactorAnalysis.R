# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



Sys.setenv(LANG = "en")
library(dplyr); library(ggplot2); library(ggpubr); library(lme4); library(lmerTest); library(rptR)



#3. Exploring Factor Effects on Behaviour (GULD) ----

#  Behavioral variables used in analysis:
#    avespeed_tot
#    avespeed_mob
#    aveacceler
#    propmoving.exp
#    dist
#    timefrozen_tot.ln
#    centretime.lnplus1
#    emergelat.bin
#    endpointlat.bin
#    endpointspeed.ln
#    refugereturnlat.ln

GULD_ACT.processed <- read.csv('~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis/GULD_ACT.processing.csv')
GULD_EXPL.processed <- read.csv('~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis/GULD_EXPL.processing.csv')

### 3.1. Systematic factors influencing behavioural variation ----
#Experimental control predictors 
#    TrialDay
#    TrialRound
#    ArenaID
#    TankID



#ACT variables
#Currently running trial day and round as covariates
#GULD_ACT.processed$TrialDay <- as.factor(GULD_ACT.processed$TrialDay)
#GULD_ACT.processed$TrialRound <- as.factor(GULD_ACT.processed$TrialRound)


GULD_avespeed_tot.mod.sysfact <- lmer(avespeed_tot ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                      data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.sysfact)
summary(GULD_avespeed_tot.mod.sysfact) 
#TrialDay: significant decrease over trial days
#TrialRound: no effect
#ArenaID: Arena C significantly lower, Arena G marginally lower
#TankID: no effect of holding tank

GULD_avespeed_mob.mod.sysfact <- lmer(avespeed_mob ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                      data=GULD_ACT.processed)
plot(GULD_avespeed_mob.mod.sysfact)
summary(GULD_avespeed_mob.mod.sysfact) 
#TrialDay: significant decrease over trial days 1-3
#TrialRound: no effect
#ArenaID: no effect
#TankID: no effect

GULD_aveacceler.mod.sysfact <- lmer(aveacceler ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                    data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.sysfact)
summary(GULD_aveacceler.mod.sysfact) 
#TrialDay: significant decrease over trial days 1-3
#TrialRound: no effect
#ArenaID: Arena C and G significantly lower, Arena F marginally lower
#TankID: no effect

GULD_propmoving.exp.mod.sysfact <- lmer(propmoving.exp ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                        data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.sysfact)
summary(GULD_propmoving.exp.mod.sysfact) 
#TrialDay: significant decrease over trial days 1-3
#TrialRound: no effect
#ArenaID: Arena C and G significantly lower, Arena F marginally lower
#TankID: no effect

GULD_dist.mod.sysfact <- lmer(dist ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                              data=GULD_ACT.processed)
plot(GULD_dist.mod.sysfact)
summary(GULD_dist.mod.sysfact) 
#TrialDay: significant decrease over trial days 1-3
#TrialRound: no effect
#ArenaID: Arena C significantly lower, Arena F and G marginally lower
#TankID: no effect

GULD_timefrozen_tot.ln.mod.sysfact <- lmer(timefrozen_tot.ln ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                           data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.sysfact)
summary(GULD_timefrozen_tot.ln.mod.sysfact) 
#TrialDay: significant increase over trial days 1-3
#TrialRound: no effect
#ArenaID: Arena C, F and G significantly higher, Arena B marginally higher
#TankID: no effect

GULD_centretime.lnplus1.mod.sysfact <- lmer(centretime.lnplus1 ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                            data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.sysfact)
summary(GULD_centretime.lnplus1.mod.sysfact) 
#TrialDay: no effect
#TrialRound: no effect
#ArenaID: Arena C and G significantly lower
#TankID: no effect


#ACT Summary: 
#    TrialDay: strong decreases in activity across all variables (excl. centre time)
#    TrialRound: no effect
#    ArenaID: appears to be some issues with C, G, F, potentially B (these are central tanks, so may be considered more exposed, or a calibration issue??)
#    TankID: no effect



#EXPL variables

#Currently running Trial day and round as covariates
#GULD_EXPL.processed$TrialDay <- as.factor(GULD_EXPL.processed$TrialDay)
#GULD_EXPL.processed$TrialRound <- as.factor(GULD_EXPL.processed$TrialRound)


GULD_emergelat.bin.mod.sysfac <- glmer(emergelat.bin ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                       family=binomial, 
                                       data=GULD_EXPL.processed,
                                       control=glmerControl(optimizer="bobyqa",
                                                            optCtrl=list(maxfun=2e5)))
summary(GULD_emergelat.bin.mod.sysfac) 
#TrialDay: no effect
#TrialRound: no effect
#ArenaID: no effect
#TankID: no effect

GULD_endpointlat.bin.mod.sysfac <- glmer(endpointlat.bin ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                       family=binomial, 
                                       data=GULD_EXPL.processed,
                                       control=glmerControl(optimizer="bobyqa",
                                                            optCtrl=list(maxfun=2e5)))
summary(GULD_endpointlat.bin.mod.sysfac) 
#TrialDay: no effect
#TrialRound: no effect
#ArenaID: no effect
#TankID: no effect

GULD_endpointspeed.ln.mod.sysfac <- lmer(endpointspeed.ln ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                            data=GULD_EXPL.processed)
plot(GULD_endpointspeed.ln.mod.sysfac)
summary(GULD_endpointspeed.ln.mod.sysfac) 
#TrialDay: no effect
#TrialRound: no effect
#ArenaID: no effect
#TankID: no effect

GULD_refugereturnlat.ln.mod.sysfac <- lmer(refugereturnlat.ln ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                           data=GULD_EXPL.processed)
plot(GULD_refugereturnlat.ln.mod.sysfac)
summary(GULD_refugereturnlat.ln.mod.sysfac)
#TrialDay: decrease in return latency over trial days
#TrialRound: decrease in return latency over trial days
#ArenaID: no effect
#TankID: no effect


#EXPL Summary: 
#    - Only systematic effect is on refuge return latency, where fish returned faster over the trial days, and over trial rounds.



### 3.2. Fish state factors influencing behavioural variation ----
#  Fish state predictors
#    Sex
#    TL
#    Condition factor
#    InfectionScore


#ACT variables
GULD_ACT.processed$Sex[GULD_ACT.processed$Sex == "m(j)"] <- "m" #grouped as male v female, only two males were noted as juvenile
#Note: sample heavily biased towards males (consistent with other studies)
n_distinct(subset(GULD_ACT.processed, Sex == 'f')$FishID) #n = 6
n_distinct(subset(GULD_ACT.processed, Sex == 'm')$FishID) #n = 37

#As there is a strong correlation between TL and Weight, only TL has been used in analysis
summary(GULD_ACT.processed$TL)
#Range from 9.5 - 17.30 cm
summary(GULD_ACT.processed$Weight)
#Range from 10 - 70 g
plot(GULD_ACT.processed$TL, GULD_ACT.processed$Weight)

#Calculating a fultons condition factor score
GULD_ACT.processed$ConditionFactor <- 100* (GULD_ACT.processed$Weight / ((GULD_ACT.processed$TL)^3))

#Infection Score 
#Score of 1-3: 
#1 = 0 - 10% of fins with ectoparasite/fungal infection
#2 = 10 - 50% of fins with ectoparasite/fungal infection
#3 = 50 - 100% of fins with ectoparasite/fungal infection


GULD_avespeed_tot.mod.stafac <- lmer(avespeed_tot ~  Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                  data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.stafac)
summary(GULD_avespeed_tot.mod.stafac) 
#Sex: significantly higher in males
#TL: no effect
#Condition factor: marginally positive effect
#InfectionScore: no effect

GULD_avespeed_mob.mod.stafac <- lmer(avespeed_mob ~  Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                  data=GULD_ACT.processed)
plot(GULD_avespeed_mob.mod.stafac)
summary(GULD_avespeed_mob.mod.stafac) 
#Sex: no effect
#TL: no effect
#Condition factor: no effect
#InfectionScore: no effect

GULD_aveacceler.mod.stafac <- lmer(aveacceler ~  Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.stafac)
summary(GULD_aveacceler.mod.stafac) 
#Sex: significantly higher in males
#TL: no effect
#Condition factor: no effect
#InfectionScore: no effect

GULD_propmoving.exp.mod.stafac <- lmer(propmoving.exp ~  Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                    data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.stafac)
summary(GULD_propmoving.exp.mod.stafac) 
#Sex: significantly higher in males
#TL: no effect
#Condition factor: significant positive effect
#InfectionScore: no effect

GULD_dist.mod.stafac <- lmer(dist ~  Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                          data=GULD_ACT.processed)
plot(GULD_dist.mod.stafac)
summary(GULD_dist.mod.stafac) 
#Sex: significantly higher in males
#TL: no effect
#Condition factor: marginally positive effect
#InfectionScore: no effect

GULD_timefrozen_tot.ln.mod.stafac <- lmer(timefrozen_tot.ln ~  Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                       data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.stafac)
summary(GULD_timefrozen_tot.ln.mod.stafac) 
#Sex: marginally lower in males
#TL: no effect
#Condition factor: significantly negative effect
#InfectionScore: significantly positive effect

GULD_centretime.lnplus1.mod.stafac <- lmer(centretime.lnplus1 ~  Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                           data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.stafac)
summary(GULD_centretime.lnplus1.mod.stafac) 
#Sex: no effect
#TL: no effect
#Condition factor: no effect
#InfectionScore: no effect


#ACT Summary: 
#    - Sex: Strong differences where males appear more active in most variables, marginal effect for timefrozen_tot.ln, no effect for centretime.lnplus1, avespeed_mob
#    - TL: No signficant effect
#    - ConditionFactor: positive for propmoving.exp, marginally positive for avespeed_tot and dist, negative effect for timefrozen_tot.ln
#    - InfectionScore: positive efect for timefrozen_tot.ln



#EXPL variables
GULD_EXPL.processed$Sex[GULD_EXPL.processed$Sex == "m(j)"] <- "m" #grouped as male v female, only two males were noted as juvenile

#Note: sample heavily biased towards males (consistent with other studies)
n_distinct(subset(GULD_EXPL.processed, Sex == 'f')$FishID) #n = 6
n_distinct(subset(GULD_EXPL.processed, Sex == 'm')$FishID) #n = 37

#TL range
summary(GULD_EXPL.processed$TL)
#Range from 9.5 - 17.30 cm

#Adding weight data from ACT dataframe to this dataframe
GULD_EXPL.processed$mergeID <- paste(GULD_EXPL.processed$FishID, GULD_EXPL.processed$TrialDay, sep = '')
GULD_ACT.working <- GULD_ACT.processed
GULD_ACT.working$mergeID <- paste(GULD_ACT.working$FishID, GULD_ACT.working$TrialDay, sep = '')
GULD_ACT.working <- select(GULD_ACT.working, -c(X, TimeLoaded, TrialRound, timefrozen_tot, timefrozen_ave, centretime, 
                                                PITID, FishID, TL, InfectionScore, Sex, Date, dist, timefrozen_tot.ln, 
                                                propmoving.exp, centretime.lnplus1, TrialType, TrialDay, ArenaID, TankID, 
                                                avespeed_tot, avespeed_mob, aveacceler, propmoving, Notes, UniqueID))
GULD_EXPL.processed <- merge(GULD_EXPL.processed, GULD_ACT.working, by = 'mergeID', all.x = TRUE)
GULD_EXPL.processed <- select(GULD_EXPL.processed, -c(mergeID))
nrow(GULD_EXPL.processed) #113 trials
#Calculating a fultons condition factor score
GULD_EXPL.processed$ConditionFactor <- 100* (GULD_EXPL.processed$Weight / ((GULD_EXPL.processed$TL)^3))



GULD_emergelat.bin.mod.stafac <- glmer(emergelat.bin ~ Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                       family=binomial, 
                                       data=GULD_EXPL.processed,
                                       control=glmerControl(optimizer="bobyqa",
                                                            optCtrl=list(maxfun=2e5)))
summary(GULD_emergelat.bin.mod.stafac) 
#Sex: no effect
#TL: no effect
#Condition factor: no effect
#InfectionScore: no effect

GULD_endpointlat.bin.mod.stafac <- glmer(endpointlat.bin ~ Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                        family=binomial, 
                                        data=GULD_EXPL.processed,
                                        control=glmerControl(optimizer="bobyqa",
                                                             optCtrl=list(maxfun=2e5)))
plot(GULD_endpointlat.bin.mod.stafac)
summary(GULD_endpointlat.bin.mod.stafac) 
#Sex: no effect
#TL: no effect
#Condition factor: no effect
#InfectionScore: no effect


GULD_endpointspeed.ln.mod.stafac <- lmer(endpointspeed.ln ~ Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                         data=GULD_EXPL.processed)
plot(GULD_endpointspeed.ln.mod.stafac)
summary(GULD_endpointspeed.ln.mod.stafac) 
#Sex: no effect
#TL: no effect
#Condition factor: no effect
#InfectionScore: no effect

GULD_refugereturnlat.ln.mod.stafac <- lmer(refugereturnlat.ln ~ Sex + TL + ConditionFactor + InfectionScore + (1|FishID), 
                                           data=GULD_EXPL.processed)
plot(GULD_refugereturnlat.ln.mod.stafac)
summary(GULD_refugereturnlat.ln.mod.stafac)
#Sex: no effect
#TL: no effect
#Condition factor: no effect
#InfectionScore: no effect


#EXPL Summary: 
#    - Sex: no effect
#    - TL: no effect
#    - ConditionFactor: no effect
#    - InfectionScore: no effect


labels(GULD_ACT.processed)
GULD_ACT.processed <- GULD_ACT.processed[, c(2,3,4,14,28,5,6,7,8,9,10,11,12,24,13,15,16,17,18,26,19,20,25,21,22,27,23)]
labels(GULD_ACT.processed)
write.csv(GULD_ACT.processed, '~/trophicpersonalities_A/3_Guldbordsund_FactorAnalysis/GULD_ACT.processed.csv')

labels(GULD_EXPL.processed)
GULD_EXPL.processed <- GULD_EXPL.processed[, c(2,3,4,28,29,5,6,7,8,9,10,11,12,13,23,14,15,16,24,17,18,25,19,26,20,21,27,22)]
labels(GULD_EXPL.processed)
write.csv(GULD_EXPL.processed, '~/trophicpersonalities_A/3_Guldbordsund_FactorAnalysis/GULD_EXPL.processed.csv')



### 3.3. Calculating adjusted repeatabilities for some variables ----
#  Additional factors included for each behavioural variable:
#    avespeed_tot       - TrialDay + ArenaID + Sex
#    avespeed_mob       - TrialDay
#    aveacceler         - TrialDay + ArenaID + Sex
#    propmoving.exp     - TrialDay + ArenaID + Sex + ConditionFactor
#    dist               - TrialDay + ArenaID + Sex
#    timefrozen_tot.ln  - TrialDay + ArenaID + ConditionFactor + InfectionScore
#    centretime.lnplus1 - ArenaID 
#    emergelat.bin.B    - nil
#    endpointlat.bin.B  - nil
#    endpointspeed.ln   - nil
#    refugereturnlat.ln - TrialDay + TrialRound



GULD_avespeed_tot.mod.adj <- lmer(avespeed_tot ~ TrialDay + ArenaID + Sex + (1|FishID), data=GULD_ACT.processed)
summary(GULD_avespeed_tot.mod.adj)
plot(GULD_avespeed_tot.mod.adj)
GULD_avespeed_tot.rpt.adj <- rpt(avespeed_tot ~ TrialDay + ArenaID + Sex + (1 | FishID), grname = "FishID", 
                                 data = GULD_ACT.processed, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_avespeed_tot.rpt.adj


GULD_avespeed_mob.mod.adj <- lmer(avespeed_mob ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
summary(GULD_avespeed_mob.mod.adj)
plot(GULD_avespeed_mob.mod.adj)
GULD_avespeed_mob.rpt.adj <- rpt(avespeed_mob ~ TrialDay + (1 | FishID), grname = "FishID", 
                                 data = GULD_ACT.processed, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_avespeed_mob.rpt.adj


GULD_aveacceler.mod.adj <- lmer(aveacceler ~  TrialDay + ArenaID + Sex + (1|FishID), 
                                data=GULD_ACT.processed)
summary(GULD_aveacceler.mod.adj)
plot(GULD_aveacceler.mod.adj)
GULD_aveacceler.rpt.adj <- rpt(aveacceler ~  TrialDay + ArenaID + Sex + (1 | FishID), grname = "FishID", 
                               data = GULD_ACT.processed, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
GULD_aveacceler.rpt.adj


GULD_propmoving.exp.mod.adj <- lmer(propmoving.exp ~ TrialDay + ArenaID + Sex + ConditionFactor + (1|FishID), data=GULD_ACT.processed)
summary(GULD_propmoving.exp.mod.adj)
plot(GULD_propmoving.exp.mod.adj)
GULD_propmoving.exp.rpt.adj <- rpt(propmoving.exp ~ TrialDay + ArenaID + Sex + ConditionFactor + (1 | FishID), grname = "FishID", 
                                   data = GULD_ACT.processed, datatype = "Gaussian", 
                                   nboot = 100, npermut = 0)
GULD_propmoving.exp.rpt.adj


GULD_dist.mod.adj <- lmer(dist ~  TrialDay + ArenaID + Sex + (1|FishID), data=GULD_ACT.processed)
summary(GULD_dist.mod.adj)
plot(GULD_dist.mod.adj)
GULD_dist.rpt.adj <- rpt(dist ~  TrialDay + ArenaID + Sex + (1 | FishID), grname = "FishID", 
                         data = GULD_ACT.processed, datatype = "Gaussian", 
                         nboot = 100, npermut = 0)
GULD_dist.rpt.adj


GULD_timefrozen_tot.ln.mod.adj <- lmer(timefrozen_tot.ln ~ TrialDay + ArenaID + ConditionFactor + InfectionScore + (1|FishID), 
                                       data=GULD_ACT.processed)
summary(GULD_timefrozen_tot.ln.mod.adj)
plot(GULD_timefrozen_tot.ln.mod.adj)
GULD_timefrozen_tot.ln.rpt.adj <- rpt(timefrozen_tot.ln ~ TrialDay + ArenaID + ConditionFactor + InfectionScore + (1 | FishID), grname = "FishID", 
                                      data = GULD_ACT.processed, datatype = "Gaussian", 
                                      nboot = 100, npermut = 0)
GULD_timefrozen_tot.ln.rpt.adj 


GULD_centretime.lnplus1.mod.adj <- lmer(centretime.lnplus1 ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
summary(GULD_centretime.lnplus1.mod.adj)
plot(GULD_centretime.lnplus1.mod.adj)
GULD_centretime.lnplus1.rpt.adj <- rpt(centretime.lnplus1 ~ ArenaID + (1 | FishID), grname = "FishID", 
                                       data = GULD_ACT.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_centretime.lnplus1.rpt.adj


GULD_refugereturnlat.ln.mod.adj <- lmer(refugereturnlat.ln ~ TrialDay + TrialRound + (1|FishID), data=GULD_EXPL.processed)
summary(GULD_refugereturnlat.ln.mod.adj)
plot(GULD_refugereturnlat.ln.mod.adj)
GULD_refugereturnlat.ln.rpt.adj <- rpt(refugereturnlat.ln ~ TrialDay + TrialRound + (1 | FishID), grname = "FishID", 
                                       data = GULD_EXPL.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_refugereturnlat.ln.rpt.adj


#Summary adjusted repeatabilities-
#avespeed_tot:      _ 0.643 [0.447, 0.768] ***       (unadjusted for comparison-   0.467 [0.266, 0.618] ***)
#avespeed_mob       _ 0.382 [0.164, 0.571] ***       (unadjusted for comparison-   0.181 [0, 0.374]     *  )
#aveacceler         _ 0.581 [0.453, 0.737] ***       (unadjusted for comparison-   0.473 [0.27, 0.63]   ***)
#propmoving.exp     _ 0.595 [0.437, 0.763] ***       (unadjusted for comparison-   0.531 [0.371, 0.683] ***)
#dist               _ 0.636 [0.464, 0.805] ***       (unadjusted for comparison-   0.468 [0.293, 0.596] ***)
#timefrozen_tot.ln  _ 0.468 [0.293, 0.668] ***       (unadjusted for comparison-   0.374 [0.134, 0.583] ***)
#centretime.lnplus1 _ 0.427 [0.248, 0.619] ***       (unadjusted for comparison-   0.43  [0.163, 0.63]  ***)
#emergelat.bin      _ NA                   NA        (unadjusted for comparison-   0.678 [0.355, 0.838] ***)
#endpointlat.bin    _ NA                   NA        (unadjusted for comparison-   0.626 [0.248, 0.804] ***)
#endpointspeed.ln   _ NA                   NA        (unadjusted for comparison-   0.222 [0, 0.489]     ns )
#refugereturnlat.ln _ 0.237 [0, 0.532]     ns        (unadjusted for comparison-   0.158 [0, 0.411]     ns )
   

