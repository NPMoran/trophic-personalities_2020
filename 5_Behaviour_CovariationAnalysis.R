# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG = "en")
library(dplyr); library(ggplot2)
library(vegan)



#5. Covariance and correlation analysis for behavioural data ----

#  Behavioral variables used  in analysis:
#    avespeed_tot
#    aveacceler
#    propmoving.exp
#    dist
#    timefrozen_tot.ln
#    centretime.lnplus1
#    emergelat.bin.B
#    endpointlat.bin.B

GULD_ACT.processed <- read.csv('~/trophicpersonalities_GULD/4_Behaviour_VarianceAnalysis/GULD_ACT.processed.csv')
GULD_EXPL.processed <- read.csv('~/trophicpersonalities_GULD/4_Behaviour_VarianceAnalysis/GULD_EXPL.processed.csv')

### 5.1. Systematic Factors influencings behavioural variation ----
#Experimental control predictors 
#    TrialDay
#    TrialRound
#    ArenaID
#    TankID

#ACT: TrialDay
GULD_avespeed_tot.mod.trialday <- lmer(avespeed_tot ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.trialday)
summary(GULD_avespeed_tot.mod.trialday) 
#avespeed_tot reduction over the three trial days
GULD_aveacceler.mod.trialday <- lmer(aveacceler ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.trialday)
summary(GULD_aveacceler.mod.trialday) 
#aveacceler reduction over the three trial days
GULD_propmoving.exp.mod.trialday <- lmer(propmoving.exp ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.trialday)
summary(GULD_propmoving.exp.mod.trialday) 
#propmoving.exp reduction over the three trial days
GULD_dist.mod.trialday <- lmer(dist ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.trialday)
summary(GULD_dist.mod.trialday) 
#dist reduction over the three trial days
GULD_timefrozen_tot.ln.mod.trialday <- lmer(timefrozen_tot.ln ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.trialday)
summary(GULD_timefrozen_tot.ln.mod.trialday) 
#timefrozen_tot.ln increase over the three trial days
GULD_centretime.lnplus1.mod.trialday <- lmer(centretime.lnplus1 ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.trialday)
summary(GULD_centretime.lnplus1.mod.trialday) 
#no effect of trial day on centretime.lnplus1

#ACT: TrialRound
GULD_avespeed_tot.mod.trialround <- lmer(avespeed_tot ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.trialround)
summary(GULD_avespeed_tot.mod.trialround) 
#no systematic differences due to the order of trials
GULD_aveacceler.mod.trialround <- lmer(aveacceler ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.trialround)
summary(GULD_aveacceler.mod.trialround) 
#no systematic differences due to the order of trials (although effect may be considered marginally significant)
GULD_propmoving.exp.mod.trialround <- lmer(propmoving.exp ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.trialround)
summary(GULD_propmoving.exp.mod.trialround) 
#no systematic differences due to the order of trials
GULD_dist.mod.trialround <- lmer(dist ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.trialround)
summary(GULD_dist.mod.trialround) 
#no systematic differences due to the order of trials
GULD_timefrozen_tot.ln.mod.trialround <- lmer(timefrozen_tot.ln ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.trialround)
summary(GULD_timefrozen_tot.ln.mod.trialround) 
#no systematic differences due to the order of trials
GULD_centretime.lnplus1.mod.trialround <- lmer(centretime.lnplus1 ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.trialround)
summary(GULD_centretime.lnplus1.mod.trialround) 
#no systematic differences due to the order of trials

#ACT: ArenaID
GULD_avespeed_tot.mod.arenaid <- lmer(avespeed_tot ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
summary(GULD_avespeed_tot.mod.arenaid)
plot(GULD_avespeed_tot.mod.arenaid)
#no systematic differences among arenas
GULD_aveacceler.mod.arenaid <- lmer(aveacceler ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.arenaid)
summary(GULD_aveacceler.mod.arenaid)
#no systematic differences among arenas
GULD_propmoving.exp.mod.arenaid <- lmer(propmoving.exp ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.arenaid)
summary(GULD_propmoving.exp.mod.arenaid)
#no systematic differences among arenas
GULD_dist.mod.arenaid <- lmer(dist ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.arenaid)
summary(GULD_dist.mod.arenaid)
#no systematic differences among arenas
GULD_timefrozen_tot.ln.mod.arenaid <- lmer(timefrozen_tot.ln ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.arenaid)
summary(GULD_timefrozen_tot.ln.mod.arenaid)
#no systematic differences among arenas
GULD_centretime.lnplus1.mod.arenaid <- lmer(centretime.lnplus1 ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.arenaid)
summary(GULD_centretime.lnplus1.mod.arenaid)
#appears the a negative effect on Arena C, and marginally significant negative effect on G

#ACT: TankID
GULD_avespeed_tot.mod.tankid <- lmer(avespeed_tot ~ TankID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.tankid)
summary(GULD_avespeed_tot.mod.tankid)
#no systematic differences among holding tanks
GULD_aveacceler.mod.tankid <- lmer(aveacceler ~ TankID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.tankid)
summary(GULD_aveacceler.mod.tankid)
#no systematic differences among holding tanks
GULD_propmoving.exp.mod.tankid <- lmer(propmoving.exp ~ TankID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.tankid)
summary(GULD_propmoving.exp.mod.tankid)
#no systematic differences among holding tanks
GULD_dist.mod.tankid <- lmer(dist ~ TankID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.tankid)
summary(GULD_dist.mod.tankid)
#no systematic differences among holding tanks
GULD_timefrozen_tot.ln.mod.tankid <- lmer(timefrozen_tot.ln ~ TankID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.tankid)
summary(GULD_timefrozen_tot.ln.mod.tankid)
#no systematic differences among holding tanks
GULD_centretime.lnplus1.mod.tankid <- lmer(centretime.lnplus1 ~ TankID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.tankid)
summary(GULD_centretime.lnplus1.mod.tankid)
#no systematic differences among holding tanks

#ACT Summary: 
#    - For all ACT variables (excl: centretime.lnplus1), there appears to be an effect of TrialDay, where fish become less active
#    - No other systematic effect of trial order (TrialRound), experimental arena (ArenaID), or holding tank (TankID)
#    - Arena ACT_C showed a signficantly lower centretime.lnplus1, although I don't think this indicated a systematic issue among Arena's


#EXPL: TrialDay
GULD_emergelat.bin.B.mod.trialday <- glmer(emergelat.bin.B ~ TrialDay + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_emergelat.bin.B.mod.trialday)
summary(GULD_emergelat.bin.B.mod.trialday) 
#no effect of trial day
GULD_endpointlat.bin.B.mod.trialday <- glmer(endpointlat.bin.B ~ TrialDay + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.trialday)
summary(GULD_endpointlat.bin.B.mod.trialday) 
#no effect of trial day

#EXPL: TrialRound
GULD_emergelat.bin.B.mod.trialround <- glmer(emergelat.bin.B ~ TrialRound + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_emergelat.bin.B.mod.trialround)
summary(GULD_emergelat.bin.B.mod.trialround) 
#no systematic differences due to the order of trials
GULD_endpointlat.bin.B.mod.trialround <- glmer(endpointlat.bin.B ~ TrialRound + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.trialround)
summary(GULD_endpointlat.bin.B.mod.trialround) 
#no systematic differences due to the order of trials

#EXPL: ArenaID
GULD_emergelat.bin.B.mod.arenaid <- glmer(emergelat.bin.B ~ ArenaID + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_emergelat.bin.B.mod.arenaid)
summary(GULD_emergelat.bin.B.mod.arenaid)
#no systematic differences among arenas
GULD_endpointlat.bin.B.mod.arenaid <- glmer(endpointlat.bin.B ~ ArenaID + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.arenaid)
summary(GULD_endpointlat.bin.B.mod.arenaid)
#no systematic differences among arenas

#EXPL: TankID
GULD_emergelat.bin.B.mod.tankid <- glmer(emergelat.bin.B ~ TankID + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_emergelat.bin.B.mod.tankid)
summary(GULD_emergelat.bin.B.mod.tankid)
#no systematic differences among holding tanks
GULD_endpointlat.bin.B.mod.tankid <- glmer(endpointlat.bin.B ~ TankID + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.tankid)
summary(GULD_endpointlat.bin.B.mod.tankid)
#no systematic differences among holding tanks

#EXPL Summary: 
#    - No systematic issues


### 5.2. Fish State Factors influencings behavioural variation ----
#  Fish state predictors
#    Sex
#    TL
#    Weight
#    InfectionScore


#ACT: Sex
GULD_ACT.processed$Sex[GULD_ACT.processed$Sex == "m(j)"] <- "m" #grouped as male v female, only two males were noted as juvenile

#Note: sample heavily biased towards males (consistent with other studies)
n_distinct(subset(GULD_ACT.processed, Sex == 'f')$FishID) #n = 6
n_distinct(subset(GULD_ACT.processed, Sex == 'm')$FishID) #n = 37

GULD_avespeed_tot.mod.sex <- lmer(avespeed_tot ~ Sex + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.sex)
summary(GULD_avespeed_tot.mod.sex) 
#positive effect for males
GULD_aveacceler.mod.sex <- lmer(aveacceler ~ Sex + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.sex)
summary(GULD_aveacceler.mod.sex) 
#positive effect for males
GULD_propmoving.exp.mod.sex <- lmer(propmoving.exp ~ Sex + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.sex)
summary(GULD_propmoving.exp.mod.sex) 
#positive effect for males
GULD_dist.mod.sex <- lmer(dist ~ Sex + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.sex)
summary(GULD_dist.mod.sex) 
#positive effect for males
GULD_timefrozen_tot.ln.mod.sex <- lmer(timefrozen_tot.ln ~ Sex + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.sex)
summary(GULD_timefrozen_tot.ln.mod.sex) 
#negative effect for males
GULD_centretime.lnplus1.mod.sex <- lmer(centretime.lnplus1 ~ Sex + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.sex)
summary(GULD_centretime.lnplus1.mod.sex) 
#no effect of sex


#ACT: Total Length (TL)
summary(GULD_ACT.processed$TL)
#Range from 9.5 - 17.30 cm

GULD_avespeed_tot.mod.tl <- lmer(avespeed_tot ~ TL + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.tl)
summary(GULD_avespeed_tot.mod.tl) 
#no effect of TL
GULD_aveacceler.mod.tl <- lmer(aveacceler ~ TL + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.tl)
summary(GULD_aveacceler.mod.tl) 
#no effect of TL
GULD_propmoving.exp.mod.tl <- lmer(propmoving.exp ~ TL + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.tl)
summary(GULD_propmoving.exp.mod.tl) 
#no effect of TL
GULD_dist.mod.tl <- lmer(dist ~ TL + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.tl)
summary(GULD_dist.mod.tl) 
#no effect of TL
GULD_timefrozen_tot.ln.mod.tl <- lmer(timefrozen_tot.ln ~ TL + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.tl)
summary(GULD_timefrozen_tot.ln.mod.tl) 
#no effect of TL
GULD_centretime.lnplus1.mod.tl <- lmer(centretime.lnplus1 ~ TL + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.tl)
summary(GULD_centretime.lnplus1.mod.tl) 
#no effect of TL


#ACT: Weight
summary(GULD_ACT.processed$Weight)
#Range from 10 - 70 g

GULD_avespeed_tot.mod.weight <- lmer(avespeed_tot ~ Weight + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.weight)
summary(GULD_avespeed_tot.mod.weight) 
#no effect of weight
GULD_aveacceler.mod.weight <- lmer(aveacceler ~ Weight + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.weight)
summary(GULD_aveacceler.mod.weight) 
#no effect of weight (near significant)
GULD_propmoving.exp.mod.weight <- lmer(propmoving.exp ~ Weight + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.weight)
summary(GULD_propmoving.exp.mod.weight) 
#no effect of weight (near significant)
GULD_dist.mod.weight <- lmer(dist ~ Weight + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.weight)
summary(GULD_dist.mod.weight) 
#no effect of weight
GULD_timefrozen_tot.ln.mod.weight <- lmer(timefrozen_tot.ln ~ Weight + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.weight)
summary(GULD_timefrozen_tot.ln.mod.weight) 
#no effect of weight
GULD_centretime.lnplus1.mod.weight <- lmer(centretime.lnplus1 ~ Weight + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.weight)
summary(GULD_centretime.lnplus1.mod.weight) 
#no effect of weight


#ACT: Infection Score 
#Score of 1-3: 
#1 = 0 - 10% of fins with ectoparasite/fungal infection
#2 = 10 - 50% of fins with ectoparasite/fungal infection
#3 = 50 - 100% of fins with ectoparasite/fungal infection
GULD_ACT.processed$InfectionScore <- as.factor(GULD_ACT.processed$InfectionScore)

GULD_avespeed_tot.mod.infectionscore <- lmer(avespeed_tot ~ InfectionScore + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.infectionscore)
summary(GULD_avespeed_tot.mod.infectionscore) 
#marginally negative effect of infectionscore
GULD_aveacceler.mod.infectionscore <- lmer(aveacceler ~ InfectionScore + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.infectionscore)
summary(GULD_aveacceler.mod.infectionscore) 
#negative effect of infectionscore
GULD_propmoving.exp.mod.infectionscore <- lmer(propmoving.exp ~ InfectionScore + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.infectionscore)
summary(GULD_propmoving.exp.mod.infectionscore) 
#negative effect of infectionscore
GULD_dist.mod.infectionscore <- lmer(dist ~ InfectionScore + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.infectionscore)
summary(GULD_dist.mod.infectionscore) 
#negative effect of infectionscore
GULD_timefrozen_tot.ln.mod.infectionscore <- lmer(timefrozen_tot.ln ~ InfectionScore + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.infectionscore)
summary(GULD_timefrozen_tot.ln.mod.infectionscore) 
#marginally negative effect of infectionscore
GULD_centretime.lnplus1.mod.infectionscore <- lmer(centretime.lnplus1 ~ InfectionScore + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.infectionscore)
summary(GULD_centretime.lnplus1.mod.infectionscore) 
#no effect of infectionscore

#ACT Summary: 
#    - Sex: Strong differences where males appear more active, only no effect for centre time
#    - TL/Weight: No signficant effect, some near signficant
#    - InfectionScore: Marginally significant effects of infection intensity on activity, where more infected individuals are less active, no effect for centre time 


#EXPL: Sex
GULD_EXPL.processed$Sex[GULD_EXPL.processed$Sex == "m(j)"] <- "m" #grouped as male v female, only two males were noted as juvenile

#Note: sample heavily biased towards males (consistent with other studies)
n_distinct(subset(GULD_EXPL.processed, Sex == 'f')$FishID) #n = 6
n_distinct(subset(GULD_EXPL.processed, Sex == 'm')$FishID) #n = 37

GULD_emergelat.bin.B.mod.sex <- glmer(emergelat.bin.B ~ Sex + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_emergelat.bin.B.mod.sex)
summary(GULD_emergelat.bin.B.mod.sex) 
#no effect of sex
GULD_endpointlat.bin.B.mod.sex <- glmer(endpointlat.bin.B ~ Sex + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.sex)
summary(GULD_endpointlat.bin.B.mod.sex) 
#no effect of trial day


#EXPL: Total Length (TL)
summary(GULD_EXPL.processed$TL)
#Range from 9.5 - 17.30 cm

GULD_emergelat.bin.B.mod.tl <- glmer(emergelat.bin.B ~ TL + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_emergelat.bin.B.mod.tl)
summary(GULD_emergelat.bin.B.mod.tl) 
#no effect of tl
GULD_endpointlat.bin.B.mod.tl <- glmer(endpointlat.bin.B ~ TL + (1|FishID), family = binomial, data=GULD_EXPL.processed)
#plot(GULD_endpointlat.bin.B.mod.tl)
#summary(GULD_endpointlat.bin.B.mod.tl) 
#model failure to converge


#EXPL: Weight
summary(GULD_EXPL.processed$Weight)
#Range from 9.5 - 17.30 cm

#adding weight data from ACT dataframe to this dataframe
GULD_EXPL.processed$mergeID <- paste(GULD_EXPL.processed$FishID, GULD_EXPL.processed$TrialDay, sep = '')
GULD_ACT.working <- GULD_ACT.processed
GULD_ACT.working$mergeID <- paste(GULD_ACT.working$FishID, GULD_ACT.working$TrialDay, sep = '')
GULD_ACT.working <- select(GULD_ACT.working, -c(PITID, FishID, TL, InfectionScore, Sex, Date, dist, timefrozen_tot.ln, propmoving.exp, centretime.lnplus1
                                                ,TrialType, TrialDay, ArenaID, TankID, avespeed_tot, avespeed_mob, aveacceler, propmoving))
GULD_EXPL.processed <- merge(GULD_EXPL.processed, GULD_ACT.working, by = 'mergeID', all.x = TRUE)
GULD_EXPL.processed <- select(GULD_EXPL.processed, -c(mergeID))
nrow(GULD_EXPL.processed) #113 trials

GULD_emergelat.bin.B.mod.tl <- glmer(emergelat.bin.B ~ TL + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_emergelat.bin.B.mod.tl)
summary(GULD_emergelat.bin.B.mod.tl) 


GULD_endpointlat.bin.B.mod.weight <- glmer(endpointlat.bin.B ~ Weight + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.weight)
summary(GULD_endpointlat.bin.B.mod.weight) 
#no effect of weight
GULD_endpointlat.bin.B.mod.weight <- glmer(endpointlat.bin.B ~ Weight + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.weight)
summary(GULD_endpointlat.bin.B.mod.weight) 
#no effect of weight

#EXPL: Infection Score 
#Score of 1-3: 
#1 = 0 - 10% of fins with ectoparasite/fungal infection
#2 = 10 - 50% of fins with ectoparasite/fungal infection
#3 = 50 - 100% of fins with ectoparasite/fungal infection
GULD_EXPL.processed$InfectionScore <- as.factor(GULD_EXPL.processed$InfectionScore)

GULD_endpointlat.bin.B.mod.infectionscore <- glmer(endpointlat.bin.B ~ InfectionScore + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.infectionscore)
summary(GULD_endpointlat.bin.B.mod.infectionscore) 
#no effect of infection score
GULD_endpointlat.bin.B.mod.infectionscore <- glmer(endpointlat.bin.B ~ InfectionScore + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.infectionscore)
summary(GULD_endpointlat.bin.B.mod.infectionscore) 
#no effect of infection score

#EXPL Summary: 
#    - Sex: no effect
#    - TL/Weight: no effect
#    - InfectionScore: no effect


write.csv(GULD_ACT.processed, '~/trophicpersonalities_GULD/5_Behaviour_CovariationAnalysis/GULD_ACT.processed2.csv')
write.csv(GULD_EXPL.processed, '~/trophicpersonalities_GULD/5_Behaviour_CovariationAnalysis/GULD_EXPL.processed2.csv')


### 5.3. Exploring relationship between behavioral variables (ACT) ----
GULD_ACT.processed2 <- read.csv('~/trophicpersonalities_GULD/5_Behaviour_CovariationAnalysis/GULD_ACT.processed2.csv')
GULD_EXPL.processed2 <- read.csv('~/trophicpersonalities_GULD/5_Behaviour_CovariationAnalysis/GULD_ACT.processed2.csv')


#Removing unneeded rows
GULD_ACT.processed2 <- select(GULD_ACT.processed, -c(X, TimeLoaded, TrialRound, avespeed_mob, propmoving, timefrozen_tot, timefrozen_ave, centretime, Notes, UniqueID))
cor(GULD_ACT.processed2[,12:17])
pairs(GULD_ACT.processed2[,12:17]) #appears to be strong correlations between all activity variables

#Running a PCA on all data
labels(GULD_ACT.processed2)
GULD_ACT.pca <- rda(GULD_ACT.processed2[,c(-1,-2,-3,-4,-5, -6, -7, -8, -9, -10, -11)], na = na.exclude, scale=TRUE)
summary(GULD_ACT.pca, display=NULL) 
screeplot(GULD_ACT.pca)
abline(a = 1, b = 0) #only PC1 has eigenvalue greater than 1

GULD_ACT.ord <- ordiplot(GULD_ACT.pca, type = "n")
data.envfit <- envfit(GULD_ACT.pca, GULD_ACT.processed2[,12:17])
plot(data.envfit, col="blue")
data.envfit
Sex <- model.matrix(~-1+Sex, GULD_ACT.processed)
data.envfit2 <- envfit(GULD_ACT.pca, env=Sex)
data.envfit2
plot(data.envfit2, col="green")
InfectionScore <- model.matrix(~-1+InfectionScore, GULD_ACT.processed)
data.envfit3 <- envfit(GULD_ACT.pca, env=InfectionScore)
data.envfit3
plot(data.envfit3, col="red")


#Taking PC1 and PC2 as composite variables
GULD_ACT.scores <- as.data.frame(scores(GULD_ACT.pca,display="sites"))
GULD_ACT.loadings <- scores(GULD_ACT.pca,display="species")

GULD_ACT.scores$mergeID <- (1:118)
GULD_ACT.processed$mergeID <- (1:118)
GULD_ACT.processed <- merge(GULD_ACT.processed, GULD_ACT.scores, by = 'mergeID')
GULD_ACT.processed <- rename(GULD_ACT.processed, ACT.PC1 = PC1)
GULD_ACT.processed <- rename(GULD_ACT.processed, ACT.PC2 = PC2)



