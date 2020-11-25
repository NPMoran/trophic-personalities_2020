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
#    avespeed_mob
#    aveacceler
#    propmoving.exp
#    dist
#    timefrozen_tot.ln
#    centretime.lnplus1
#    emergelat.bin.B
#    endpointlat.bin.B

GULD_ACT.processed <- read.csv('~/trophicpersonalities_GULD/4_Behaviour_VarianceAnalysis/GULD_ACT.processed.csv')
GULD_EXPL.processed <- read.csv('~/trophicpersonalities_GULD/4_Behaviour_VarianceAnalysis/GULD_EXPL.processed.csv')

### 5.1. Systematic factors influencing behavioural variation ----
#Experimental control predictors 
#    TrialDay
#    TrialRound
#    ArenaID
#    TankID


#ACT: TrialDay
GULD_ACT.processed$TrialDay <- as.factor(GULD_ACT.processed$TrialDay)

GULD_avespeed_tot.mod.trialday <- lmer(avespeed_tot ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.trialday)
summary(GULD_avespeed_tot.mod.trialday) 
#avespeed_tot reduction over the three trial days
GULD_avespeed_mob.mod.trialday <- lmer(avespeed_mob ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_mob.mod.trialday)
summary(GULD_avespeed_mob.mod.trialday) 
#avespeed_mob reduction over the three trial days
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
GULD_ACT.processed$TrialRound <- as.factor(GULD_ACT.processed$TrialRound)

GULD_avespeed_tot.mod.trialround <- lmer(avespeed_tot ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.trialround)
summary(GULD_avespeed_tot.mod.trialround) 
#marginal positive effect for round 6
GULD_avespeed_mob.mod.trialround <- lmer(avespeed_mob ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_mob.mod.trialround)
summary(GULD_avespeed_mob.mod.trialround) 
#no systematic differences due to the order of trials
GULD_aveacceler.mod.trialround <- lmer(aveacceler ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.trialround)
summary(GULD_aveacceler.mod.trialround) 
#positive effect for round 6
GULD_propmoving.exp.mod.trialround <- lmer(propmoving.exp ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.trialround)
summary(GULD_propmoving.exp.mod.trialround) 
#no systematic differences due to the order of trials
GULD_dist.mod.trialround <- lmer(dist ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.trialround)
summary(GULD_dist.mod.trialround) 
#marginal positive effect for round 6
GULD_timefrozen_tot.ln.mod.trialround <- lmer(timefrozen_tot.ln ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.trialround)
summary(GULD_timefrozen_tot.ln.mod.trialround) 
#positive effect for round 6
GULD_centretime.lnplus1.mod.trialround <- lmer(centretime.lnplus1 ~ TrialRound + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.trialround)
summary(GULD_centretime.lnplus1.mod.trialround) 
#no systematic differences due to the order of trials

#ACT: ArenaID
GULD_avespeed_tot.mod.arenaid <- lmer(avespeed_tot ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
summary(GULD_avespeed_tot.mod.arenaid)
plot(GULD_avespeed_tot.mod.arenaid)
#no systematic differences among arenas
GULD_avespeed_mob.mod.arenaid <- lmer(avespeed_mob ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
summary(GULD_avespeed_mob.mod.arenaid)
plot(GULD_avespeed_mob.mod.arenaid)
#no systematic differences among arenas
GULD_aveacceler.mod.arenaid <- lmer(aveacceler ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_aveacceler.mod.arenaid)
summary(GULD_aveacceler.mod.arenaid)
#marginally negative effect for arena C, marginally positive effect for arena D
GULD_propmoving.exp.mod.arenaid <- lmer(propmoving.exp ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_propmoving.exp.mod.arenaid)
summary(GULD_propmoving.exp.mod.arenaid)
#marginally negative effect for arenas C and F
GULD_dist.mod.arenaid <- lmer(dist ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_dist.mod.arenaid)
summary(GULD_dist.mod.arenaid)
#no systematic differences among arenas
GULD_timefrozen_tot.ln.mod.arenaid <- lmer(timefrozen_tot.ln ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_timefrozen_tot.ln.mod.arenaid)
summary(GULD_timefrozen_tot.ln.mod.arenaid)
#marginally positive effect for arenas G
GULD_centretime.lnplus1.mod.arenaid <- lmer(centretime.lnplus1 ~ ArenaID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.arenaid)
summary(GULD_centretime.lnplus1.mod.arenaid)
#appears the a negative effect on Arena C, and marginally significant negative effect on G

#ACT: TankID
GULD_avespeed_tot.mod.tankid <- lmer(avespeed_tot ~ TankID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.tankid)
summary(GULD_avespeed_tot.mod.tankid)
#no systematic differences among holding tanks
GULD_avespeed_mob.mod.tankid <- lmer(avespeed_mob ~ TankID + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_mob.mod.tankid)
summary(GULD_avespeed_mob.mod.tankid)
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
#    - No systematic effect of trial order (TrialRound), Round 6 appeared higher in some activity categories, but this is assumed to be becuase there were no round six trials on TrialDay = 3, when activity was much lower
#    - No effect of holding tank (TankID)
#    - Arena ACT_C showed a signficantly lower centretime.lnplus1, although the position of arenas did not show evidence of any systematic pattern between arenas


#EXPL: TrialDay
GULD_EXPL.processed$TrialDay <- as.factor(GULD_EXPL.processed$TrialDay)

GULD_emergelat.bin.B.mod.trialday <- glmer(emergelat.bin.B ~ TrialDay + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_emergelat.bin.B.mod.trialday)
summary(GULD_emergelat.bin.B.mod.trialday) 
#no effect of trial day
GULD_endpointlat.bin.B.mod.trialday <- glmer(endpointlat.bin.B ~ TrialDay + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.trialday)
summary(GULD_endpointlat.bin.B.mod.trialday) 
#no effect of trial day

#EXPL: TrialRound
GULD_EXPL.processed$TrialRound <- as.factor(GULD_EXPL.processed$TrialRound)

GULD_emergelat.bin.B.mod.trialround <- glmer(emergelat.bin.B ~ TrialRound + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_emergelat.bin.B.mod.trialround)
summary(GULD_emergelat.bin.B.mod.trialround) 
#no systematic differences due to the order of trials
GULD_endpointlat.bin.B.mod.trialround <- glmer(endpointlat.bin.B ~ TrialRound + (1|FishID), family = binomial, data=GULD_EXPL.processed)
plot(GULD_endpointlat.bin.B.mod.trialround)
summary(GULD_endpointlat.bin.B.mod.trialround) 
#marginally negative effect for round 5

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


### 5.2. Fish state factors influencing behavioural variation ----
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

GULD_avespeed_tot.mod.sex <- lmer(avespeed_tot ~  Sex + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.sex)
summary(GULD_avespeed_tot.mod.sex) 
#positive effect for males
GULD_avespeed_mob.mod.sex <- lmer(avespeed_mob ~  Sex + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_mob.mod.sex)
summary(GULD_avespeed_mob.mod.sex) 
#no effect
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
GULD_avespeed_mob.mod.tl <- lmer(avespeed_mob ~ TL + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_mob.mod.tl)
summary(GULD_avespeed_mob.mod.tl) 
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
GULD_avespeed_mob.mod.weight <- lmer(avespeed_mob ~ Weight + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_mob.mod.weight)
summary(GULD_avespeed_mob.mod.weight) 
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

#running as numeric, but similar results with categorical
#GULD_ACT.processed$InfectionScore <- as.factor(GULD_ACT.processed$InfectionScore)
GULD_ACT.processed$InfectionScore <- as.numeric(GULD_ACT.processed$InfectionScore)

GULD_avespeed_tot.mod.infectionscore <- lmer(avespeed_tot ~ InfectionScore + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_tot.mod.infectionscore)
summary(GULD_avespeed_tot.mod.infectionscore) 
#negative effect of infectionscore
GULD_avespeed_mob.mod.infectionscore <- lmer(avespeed_mob ~ InfectionScore + (1|FishID), data=GULD_ACT.processed)
plot(GULD_avespeed_mob.mod.infectionscore)
summary(GULD_avespeed_mob.mod.infectionscore) 
#no effect of infectionscore
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
#negative effect of infectionscore
GULD_centretime.lnplus1.mod.infectionscore <- lmer(centretime.lnplus1 ~ InfectionScore + (1|FishID), data=GULD_ACT.processed)
plot(GULD_centretime.lnplus1.mod.infectionscore)
summary(GULD_centretime.lnplus1.mod.infectionscore) 
#no effect of infectionscore

#ACT Summary: 
#    - Sex: Strong differences where males appear more active, only no effect for centre time
#    - TL/Weight: No signficant effect, some near signficant
#    - InfectionScore: Marginally significant effects of infection intensity on activity, where more infected individuals are less active, no effect for avespeed_mob or centre time 

#Note: Infection score was generally higher for females, so may lower activity in females may be a product of infection score or visa versa
summary(subset(GULD_ACT.processed, Sex == 'f')$InfectionScore)
summary(subset(GULD_ACT.processed, Sex == 'm')$InfectionScore)


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
GULD_ACT.working <- select(GULD_ACT.working, -c(X, TimeLoaded, TrialRound, timefrozen_tot, timefrozen_ave, centretime, PITID, FishID, TL, InfectionScore, Sex, Date, dist, timefrozen_tot.ln, propmoving.exp, centretime.lnplus1
                                                ,TrialType, TrialDay, ArenaID, TankID, avespeed_tot, avespeed_mob, aveacceler, propmoving, Notes, UniqueID))
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
#GULD_EXPL.processed$InfectionScore <- as.factor(GULD_EXPL.processed$InfectionScore)
GULD_EXPL.processed$InfectionScore <- as.numeric(GULD_EXPL.processed$InfectionScore)

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



### 5.3. Calculating adjusted repeatabilities for some variables ----
#  Factors accounted for each behavioural variable:
#    avespeed_tot       - TrialDay, Sex, InfectionScore
#    avespeed_mob       - TrialDay
#    aveacceler         - TrialDay, Sex, InfectionScore
#    propmoving.exp     - TrialDay, Sex, InfectionScore
#    dist               - TrialDay, Sex, InfectionScore
#    timefrozen_tot.ln  - TrialDay, Sex, InfectionScore
#    centretime.lnplus1 - nil
#    emergelat.bin.B    - nil
#    endpointlat.bin.B  - nil


GULD_avespeed_tot.mod.adj <- lmer(avespeed_tot ~ TrialDay + Sex + InfectionScore + (1|FishID), data=GULD_ACT.processed)
summary(GULD_avespeed_tot.mod.adj)
plot(GULD_avespeed_tot.mod.adj)
GULD_avespeed_tot.rpt.adj <- rpt(avespeed_tot ~ TrialDay + Sex + InfectionScore + (1 | FishID), grname = "FishID", data = GULD_ACT.processed, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
GULD_avespeed_tot.rpt.adj


GULD_avespeed_mob.mod.adj <- lmer(avespeed_mob ~ TrialDay + (1|FishID), data=GULD_ACT.processed)
summary(GULD_avespeed_mob.mod.adj)
plot(GULD_avespeed_mob.mod.adj)
GULD_avespeed_mob.rpt.adj <- rpt(avespeed_mob ~ TrialDay + (1 | FishID), grname = "FishID", data = GULD_ACT.processed, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
GULD_avespeed_mob.rpt.adj


GULD_aveacceler.mod.adj <- lmer(aveacceler ~  TrialDay + Sex + InfectionScore + (1|FishID), data=GULD_ACT.processed)
summary(GULD_aveacceler.mod.adj)
plot(GULD_aveacceler.mod.adj)
GULD_aveacceler.rpt.adj <- rpt(aveacceler ~  TrialDay + Sex + InfectionScore + (1 | FishID), grname = "FishID", data = GULD_ACT.processed, datatype = "Gaussian", 
                           nboot = 100, npermut = 0)
GULD_aveacceler.rpt.adj


GULD_propmoving.exp.mod.adj <- lmer(propmoving.exp ~ TrialDay + Sex + InfectionScore + (1|FishID), data=GULD_ACT.processed)
summary(GULD_propmoving.exp.mod.adj)
plot(GULD_propmoving.exp.mod.adj)
GULD_propmoving.exp.rpt.adj <- rpt(propmoving.exp ~ TrialDay + Sex + InfectionScore + (1 | FishID), grname = "FishID", data = GULD_ACT.processed, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
GULD_propmoving.exp.rpt.adj


GULD_dist.mod.adj <- lmer(dist ~  TrialDay + Sex + InfectionScore + (1|FishID), data=GULD_ACT.processed)
summary(GULD_dist.mod.adj)
plot(GULD_dist.mod.adj)
GULD_dist.rpt.adj <- rpt(dist ~  TrialDay + Sex + InfectionScore + (1 | FishID), grname = "FishID", data = GULD_ACT.processed, datatype = "Gaussian", 
                     nboot = 100, npermut = 0)
GULD_dist.rpt.adj


GULD_timefrozen_tot.ln.mod <- lmer(timefrozen_tot.ln ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_timefrozen_tot.ln.mod)
plot(GULD_timefrozen_tot.ln.mod)
GULD_timefrozen_tot.ln.rpt <- rpt(timefrozen_tot.ln ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_timefrozen_tot.ln.rpt


GULD_centretime.lnplus1.mod <- lmer(centretime.lnplus1 ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_centretime.lnplus1.mod)
plot(GULD_centretime.lnplus1.mod)
GULD_centretime.lnplus1.rpt <- rpt(centretime.lnplus1 ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                                   nboot = 100, npermut = 0)
GULD_centretime.lnplus1.rpt


#Summary adjusted repeatabilities-
#avespeed_tot:      _ 0.610 [0.447, 0.764] ***
#avespeed_mob       _ 0.377 [0.172, 0.601] *** 
#aveacceler         _ 0.517 [0.368, 0.650] ***
#propmoving.exp     _ 0.565 [0.380, 0.722] ***
#dist               _ 0.596 [0.446, 0.732] ***
#timefrozen_tot.ln  _ 0.374 [0.153, 0.574] ***
#centretime.lnplus1 _ NA
#emergelat.bin.B    - NA
#endpointlat.bin.B  - NA



### 5.4. Exploring relationship between behavioral variables ----

#Variables:
#avespeed_tot       avespeed_mob       aveacceler         propmoving.exp     
#dist               timefrozen_tot.ln  centretime.lnplus1 emergelat.bin.B    
#endpointlat.bin.B  

GULD_ACT.processed2 <- read.csv('~/trophicpersonalities_GULD/5_Behaviour_CovariationAnalysis/GULD_ACT.processed2.csv')
GULD_EXPL.processed2 <- read.csv('~/trophicpersonalities_GULD/5_Behaviour_CovariationAnalysis/GULD_EXPL.processed2.csv')
GULD_ACT.processed2$TrialDay <- as.factor(GULD_ACT.processed2$TrialDay)
GULD_ACT.processed2$TrialRound <- as.factor(GULD_ACT.processed2$TrialRound)
GULD_EXPL.processed2$TrialDay <- as.factor(GULD_EXPL.processed2$TrialDay)
GULD_EXPL.processed2$TrialRound <- as.factor(GULD_EXPL.processed2$TrialRound)

#Creating dataframe with all beahvioural variables
GULD_ACT.processed2$mergeID <- paste(GULD_ACT.processed2$FishID, GULD_ACT.processed2$TrialDay, sep ='_')
GULD_EXPL.processed2$mergeID <- paste(GULD_EXPL.processed2$FishID, GULD_EXPL.processed2$TrialDay, sep ='_')
GULD_behav_merged <- merge(GULD_ACT.processed2, GULD_EXPL.processed2, by = 'mergeID', all.x = TRUE)
GULD_behav_merged <- select(GULD_behav_merged, -c(Weight.y, endpointlat.bin.A, emergelat.bin.A, endpointspeed.ln, refugereturnlat.ln,
                                                  Notes.y, UniqueID.y, emergelat, endpointtime, endpointlat, endpointspeed, refugereturntime, 
                                                  refugereturnlat, Date.y, TimeLoaded.y, TimeInitiated, TrialType.y, TrialDay.y, 
                                                  TrialRound.y, ArenaID.y, TankID.y,emergetime, X.1.y, X.y, PITID.y, FishID.y,
                                                  TL.y, InfectionScore.y, Sex.y, timefrozen_tot, timefrozen_ave, centretime, Notes.x,
                                                  UniqueID.x, propmoving, TrialType.x, Date.x, mergeID, X.1.x, X.x, TimeLoaded.x, ArenaID.x))
labels(GULD_behav_merged)

GULD_behav_merged <- rename(GULD_behav_merged, PITID = PITID.x)
GULD_behav_merged <- rename(GULD_behav_merged, FishID = FishID.x)
GULD_behav_merged <- rename(GULD_behav_merged, TL = TL.x)
GULD_behav_merged <- rename(GULD_behav_merged, InfectionScore = InfectionScore.x)
GULD_behav_merged <- rename(GULD_behav_merged, Sex = Sex.x)
GULD_behav_merged <- rename(GULD_behav_merged, TrialDay = TrialDay.x)
GULD_behav_merged <- rename(GULD_behav_merged, TrialRound = TrialRound.x)
GULD_behav_merged <- rename(GULD_behav_merged, TankID = TankID.x)
GULD_behav_merged <- rename(GULD_behav_merged, Weight = Weight.x)

labels(GULD_behav_merged)


#As the PCA cannot handle NA, this exclude rows with NAs for
#i.e. fish who completed the activity trial, but not the exploratory trial in a week are excluded
#5 rows excluded
GULD_behav_merged.pca <- subset(GULD_behav_merged, emergelat.bin.B != 'NA')
cor(GULD_behav_merged.pca[,10:18])
pairs(GULD_behav_merged.pca[,10:18])
#appears to be strong correlations between all activity variables, more active individual appear to be more likely to emerge and reach endpoint

#Running a PCA on all data
GULD_behav_merged.pca.rda <- rda(GULD_behav_merged.pca[,c(-1,-2,-3,-4,-5, -6, -7, -8, -9)], na = na.exclude, scale=TRUE)
summary(GULD_behav_merged.pca.rda, display=NULL) 
screeplot(GULD_behav_merged.pca.rda)
abline(a = 1, b = 0) #PC1 and PC2 have eigenvalue greater than 1

GULD_behav_merged.pca.ord <- ordiplot(GULD_behav_merged.pca.rda, type = "n")
data.envfit <- envfit(GULD_behav_merged.pca.rda, GULD_behav_merged.pca[,10:18])
plot(data.envfit, col="blue")
data.envfit
Sex <- model.matrix(~-1+Sex, GULD_behav_merged.pca)
data.envfit2 <- envfit(GULD_behav_merged.pca.rda, env=Sex)
data.envfit2
plot(data.envfit2, col="green")
InfectionScore <- model.matrix(~-1+InfectionScore, GULD_behav_merged.pca)
data.envfit3 <- envfit(GULD_behav_merged.pca.rda, env=InfectionScore)
data.envfit3
plot(data.envfit3, col="red")
TrialDay <- model.matrix(~-1+TrialDay, GULD_behav_merged.pca)
data.envfit4 <- envfit(GULD_behav_merged.pca.rda, env=TrialDay)
data.envfit4
plot(data.envfit4, col="pink")

#Taking PC1 and PC2 as composite variables
GULD_behav_merged.pca.scores <- as.data.frame(scores(GULD_behav_merged.pca.rda, display="sites"))
GULD_behav_merged.pca.loadings <- scores(GULD_behav_merged.pca.rda, display="species")

#Adding those back into the main dataframe
GULD_behav_merged.pca.scores$mergeID <- (1:113)
GULD_behav_merged.pca$mergeID <- (1:113)
GULD_behav_merged.pca <- merge(GULD_behav_merged.pca, GULD_behav_merged.pca.scores, by = 'mergeID')
nrow(GULD_behav_merged.pca)
GULD_behav_merged.pca$mergeID <- paste(GULD_behav_merged.pca$FishID, GULD_behav_merged.pca$TrialDay, sep = '_')
GULD_behav_merged$mergeID <-  paste(GULD_behav_merged$FishID, GULD_behav_merged$TrialDay, sep = '_')
GULD_behav_merged.pca <- select(GULD_behav_merged.pca, c(mergeID, PC1, PC2))
GULD_behav_merged <- merge(GULD_behav_merged, GULD_behav_merged.pca, by = 'mergeID', all.x = TRUE)


#Analysing effects on composite variables
GULD_PC1.mod <- lmer(PC1 ~  TrialDay + Sex + InfectionScore + (1|FishID), data=GULD_behav_merged)
summary(GULD_PC1.mod)
plot(GULD_PC1.mod)
GULD_PC1.rpt.adj <- rpt(PC1 ~  TrialDay + Sex + InfectionScore + (1 | FishID), grname = "FishID", data = GULD_behav_merged, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
GULD_PC1.rpt.adj
#PC1:
#    -ve male Sex effect 
#    positive TrialDay effect
#    no InfectionScore effect
#    highly repeatable (0.59, [0.473, 0.758])

GULD_PC2.mod <- lmer(PC2 ~  TrialDay + Sex + InfectionScore + (1|FishID), data=GULD_behav_merged)
summary(GULD_PC2.mod)
plot(GULD_PC2.mod)
GULD_PC2.rpt.adj <- rpt(PC2 ~  TrialDay + Sex + InfectionScore + (1 | FishID), grname = "FishID", data = GULD_behav_merged, datatype = "Gaussian", 
                            nboot = 100, npermut = 0)
GULD_PC2.rpt.adj
#PC1:
#    no Sex effect
#    positive TrialDay effect
#    no InfectionScore effect
#    highly repeatable (0.536, [0.352, 0.704])

write.csv(GULD_behav_merged, '~/trophicpersonalities_GULD/5_Behaviour_CovariationAnalysis/GULD_behav_merged.csv')

