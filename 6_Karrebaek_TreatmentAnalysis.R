# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



Sys.setenv(LANG = "en")
library(dplyr)); library(ggplot2); library(lme4); library(lmerTest); 



#6. Analysis of Treatment Effects (KARR) ----

### 6.1. Behavioural Analysis, full interaction models  ----
### 6.2. Behavioural Analysis, post trial only ----
### 6.3. Behavioural Analysis, precentage change ----
### 6.4. Survival Analysis ----
### 6.5. Growth Analysis ----


#  Behavioral variables used in analysis:
#    avespeed_tot.sqrt
#    avespeed_mob
#    aveacceler.sqrt
#    propmoving
#    dist.sqrt
#    timefrozen_tot
#    centretime.lnplus1

#Interaction Models- post-treatment data
KARR_ACT2<- subset(KARR_ACT, TrialDay == 5)

boxplot(KARR_ACT2$avespeed_tot~KARR_ACT2$Treatment)
boxplot(KARR_ACT2$avespeed_mob~KARR_ACT2$Treatment)
boxplot(KARR_ACT2$aveacceler ~KARR_ACT2$Treatment)
boxplot(KARR_ACT2$propmoving~KARR_ACT2$Treatment)
boxplot(KARR_ACT2$dist~KARR_ACT2$Treatment)
boxplot(KARR_ACT2$timefrozen_tot~KARR_ACT2$Treatment)
boxplot(KARR_ACT2$timefrozen_ave~KARR_ACT2$Treatment)
boxplot(KARR_ACT2$centretime~KARR_ACT2$Treatment)

avespeed_tot.mod <- lmer(centretime.lnplus1 ~ Treatment + (1|ArenaID), data = KARR_ACT2)
anova(avespeed_tot.mod)

?anova

#Interaction Models- untransformed data
KARR_ACT$TrialDay <- as.factor(KARR_ACT$TrialDay)
KARR_avespeed_tot.mod <- lmer(avespeed_tot ~ ArenaID + TrialDay*Treatment + (1|FishID), data=KARR_ACT)
summary(KARR_avespeed_tot.mod)
plot(KARR_avespeed_tot.mod)

KARR_avespeed_mob.mod <- lmer(avespeed_mob ~ TrialDay*Treatment + (1|FishID), data=KARR_ACT)
summary(KARR_avespeed_mob.mod)
plot(KARR_avespeed_mob.mod)

KARR_aveacceler.mod <- lmer(aveacceler ~ TrialDay*Treatment + (1|FishID), data=KARR_ACT)
summary(KARR_aveacceler.mod)
plot(KARR_aveacceler.mod)

KARR_propmoving.mod <- lmer(propmoving ~ TrialDay*Treatment + (1|FishID), data=KARR_ACT)
summary(KARR_propmoving.mod)
plot(KARR_propmoving.mod)

KARR_dist.mod <- lmer(dist ~ TrialDay*Treatment + (1|FishID), data=KARR_ACT)
summary(KARR_dist.mod)
plot(KARR_dist.mod)

KARR_timefrozen_tot.mod <- lmer(timefrozen_tot ~ TrialDay*Treatment + (1|FishID), data=KARR_ACT)
summary(KARR_timefrozen_tot.mod)
plot(KARR_timefrozen_tot.mod)

KARR_timefrozen_ave.mod <- lmer(timefrozen_ave ~ TrialDay*Treatment + (1|FishID), data=KARR_ACT)
summary(KARR_timefrozen_ave.mod)
plot(KARR_timefrozen_ave.mod)

KARR_centretime.mod <- lmer(centretime ~ TrialDay*Treatment + (1|FishID), data=KARR_ACT)
summary(KARR_centretime.mod)
plot(KARR_centretime.mod)



