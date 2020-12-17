# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



Sys.setenv(LANG = "en")
library(dplyr); library(ggpubr); library(ggplot2); library(lme4); library(rptR); library(lmerTest); library(rptR)




KARR_ACT <- read.csv('~/trophicpersonalities_A/5_Karrebaek_PreliminaryAnalysis/KARR_ACT112020.csv')
labels(KARR_ACT)

nrow()
#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"),
                       axis.text.x = element_text(size = 10, colour = "black"), 
                       panel.background = element_rect(fill = "white"),
                       axis.title.y  = element_text(size=12, vjust = 2),
                       axis.title.x  = element_text(size=12, vjust = 0.1),
                       panel.border = element_rect(colour = "black", fill=NA, size = 1))


#Initial exploration of variable and assessing distributions-
#avespeed_tot: (mm/s) the average speed of the individual accross the full trial period
ggplot(KARR_ACT) + aes(x = avespeed_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 4.5) + simpletheme 
ggqqplot(KARR_ACT$avespeed_tot)

#Variance analysis-
KARR_avespeed_tot.mod <- lmer(avespeed_tot ~ (1|FishID), data=KARR_ACT)
summary(KARR_avespeed_tot.mod)
plot(KARR_avespeed_tot.mod)
KARR_avespeed_tot.rpt <- rpt(avespeed_tot ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
KARR_avespeed_tot.rpt




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


#Interaction Models- untransformed data
KARR_ACT$TrialDay <- as.factor(KARR_ACT$TrialDay)
KARR_avespeed_tot.mod <- lmer(avespeed_tot ~ TrialDay*Treatment + (1|FishID), data=KARR_ACT)
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
