# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



#Karrabaek 2. Treatment Analysis ----


Sys.setenv(LANG = "en")


#Loading required packages- 
library(dplyr); library(ggplot2); library(lme4); library(lmerTest); library(car)
library(survival); library(survminer); library(rptR)

#Loading required datasets-
KARRact.processed <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARR_ACTdat_processed.csv")
labels(KARRact.processed)


#Creating a general theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"),
                       axis.text.x = element_text(size = 10, colour = "black"), 
                       panel.background = element_rect(fill = "white"),
                       axis.title.y  = element_text(size=12, vjust = 2),
                       axis.title.x  = element_text(size=12, vjust = 0.1),
                       panel.border = element_rect(colour = "black", fill=NA, size = 1))



### K.2.1 Testing for treatment effects on behavioural variables (full models) ----

#  Variable             Random effects                    Fixed effects

#  avespeed_tot.sqrt:   TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  avespeed_mob:        TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  aveacceler:          TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  propmoving:          TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  dist.sqrt:           TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  frozenevents:        TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  timefrozen_tot:      TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  timefrozen_ave.ln:   TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  centretime50.sqrt:   TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  centretime75.sqrt:   TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay
#  centretime100.sqrt:  TankID.combo, ArenaID, FishID     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay


KARR_avespeed_tot.sqrt.mod.treat2 <- lmer(avespeed_tot.sqrt ~ 
                                            Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_avespeed_tot.sqrt.mod.treat2) #effect of TrialDay, Treatment*TrialDay interaction non-significant effect (marginal)
summary(KARR_avespeed_tot.sqrt.mod.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_avespeed_tot.sqrt.mod.treat2) #no clustering issues

KARR_avespeed_mob.mod.treat2 <- lmer(avespeed_mob ~ 
                                       Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_avespeed_mob.mod.treat2) #effect of TrialDay, no Treatment*TrialDay interaction effect
summary(KARR_avespeed_tot.sqrt.mod.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_avespeed_mob.mod.treat2) #no clustering issues

KARR_aveacceler.mod.treat2 <- lmer(aveacceler ~ 
                                     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_aveacceler.mod.treat2) #effect of TrialDay, Treatment*TrailDay interaction non-significant effect (marginal)
summary(KARR_aveacceler.mod.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_aveacceler.mod.treat2) #no clustering issues

KARR_propmoving.treat2 <- lmer(propmoving ~ 
                                 Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_propmoving.treat2) #effect of TrialDay + Treatment*TrailDay interaction
summary(KARR_propmoving.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_propmoving.treat2) #no clustering issues

KARR_dist.sqrt.treat2 <- lmer(dist.sqrt ~ 
                                Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_dist.sqrt.treat2) #effect of TrialDay, , Treatment*TrailDay interaction non-significant effect (marginal)
summary(KARR_dist.sqrt.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_dist.sqrt.treat2) #no clustering issues

KARR_frozenevents.treat2 <- lmer(frozenevents ~ 
                                   Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_frozenevents.treat2) #no effects
summary(KARR_frozenevents.treat2) #no effects
plot(KARR_frozenevents.treat2) #no clustering issues

KARR_timefrozen_tot.treat2 <- lmer(timefrozen_tot ~ 
                                     Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_timefrozen_tot.treat2) #effect of TrialDay, Treatment*TrailDay interaction non-significant effect (marginal)
summary(KARR_timefrozen_tot.treat2) #Treatmentpit:TrialDay pit group sig higher than cont
plot(KARR_timefrozen_tot.treat2) #no clustering issues

KARR_timefrozen_ave.ln.treat2 <- lmer(timefrozen_ave.ln ~ 
                                        Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_timefrozen_ave.ln.treat2)  #effect of TrialDay, Treatment*TrailDay interaction no significant effect, ConditionFactor non-signficant effect (marginal),
summary(KARR_timefrozen_ave.ln.treat2) #no Treatment*TrialDay effects
plot(KARR_timefrozen_ave.ln.treat2) #some asymmetry

KARR_centretime50.sqrt.treat2 <- lmer(centretime50.sqrt ~ 
                                        Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_centretime50.sqrt.treat2) #ConditionFactor effect
summary(KARR_centretime50.sqrt.treat2) #no Treatment*TrialDay effects
plot(KARR_centretime50.sqrt.treat2) #no clustering issues

KARR_centretime75.sqrt.treat2 <- lmer(centretime75.sqrt ~ 
                                        Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_centretime75.sqrt.treat2)  #ConditionFactor effect
summary(KARR_centretime75.sqrt.treat2) #positive effect of condition factor
plot(KARR_centretime75.sqrt.treat2) #no clustering issues

KARR_centretime100.sqrt.treat2 <- lmer(centretime100.sqrt ~ 
                                         Sex + TL + ConditionFactor + InfectionScore + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_centretime100.sqrt.treat2) #ConditionFactor effect
summary(KARR_centretime100.sqrt.treat2) #no Treatment*TrialDay effects
plot(KARR_centretime100.sqrt.treat2) #no clustering issues



### K.2.2 Testing for treatment effects on behavioural variables (reduced models) ----

#  Variable             Random effects                    Fixed effects

#  avespeed_tot.sqrt:   TankID.combo, ArenaID, FishID     Treatment*TrialDay
#  avespeed_mob:        TankID.combo, ArenaID, FishID     Treatment*TrialDay
#  aveacceler:          TankID.combo, ArenaID, FishID     Treatment*TrialDay
#  propmoving:          TankID.combo, ArenaID, FishID     Treatment*TrialDay
#  dist.sqrt:           TankID.combo, ArenaID, FishID     Treatment*TrialDay
#  frozenevents:        TankID.combo, ArenaID, FishID     Treatment*TrialDay
#  timefrozen_tot:      TankID.combo, ArenaID, FishID     Treatment*TrialDay
#  timefrozen_ave.ln:   TankID.combo, FishID              Treatment*TrialDay
#  centretime50.sqrt:   TankID.combo, FishID              ConditionFactor + Treatment*TrialDay
#  centretime75.sqrt:   TankID.combo, FishID              ConditionFactor + Treatment*TrialDay
#  centretime100.sqrt:  TankID.combo, FishID              ConditionFactor + Treatment*TrialDay



KARR_avespeed_tot.sqrt.mod.treat2 <- lmer(avespeed_tot.sqrt ~ 
                                            Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_avespeed_tot.sqrt.mod.treat2) #effect of TrialDay, Treatment*TrailDay interaction non-significant effect (marginal)
summary(KARR_avespeed_tot.sqrt.mod.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_avespeed_tot.sqrt.mod.treat2) #no clustering issues

KARR_avespeed_mob.mod.treat2 <- lmer(avespeed_mob ~ 
                                       Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_avespeed_mob.mod.treat2) #effect of TrialDay, no Treatment*TrailDay interaction effect
summary(KARR_avespeed_tot.sqrt.mod.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_avespeed_mob.mod.treat2) #no clustering issues

KARR_aveacceler.mod.treat2 <- lmer(aveacceler ~ 
                                     Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_aveacceler.mod.treat2) #effect of TrialDay, Treatment*TrailDay interaction non-significant effect (marginal)
summary(KARR_aveacceler.mod.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_aveacceler.mod.treat2) #no clustering issues

KARR_propmoving.treat2 <- lmer(propmoving  ~ 
                                 Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_propmoving.treat2) #effect of TrialDay + Treatment*TrailDay interaction
summary(KARR_propmoving.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_propmoving.treat2) #no clustering issues
confint(KARR_propmoving.treat2)

KARR_dist.sqrt.treat2 <- lmer(dist.sqrt ~ 
                                Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_dist.sqrt.treat2) #effect of TrialDay, , Treatment*TrailDay interaction non-significant effect (marginal)
summary(KARR_dist.sqrt.treat2) #Treatmentpit:TrialDay sig lower than controls
plot(KARR_dist.sqrt.treat2) #no clustering issues

KARR_frozenevents.treat2 <- lmer(frozenevents ~ 
                                   Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_frozenevents.treat2) #no effects
summary(KARR_frozenevents.treat2) #no effects
plot(KARR_frozenevents.treat2) #no clustering issues

KARR_timefrozen_tot.treat2 <- lmer(timefrozen_tot ~ 
                                     Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
Anova(KARR_timefrozen_tot.treat2) #effect of TrialDay, Treatment*TrailDay interaction non-significant effect (marginal)
summary(KARR_timefrozen_tot.treat2) #Treatmentpit:TrialDay pit group sig higher than cont
plot(KARR_timefrozen_tot.treat2) #no clustering issues

KARR_timefrozen_ave.ln.treat2 <- lmer(timefrozen_ave.ln ~ 
                                        Treatment*TrialDay + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
Anova(KARR_timefrozen_ave.ln.treat2)  #effect of TrialDay, Treatment*TrailDay interaction no significant effect, ConditionFactor non-signficant effect (marginal),
summary(KARR_timefrozen_ave.ln.treat2) #no Treatment*TrialDay effects
plot(KARR_timefrozen_ave.ln.treat2) #some asymmetry

KARR_centretime50.sqrt.treat2 <- lmer(centretime50.sqrt ~ 
                                        ConditionFactor + Treatment*TrialDay + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
Anova(KARR_centretime50.sqrt.treat2) #ConditionFactor effect
summary(KARR_centretime50.sqrt.treat2) #no Treatment*TrialDay effects
plot(KARR_centretime50.sqrt.treat2) #no clustering issues

KARR_centretime75.sqrt.treat2 <- lmer(centretime75.sqrt ~ 
                                       ConditionFactor + Treatment*TrialDay + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
Anova(KARR_centretime75.sqrt.treat2)  #ConditionFactor effect
summary(KARR_centretime75.sqrt.treat2) #positive effect of condition factor
plot(KARR_centretime75.sqrt.treat2) #no clustering issues

KARR_centretime100.sqrt.treat2 <- lmer(centretime100.sqrt ~ 
                                         ConditionFactor + Treatment*TrialDay + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
Anova(KARR_centretime100.sqrt.treat2) #ConditionFactor effect
summary(KARR_centretime100.sqrt.treat2) #no Treatment*TrialDay effects
plot(KARR_centretime100.sqrt.treat2) #no clustering issues



### K.2.3 Testing for consistency of behaviour under treatments ----

#  Variable             Raw Repeatability       Adj Repeatability
#  avespeed_tot.sqrt:   0.645 [0.483, 0.768]    0.718 [0.594, 0.842]
#  avespeed_mob:        0.346 [0.105, 0.593]    0.373 [0.144, 0.639]
#  aveacceler:          0.646 [0.471, 0.792]    0.711 [0.578, 0.836]
#  propmoving:          0.691 [0.501, 0.801]    0.752 [0.631, 0.849]
#  dist.sqrt:           0.650 [0.450, 0.768]    0.721 [0.616, 0.846]
#  frozenevents:        0.586 [0.388, 0.735]    0.593 [0.417, 0.779]
#  timefrozen_tot:      0.719 [0.449, 0.819]    0.763 [0.633, 0.888]
#  timefrozen_ave.ln:   0.706 [0.495, 0.823]    0.722 [0.559, 0.866]
#  centretime50.sqrt:   0.573 [0.356, 0.747]    0.577 [0.390, 0.763]
#  centretime75.sqrt:   0.535 [0.296, 0.715]    0.537 [0.353, 0.708]
#  centretime100.sqrt:  0.605 [0.382, 0.712]    0.596 [0.407, 0.755]


#Using simplified approach becuase it had convergence issuse with the full random effects structure
KARR_avespeed_tot.sqrt.mod.rpt1 <- rpt(avespeed_tot.sqrt ~ (1 | FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
KARR_avespeed_tot.sqrt.mod.rpt1
KARR_avespeed_tot.sqrt.mod.rpt2 <- rpt(avespeed_tot.sqrt ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
KARR_avespeed_tot.sqrt.mod.rpt2

KARR_avespeed_mob.mod.rpt1 <- rpt(avespeed_mob ~ (1 | FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
KARR_avespeed_mob.mod.rpt1
KARR_avespeed_mob.mod.rpt2 <- rpt(avespeed_mob ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
KARR_avespeed_mob.mod.rpt2

KARR_aveacceler.mod.rpt1 <- rpt(aveacceler ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
KARR_aveacceler.mod.rpt1
KARR_aveacceler.mod.rpt2 <- rpt(aveacceler ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
KARR_aveacceler.mod.rpt2

KARR_propmoving.mod.rpt1 <- rpt(propmoving ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
KARR_propmoving.mod.rpt1
KARR_propmoving.mod.rpt2 <- rpt(propmoving ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
KARR_propmoving.mod.rpt2

KARR_dist.sqrt.mod.rpt1 <- rpt(dist.sqrt ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
KARR_dist.sqrt.mod.rpt1
KARR_dist.sqrt.mod.rpt2 <- rpt(dist.sqrt ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
KARR_dist.sqrt.mod.rpt2

KARR_frozenevents.mod.rpt1 <- rpt(frozenevents ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
KARR_frozenevents.mod.rpt1
KARR_frozenevents.mod.rpt2 <- rpt(frozenevents ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
KARR_frozenevents.mod.rpt2

KARR_timefrozen_tot.mod.rpt1 <- rpt(timefrozen_tot ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
KARR_timefrozen_tot.mod.rpt1
KARR_timefrozen_tot.mod.rpt2 <- rpt(timefrozen_tot ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
KARR_timefrozen_tot.mod.rpt2

KARR_timefrozen_ave.ln.mod.rpt1 <- rpt(timefrozen_ave.ln ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                    nboot = 100, npermut = 0)
KARR_timefrozen_ave.ln.mod.rpt1
KARR_timefrozen_ave.ln.mod.rpt2 <- rpt(timefrozen_ave.ln ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                    nboot = 100, npermut = 0)
KARR_timefrozen_ave.ln.mod.rpt2

KARR_centretime50.sqrt.mod.rpt1 <- rpt(centretime50.sqrt ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
KARR_centretime50.sqrt.mod.rpt1
KARR_centretime50.sqrt.mod.rpt2 <- rpt(centretime50.sqrt ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
KARR_centretime50.sqrt.mod.rpt2

KARR_centretime75.sqrt.mod.rpt1 <- rpt(centretime75.sqrt ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
KARR_centretime75.sqrt.mod.rpt1
KARR_centretime75.sqrt.mod.rpt2 <- rpt(centretime75.sqrt ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
KARR_centretime75.sqrt.mod.rpt2

KARR_centretime100.sqrt.mod.rpt1 <- rpt(centretime100.sqrt ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
KARR_centretime100.sqrt.mod.rpt1
KARR_centretime100.sqrt.mod.rpt2 <- rpt(centretime100.sqrt ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
KARR_centretime100.sqrt.mod.rpt2



### K.2.4 Testing for treatment effects on survival ----
#Fish monitored for survival over 10 weeks, from 10-11-2020 to 19-01-2020
#   - When fish were found dead, time to death was set as the midpoint between the date found and the previous check date
#   - when fish euthanised (e.g. due to severe lethargy or wounds), time to death was set at the time of euthanisation
KARR_surv <- read.csv('~/trophicpersonalities_A/Data_Karrebaek/KARRfish_survival19012021.csv')


# Creating survfit object, using the Kaplan-Meier method (non-parametric approach, to create step function)
Surv(KARR_surv$SurvivalTime, KARR_surv$Status)[1:48]
Karr_surv_fn <- survfit(Surv(SurvivalTime, Status) ~ Treatment, data = KARR_surv)
names(Karr_surv_fn)


# Plotting survival (appears that CIs are not possible, likely due to >50% death, so no median is calculated)
Karr_surv_plot <- ggsurvplot(Karr_surv_fn, conf.int = FALSE, 
                             legend = "right",
                             palette = c('#45B39D', '#9B59B6', '#EB984E'),
                             xlab = "Days", 
                             ylab = "Overall survival probability")
Karr_surv_plot

ggsave(file = "~/trophicpersonalities_A/Data_Karrebaek/Fig_surv.jpg", width = 15, height = 9, units = "cm", print(Karr_surv_plot), dpi = 600)


# Testing for treatment effects
Karr_surv_treteff <- survdiff(Surv(SurvivalTime, Status) ~ Treatment, data = KARR_surv)
Karr_surv_treteff #No significant effect of treatment
1 - pchisq(Karr_surv_treteff$chisq, length(Karr_surv_treteff$n) - 1) 
# chisq = 1.8
# df = 2
# p= 0.413935



### K.2.5 Testing for treatment effects on growth ----
KARRfish_initial <- read.csv('~/trophicpersonalities_A/Data_Karrebaek/KARRfish_10112020.csv')
KARRfish_final <- read.csv('~/trophicpersonalities_A/Data_Karrebaek/KARRfish_19012021.csv')
KARRfish_final <- select(KARRfish_final, -c(Treatment, TankID, PITID))


KARRgrowth <- merge(KARRfish_initial, KARRfish_final, by = 'FishID', all.x = FALSE)
KARRgrowth <- select(KARRgrowth, -c(RANDBETWEEN.0.1000000.))
labels(KARRgrowth)

#Response variables
# Absolute growth in total length (dTL)
# Proportional growth in total length (pTL)
# Absolute growth in standard length (dSL)
# Proportional growth in standard length (pTL)
# Absolute growth in weight (dWeight)
# Proportional growth in weight (pWeight)

#Creating variables
KARRgrowth$dTL <- KARRgrowth$TL - KARRgrowth$TL_initial
KARRgrowth$pTL <- KARRgrowth$dTL/KARRgrowth$TL_initial
KARRgrowth$dSL <- KARRgrowth$SL - KARRgrowth$SL_initial
KARRgrowth$pSL <- KARRgrowth$dSL/KARRgrowth$SL_initial
KARRgrowth$dWeight <- KARRgrowth$Weight - KARRgrowth$Weight_initial
KARRgrowth$pWeight <- KARRgrowth$dWeight/KARRgrowth$Weight_initial

#Including holding tank as a random effect
KARRgrowth$TankID.combo <- KARRgrowth$TankID
#Running Tank ID as 2 categories, D and E, as these were connected
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "D_4"] <- "D"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "D_3"] <- "D"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "D_2"] <- "D"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "D_1"] <- "D"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "E_4"] <- "E"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "E_3"] <- "E"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "E_2"] <- "E"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "E_1"] <- "E"




#Using TankID as a random effect, Treatment as fixed effect
KARR_dTL.treat1 <- lmer(dTL ~ Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_dTL.treat1) #negative effect for fin clip group, as expected
Anova(KARR_dTL.treat1)

KARR_pTL.treat1 <- lmer(pTL ~ Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_pTL.treat1) #non.sig negative effect for fin clip group, as expected
Anova(KARR_pTL.treat1)



KARR_dSL.treat1 <- lmer(dSL ~ Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_dSL.treat1) #no negative effect
Anova(KARR_dSL.treat1)

KARR_pSL.treat1 <- lmer(pSL ~ Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_pSL.treat1) #non.sig negative effect for fin clip group, as expected
Anova(KARR_pSL.treat1)



KARR_dSL.dWeight <- lmer(dWeight ~ Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_dSL.dWeight) #negative effect for fin clip group, as expected
Anova(KARR_dSL.dWeight)

KARR_pWeight.treat1 <- lmer(pWeight ~ Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_pWeight.treat1) #non.sig negative effect for fin clip group, as expected
Anova(KARR_pWeight.treat1)



#Using TankID as a random effect, Sex + InfectionScore + Treatment as fixed effects
KARR_dTL.treat1 <- lmer(dTL ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_dTL.treat1) #negative effect for fin clip group, as expected
Anova(KARR_dTL.treat1)

KARR_pTL.treat1 <- lmer(pTL ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_pTL.treat1) #non.sig negative effect for fin clip group, as expected
Anova(KARR_pTL.treat1)



KARR_dSL.treat1 <- lmer(dSL ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_dSL.treat1) #no negative effect
Anova(KARR_dSL.treat1)

KARR_pSL.treat1 <- lmer(pSL ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_pSL.treat1) #non.sig negative effect for fin clip group, as expected
Anova(KARR_pSL.treat1)



KARR_dSL.dWeight <- lmer(dWeight ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_dSL.dWeight) #negative effect for fin clip group, as expected
Anova(KARR_dSL.dWeight)

KARR_pWeight.treat1 <- lmer(pWeight ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
summary(KARR_pWeight.treat1) #non.sig negative effect for fin clip group, as expected
Anova(KARR_pWeight.treat1)


