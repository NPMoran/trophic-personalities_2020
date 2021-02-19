# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#### Karrebaek 2. Treatment Analysis ####

Sys.setenv(LANG = "en")

#Loading required packages- 
library(dplyr); library(ggplot2); library(ggpubr)
library(lme4); library(lmerTest); library(car); library(rptR); library(performance)
library(survival); library(survminer)

#Creating a general theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, size = 1))


## K.2.1 Treatment effects on behaviour ----
#Loading required datasets-
KARRact.processed <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARR_ACTdat_processed.csv")
labels(KARRact.processed)


#Z-transformation/scaling of continuous fixed effects
KARRact.processed$TL.C <- scale(KARRact.processed$TL)  
KARRact.processed$ConditionFactor.C <- scale(KARRact.processed$ConditionFactor)  
KARRact.processed$InfectionScore.C <- scale(KARRact.processed$InfectionScore)  


#Pre-checking distributions
ggplot(KARRact.processed) + aes(x = dist) + geom_histogram(color="black", fill="lightblue", binwidth = 4800) + simpletheme 
ggqqplot(KARRact.processed$dist) #positive skew
ggplot(KARRact.processed) + aes(x = sqrt(dist)) + geom_histogram(color="black", fill="lightblue", binwidth = 15) + simpletheme 
ggqqplot(sqrt(KARRact.processed$dist)) #root transformation is improved, but still shows censoring at 0


ggplot(KARRact.processed) + aes(x = avespeed_mob) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(KARRact.processed$avespeed_mob) #majority of data appears normal with 5 - 10 very inactive fish


ggplot(KARRact.processed) + aes(x = propmoving) + geom_histogram(color="black", fill="lightblue", binwidth = 0.05) + simpletheme 
ggqqplot(KARRact.processed$propmoving) #majority of data appears normal with 5 - 10 very inactive fish
ggplot(KARRact.processed) + aes(x = sqrt(propmoving)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.05) + simpletheme 
ggqqplot(sqrt(KARRact.processed$propmoving)) #sqrt transformation is improved


ggplot(KARRact.processed) + aes(x = centrescore) + geom_histogram(color="black", fill="lightblue", binwidth = 0.2) + simpletheme 
ggqqplot(KARRact.processed$centrescore) #minimal positive skew 
ggplot(KARRact.processed) + aes(x = sqrt(centrescore)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.05) + simpletheme 
ggqqplot(sqrt(KARRact$centrescore)) #root transformation is improved


#ggplot(KARRact.processed) + aes(x = avespeed_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 4.5) + simpletheme 
#ggqqplot(KARRact.processed$avespeed_tot) #positive skew
#ggplot(KARRact.processed) + aes(x = sqrt(avespeed_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
#ggqqplot(sqrt(KARRact.processed$avespeed_tot)) #sqrt transformation is improved, some censoring at low end
#
#ggplot(KARRact.processed) + aes(x = timefrozen_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
#ggqqplot(KARRact.processed$timefrozen_tot) #binomial would be better
#ggplot(KARRact.processed) + aes(x = sqrt(1200-timefrozen_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 2.5) + simpletheme 
#ggqqplot(sqrt(1200-KARRact.processed$timefrozen_tot)) #root transformation is improved, but still shows censoring at 0
#
#ggplot(KARRact.processed) + aes(x = centretime50) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
#ggqqplot(KARRact.processed$centretime50) #root transformation is improved, but still shows censoring at 0
#ggplot(KARRact.processed) + aes(x = sqrt(centretime50)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
#ggqqplot(sqrt(KARRact$centretime50)) #root transformation is improved, but still shows censoring at 0
#
#ggplot(KARRact.processed) + aes(x = centretime75) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
#ggqqplot(KARRact.processed$centretime75) #positive skew 
#ggplot(KARRact.processed) + aes(x = sqrt(centretime75)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
#ggqqplot(sqrt(KARRact$centretime75)) #root transformation is improved, but still shows censoring at 0



#Full Models- including all random and fixed effects

#  Variable        Transformation/Distribution    Random effects                                Fixed effects
#  dist.sqrt       sqrt/Gaussian                  TankID.combo, ArenaID, FishID, TrialRound     Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay
#  propmoving      sqrt/Gaussian                  TankID.combo, ArenaID, FishID, TrialRound     Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay
#  avespeed_mob    nil/Gaussian                   TankID.combo, ArenaID, FishID, TrialRound     Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay
#  centrescore     sqrt/Gaussian                  TankID.combo, ArenaID, FishID, TrialRound     Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay


#dist: (mm) total distance travelled during trial
KARR_dist.sqrt <- lmer(sqrt(dist) ~ 
                         Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) +  (1|FishID), data=KARRact.processed)
Anova(KARR_dist.sqrt)                 #No Sex, TL.C, ConditionFactor.C, InfectionScore.C effects
summary(KARR_dist.sqrt)               #All random effects resolve some variance
plot(KARR_dist.sqrt)                  #No clustering issues
r2_nakagawa(KARR_dist.sqrt)


#propmoving: (proportional) proportion of time mobile
KARR_propmoving.sqrt <- lmer(sqrt(propmoving) ~ 
                               Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) +  (1|FishID), data=KARRact.processed)
Anova(KARR_propmoving.sqrt)           #No Sex, TL.C, ConditionFactor.C, InfectionScore.C effects
summary(KARR_propmoving.sqrt)         #All random effects resolve some variance
plot(KARR_propmoving.sqrt)            #No clustering issues
r2_nakagawa(KARR_propmoving.sqrt)


#avespeed_mob: (mm/s) the average speed of the individual excluding periods when it was immobile
KARR_avespeed_mob.mod <- lmer(avespeed_mob ~ 
                                Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), data=KARRact.processed)
Anova(KARR_avespeed_mob.mod)          #InfectionScore.C positive effect
summary(KARR_avespeed_mob.mod)        #All random effects resolve some variance
plot(KARR_avespeed_mob.mod)           #No clustering issues
r2_nakagawa(KARR_avespeed_mob.mod)


#centrescore: (NA) calculated from the proportion of time spent in each area
KARR_centrescore.sqrt <- lmer(sqrt(centrescore) ~ 
                                Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) +  (1|FishID), data=KARRact.processed)
Anova(KARR_centrescore.sqrt)          #ConditionFactor effect, TrialDay effect  
summary(KARR_centrescore.sqrt)        #All random effects resolve some variance
plot(KARR_centrescore.sqrt)           #Stunningly beautiful
r2_nakagawa(KARR_centrescore.sqrt)    #ArenaID and TrialRound resolved no variance


save(KARR_dist.sqrt, file = "./Output_Karrebaek/KARR_dist.sqrt.RData")
save(KARR_propmoving.sqrt, file = "./Output_Karrebaek/KARR_propmoving.sqrt.RData")
save(KARR_avespeed_mob.mod, file = "./Output_Karrebaek/KARR_avespeed_mob.mod.RData")
save(KARR_centrescore.sqrt, file = "./Output_Karrebaek/KARR_centrescore.sqrt.RData")


##avespeed_tot: (mm/s) the average speed of the individual across the full trial period
#KARR_avespeed_tot.sqrt.mod <- lmer(sqrt(avespeed_tot) ~ 
#                                            Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), data=KARRact.processed)
#Anova(KARR_avespeed_tot.sqrt.mod)     #No Sex, TL.C, ConditionFactor.C, InfectionScore.C effects
#summary(KARR_avespeed_tot.sqrt.mod)   #All random effects resolve some variance
#plot(KARR_avespeed_tot.sqrt.mod)      #No clustering issues
#r2_nakagawa(KARR_avespeed_tot.sqrt.mod)
#
#
##timefrozen_tot: (sec) total time spend frozen 
#KARR_timefrozen_tot.invsqrt.sqrt <- lmer(sqrt(1200-timefrozen_tot) ~ 
#                         Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) +  (1|FishID), data=KARRact.processed)
#Anova(KARR_timefrozen_tot.invsqrt.sqrt)                 #No Sex, TL.C, ConditionFactor.C, InfectionScore.C effects
#summary(KARR_timefrozen_tot.invsqrt.sqrt)               #All random effects resolve some variance
#plot(KARR_timefrozen_tot.invsqrt.sqrt)                  #No clustering issues
#r2_nakagawa(KARR_timefrozen_tot.invsqrt.sqrt)
#
#
##centretime50: (s) time >5cm away from edge
#KARR_centretime50.sqrt <- lmer(sqrt(centretime50) ~ 
#                                        Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) +  (1|FishID), data=KARRact.processed)
#Anova(KARR_centretime50.sqrt)         #ConditionFactor.C effect
#summary(KARR_centretime50.sqrt)       #ArenaID, TrialRound V = 0.000
#plot(KARR_centretime50.sqrt)          #Some evidence of censoring
#r2_nakagawa(KARR_centretime50.sqrt)   #Need to revise random effect structure
#
#
##centretime75: (s) time >7.5cm away from edge
#KARR_centretime75.sqrt <- lmer(sqrt(centretime75) ~ 
#                                        Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) +  (1|FishID), data=KARRact.processed)
#Anova(KARR_centretime75.sqrt)         #ConditionFactor.C effect
#summary(KARR_centretime75.sqrt)       #ArenaID V = 0.000, TrialRound V = 1.601e-08
#plot(KARR_centretime75.sqrt)          #Evidence of censoring
#r2_nakagawa(KARR_centretime75.sqrt)   #Need to revise random effect structure
#
#
##centretime100: (s) time >10cm away from edge
#KARR_centretime100.sqrt <- lmer(sqrt(centretime100) ~ 
#                                         Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) +  (1|FishID), data=KARRact.processed)
#Anova(KARR_centretime100.sqrt)        #ConditionFactor effect, Treatment effect  
#summary(KARR_centretime100.sqrt)      #All random effects resolve some variance
#plot(KARR_centretime100.sqrt)         #Evidence of censoring
#r2_nakagawa(KARR_centretime100.sqrt)



#Reduced Models- removing random effects that failed to resolve variance, and fixed effects with insignificant effect
#  Variable        Transformation/Distribution    Random effects                                Fixed effects
#  dist.sqrt       sqrt/Gaussian                  TankID.combo, ArenaID, FishID, TrialRound     Treatment*TrialDay
#  propmoving      sqrt/Gaussian                  TankID.combo, ArenaID, FishID, TrialRound     Treatment*TrialDay
#  avespeed_mob    nil/Gaussian                   TankID.combo, ArenaID, FishID, TrialRound     InfectionScore + Treatment*TrialDay
#  centrescore     sqrt/Gaussian                  TankID.combo, FishID,                         ConditionFactor + Treatment*TrialDay


#dist: (mm) total distance travelled during trial
KARR_dist.sqrt.red <- lmer(sqrt(dist) ~ 
                             Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), data=KARRact.processed)
Anova(KARR_dist.sqrt.red)                 #No Sex, TL.C, ConditionFactor.C, InfectionScore.C effects
summary(KARR_dist.sqrt.red)               #All random effects resolve some variance
plot(KARR_dist.sqrt.red)                  #No clustering issues
r2_nakagawa(KARR_dist.sqrt.red)


#propmoving: (proportional) proportion of time mobile
KARR_propmoving.sqrt.red <- lmer(sqrt(propmoving) ~ 
                                   Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), data=KARRact.processed)
Anova(KARR_propmoving.sqrt.red)           #No Sex, TL.C, ConditionFactor.C, InfectionScore.C effects
summary(KARR_propmoving.sqrt.red)         #All random effects resolve some variance
plot(KARR_propmoving.sqrt.red)            #No clustering issues
r2_nakagawa(KARR_propmoving.sqrt.red)


#avespeed_mob: (mm/s) the average speed of the individual excluding periods when it was immobile
KARR_avespeed_mob.mod.red <- lmer(avespeed_mob ~ 
                                    InfectionScore.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), data=KARRact.processed)
Anova(KARR_avespeed_mob.mod.red)          #
summary(KARR_avespeed_mob.mod.red)        #InfectionScore.C positive effect
plot(KARR_avespeed_mob.mod.red)           #No clustering issues
r2_nakagawa(KARR_avespeed_mob.mod.red)


#centrescore: (NA) calculated from the proportion of time spent in each area
KARR_centrescore.sqrt.red <- lmer(sqrt(centrescore) ~ 
                                    ConditionFactor.C + Treatment*TrialDay + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
Anova(KARR_centrescore.sqrt.red)          #ConditionFactor effect, Treatment effect  
summary(KARR_centrescore.sqrt.red)        #All random effects resolve some variance
plot(KARR_centrescore.sqrt.red)           #Evidence of censoring
r2_nakagawa(KARR_centrescore.sqrt.red)    #performs better than other versions


save(KARR_dist.sqrt.red, file = "./Output_Karrebaek/KARR_dist.sqrt.red.RData")
save(KARR_propmoving.sqrt.red, file = "./Output_Karrebaek/KARR_propmoving.sqrt.red.RData")
save(KARR_avespeed_mob.mod.red, file = "./Output_Karrebaek/KARR_avespeed_mob.mod.red.RData")
save(KARR_centrescore.sqrt.red, file = "./Output_Karrebaek/KARR_centrescore.sqrt.red.RData")


##avespeed_tot: (mm/s) the average speed of the individual across the full trial period
#KARR_avespeed_tot.sqrt.mod.red <- lmer(sqrt(avespeed_tot) ~ 
#                                     Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), data=KARRact.processed)
#Anova(KARR_avespeed_tot.sqrt.mod.red)     #
#summary(KARR_avespeed_tot.sqrt.mod.red)   #TreatmentPITtagged:TrialDayDay 2
#plot(KARR_avespeed_tot.sqrt.mod.red)      #No clustering issues
#r2_nakagawa(KARR_avespeed_tot.sqrt.mod.red)
#
#
##timefrozen_tot: (sec) total time spend frozen 
#KARR_timefrozen_tot.invsqrt.sqrt.red <- lmer(sqrt(1200-timefrozen_tot) ~ 
#                                          Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) +  (1|FishID), data=KARRact.processed)
#Anova(KARR_timefrozen_tot.invsqrt.sqrt.red)                 #No Sex, TL.C, ConditionFactor.C, InfectionScore.C effects
#summary(KARR_timefrozen_tot.invsqrt.sqrt.red)               #All random effects resolve some variance
#plot(KARR_timefrozen_tot.invsqrt.sqrt.red)                  #No clustering issues
#r2_nakagawa(KARR_timefrozen_tot.invsqrt.sqrt.red)
#
#
##centretime50: (s) time >5cm away from edge
#KARR_centretime50.sqrt.red <- lmer(sqrt(centretime50) ~ 
#                                 ConditionFactor.C + Treatment*TrialDay + (1|TankID.combo) +  (1|FishID), data=KARRact.processed)
#Anova(KARR_centretime50.sqrt.red)         #ConditionFactor.C effect
#summary(KARR_centretime50.sqrt.red)       #ArenaID, TrialRound V = 0.000
#plot(KARR_centretime50.sqrt.red)          #Some evidence of censoring
#r2_nakagawa(KARR_centretime50.sqrt.red)
#
#
##centretime75: (s) time >7.5cm away from edge
#KARR_centretime75.sqrt.red <- lmer(sqrt(centretime75) ~ 
#                                        ConditionFactor.C + Treatment*TrialDay + (1|TankID.combo) +  (1|FishID), data=KARRact.processed)
#Anova(KARR_centretime75.sqrt.red)         #ConditionFactor.C effect
#summary(KARR_centretime75.sqrt.red)       #ArenaID V = 0.000, TrialRound V = 1.601e-08
#plot(KARR_centretime75.sqrt.red)          #Evidence of censoring
#r2_nakagawa(KARR_centretime75.sqrt.red)
#
#
##centretime100: (s) time >10cm away from edge
#KARR_centretime100.sqrt.red <- lmer(sqrt(centretime100) ~ 
#                                         ConditionFactor.C + Treatment*TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) +  (1|FishID), data=KARRact.processed)
#Anova(KARR_centretime100.sqrt.red)        #ConditionFactor effect, Treatment effect  
#summary(KARR_centretime100.sqrt.red)      #All random effects resolve some variance
#plot(KARR_centretime100.sqrt.red)         #Evidence of censoring
#r2_nakagawa(KARR_centretime100.sqrt.red)


## K.2.2 Testing for treatment effects on repeatabilities ----

#Calculating:             
# - Raw Repeatability       
# - Adj Repeatability, no Treatment effect included      
# - Adj Repeatability, Treatment effect included


#Using using same model parameters as the reduced models above
KARR_dist.sqrt.mod.rpt1 <- rpt(sqrt(dist) ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
KARR_dist.sqrt.mod.rpt1
KARR_dist.sqrt.mod.rpt2 <- rpt(sqrt(dist) ~ TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
KARR_dist.sqrt.mod.rpt2
KARR_dist.sqrt.mod.rpt3 <- rpt(sqrt(dist) ~ TrialDay*Treatment + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
KARR_dist.sqrt.mod.rpt3


KARR_propmoving.sqrt.mod.rpt1 <- rpt(sqrt(propmoving) ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
KARR_propmoving.sqrt.mod.rpt1
KARR_propmoving.sqrt.mod.rpt2 <- rpt(sqrt(propmoving) ~ TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
KARR_propmoving.sqrt.mod.rpt2
KARR_propmoving.sqrt.mod.rpt3 <- rpt(sqrt(propmoving) ~ TrialDay*Treatment + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
KARR_propmoving.sqrt.mod.rpt3


KARR_avespeed_mob.mod.rpt1 <- rpt(avespeed_mob ~ (1 | FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
KARR_avespeed_mob.mod.rpt1
KARR_avespeed_mob.mod.rpt2 <- rpt(avespeed_mob ~ TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
KARR_avespeed_mob.mod.rpt2
KARR_avespeed_mob.mod.rpt3 <- rpt(avespeed_mob ~ TrialDay*Treatment + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
KARR_avespeed_mob.mod.rpt3


KARR_centrescore.sqrt.mod.rpt1 <- rpt(sqrt(centrescore) ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                        nboot = 100, npermut = 0)
KARR_centrescore.sqrt.mod.rpt1
KARR_centrescore.sqrt.mod.rpt2 <- rpt(sqrt(centrescore) ~ TrialDay + (1|TankID.combo) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                        nboot = 100, npermut = 0)
KARR_centrescore.sqrt.mod.rpt2
KARR_centrescore.sqrt.mod.rpt3 <- rpt(sqrt(centrescore) ~ TrialDay*Treatment + (1|TankID.combo) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
                                        nboot = 100, npermut = 0)
KARR_centrescore.sqrt.mod.rpt3


save(KARR_dist.sqrt.mod.rpt1, file = "./Output_Karrebaek/KARR_dist.sqrt.mod.rpt1.RData")
save(KARR_dist.sqrt.mod.rpt2, file = "./Output_Karrebaek/KARR_dist.sqrt.mod.rpt2.RData")
save(KARR_dist.sqrt.mod.rpt3, file = "./Output_Karrebaek/KARR_dist.sqrt.mod.rpt3.RData")

save(KARR_propmoving.sqrt.mod.rpt1, file = "./Output_Karrebaek/KARR_propmoving.sqrt.mod.rpt1.RData")
save(KARR_propmoving.sqrt.mod.rpt2, file = "./Output_Karrebaek/KARR_propmoving.sqrt.mod.rpt2.RData")
save(KARR_propmoving.sqrt.mod.rpt3, file = "./Output_Karrebaek/KARR_propmoving.sqrt.mod.rpt3.RData")

save(KARR_avespeed_mob.mod.rpt1, file = "./Output_Karrebaek/KARR_avespeed_mob.mod.rpt1.RData")
save(KARR_avespeed_mob.mod.rpt2, file = "./Output_Karrebaek/KARR_avespeed_mob.mod.rpt2.RData")
save(KARR_avespeed_mob.mod.rpt3, file = "./Output_Karrebaek/KARR_avespeed_mob.mod.rpt3.RData")

save(KARR_centrescore.sqrt.mod.rpt1, file = "./Output_Karrebaek/KARR_centrescore.sqrt.mod.rpt1.RData")
save(KARR_centrescore.sqrt.mod.rpt2, file = "./Output_Karrebaek/KARR_centrescore.sqrt.mod.rpt2.RData")
save(KARR_centrescore.sqrt.mod.rpt3, file = "./Output_Karrebaek/KARR_centrescore.sqrt.mod.rpt3.RData")


#KARR_avespeed_tot.sqrt.mod.rpt1 <- rpt(sqrt(avespeed_tot) ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                       nboot = 100, npermut = 0)
#KARR_avespeed_tot.sqrt.mod.rpt1
#KARR_avespeed_tot.sqrt.mod.rpt2 <- rpt(sqrt(avespeed_tot) ~ TrialDay + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                       nboot = 100, npermut = 0)
#KARR_avespeed_tot.sqrt.mod.rpt2
#KARR_avespeed_tot.sqrt.mod.rpt3 <- rpt(sqrt(avespeed_tot) ~ TrialDay*Treatment + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound) + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                       nboot = 100, npermut = 0)
#KARR_avespeed_tot.sqrt.mod.rpt3
#
#
#KARR_timefrozen_tot.invsqrt.mod.rpt1 <- rpt(sqrt(1200-timefrozen_tot) ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                            nboot = 100, npermut = 0)
#KARR_timefrozen_tot.invsqrt.mod.rpt1
#KARR_timefrozen_tot.invsqrt.mod.rpt2 <- rpt(sqrt(1200-timefrozen_tot) ~ TrialDay + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                            nboot = 100, npermut = 0)
#KARR_timefrozen_tot.invsqrt.mod.rpt2
#KARR_timefrozen_tot.invsqrt.mod.rpt3 <- rpt(sqrt(1200-timefrozen_tot) ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                            nboot = 100, npermut = 0)
#KARR_timefrozen_tot.invsqrt.mod.rpt3
#
#
#KARR_centretime50.sqrt.mod.rpt1 <- rpt(sqrt(centretime50) ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                       nboot = 100, npermut = 0)
#KARR_centretime50.sqrt.mod.rpt1
#KARR_centretime50.sqrt.mod.rpt2 <- rpt(sqrt(centretime50) ~ TrialDay + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                       nboot = 100, npermut = 0)
#KARR_centretime50.sqrt.mod.rpt2
#KARR_centretime50.sqrt.mod.rpt3 <- rpt(sqrt(centretime50) ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                       nboot = 100, npermut = 0)
#KARR_centretime50.sqrt.mod.rpt3
#
#
#KARR_centretime75.sqrt.mod.rpt1 <- rpt(sqrt(centretime75) ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                       nboot = 100, npermut = 0)
#KARR_centretime75.sqrt.mod.rpt1
#KARR_centretime75.sqrt.mod.rpt2 <- rpt(sqrt(centretime75) ~ TrialDay + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                       nboot = 100, npermut = 0)
#KARR_centretime75.sqrt.mod.rpt2
#KARR_centretime75.sqrt.mod.rpt3 <- rpt(sqrt(centretime75) ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                       nboot = 100, npermut = 0)
#KARR_centretime75.sqrt.mod.rpt3
#
#
#KARR_centretime100.sqrt.mod.rpt1 <- rpt(sqrt(centretime100) ~ (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                        nboot = 100, npermut = 0)
#KARR_centretime100.sqrt.mod.rpt1
#KARR_centretime100.sqrt.mod.rpt2 <- rpt(sqrt(centretime100) ~ TrialDay + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                        nboot = 100, npermut = 0)
#KARR_centretime100.sqrt.mod.rpt2
#KARR_centretime100.sqrt.mod.rpt3 <- rpt(sqrt(centretime100) ~ TrialDay*Treatment + (1|FishID), grname = "FishID", data = KARRact.processed, datatype = "Gaussian", 
#                                        nboot = 100, npermut = 0)
#KARR_centretime100.sqrt.mod.rpt3



## K.2.3 Testing for treatment effects on survival ----
#Fish monitored for survival over 10 weeks, from 10-11-2020 to 19-01-2020
#   - When fish were found dead, time to death was set as the midpoint between the date found and the previous check date
#   - when fish euthanised (e.g. due to severe lethargy or wounds), time to death was set at the time of euthanisation

#Loading required datasets-
KARR_surv <- read.csv('~/trophicpersonalities_A/Data_Karrebaek/KARRfish_survival19012021.csv')

KARR_surv$Treatment[KARR_surv$Treatment == 'cont'] <- "control"
KARR_surv$Treatment[KARR_surv$Treatment == 'finclip'] <- "PIT+clip"
KARR_surv$Treatment[KARR_surv$Treatment == 'pit'] <- "PITtagged"
KARR_surv$Treatment <- ordered(KARR_surv$Treatment, levels = c("control","PITtagged","PIT+clip"))

# Creating survfit object, using the Kaplan-Meier method (non-parametric approach, to create step function)
Surv(KARR_surv$SurvivalTime, KARR_surv$Status)[1:48]
Karr_surv_fn <- survfit(Surv(SurvivalTime, Status) ~ Treatment, data = KARR_surv)
names(Karr_surv_fn)
save(Karr_surv_fn, file = "./Output_Karrebaek/Karr_surv_fn.RData")

# Plotting survival (appears that CIs are not possible, likely due to >50% death, so no median is calculated)
Karr_surv_plot <- ggsurvplot(Karr_surv_fn, conf.int = FALSE, 
                             legend = "right",
                             palette = c("black","seashell3","red"),
                             xlab = "Days", 
                             ylab = "Overall survival probability")
Karr_surv_plot <- Karr_surv_plot$plot + geom_vline(xintercept = 70, linetype = "longdash")
Karr_surv_plot

ggsave(file = "~/trophicpersonalities_A/Output_Karrebaek/Fig_surv.jpg", width = 18, height = 9, units = "cm", print(Karr_surv_plot), dpi = 600)


# Testing for treatment effects
Karr_surv_treteff <- survdiff(Surv(SurvivalTime, Status) ~ Treatment, data = KARR_surv)
Karr_surv_treteff #No significant effect of treatment
1 - pchisq(Karr_surv_treteff$chisq, length(Karr_surv_treteff$n) - 1) 
# chisq = 1.8
# df = 2
# p= 0.413935

save(Karr_surv_treteff, file = "./Output_Karrebaek/Karr_surv_treteff.RData")



## K.2.4 Testing for treatment effects on growth ----
#Loading required datasets-
KARRfish_initial <- read.csv('~/trophicpersonalities_A/Data_Karrebaek/KARRfish_10112020.csv')
KARRfish_final <- read.csv('~/trophicpersonalities_A/Data_Karrebaek/KARRfish_19012021.csv')
KARRfish_final <- select(KARRfish_final, -c(Treatment, TankID, PITID))

#Calculating initial and final condition factors
KARRfish_initial$ConditionFactor <- 100* (KARRfish_initial$Weight / (KARRfish_initial$TL)^3)
KARRfish_final$ConditionFactor <- 100* (KARRfish_final$Weight / (KARRfish_final$TL)^3)

#Merging databases
KARRgrowth <- merge(KARRfish_initial, KARRfish_final, by = 'FishID', all.x = FALSE)
KARRgrowth <- select(KARRgrowth, -c(RANDBETWEEN.0.1000000.))
labels(KARRgrowth)

#Formatting variables (to order Treatment levels so that PITtagged and PIT+clip groups are compared to control groups)
KARRgrowth$Treatment[KARRgrowth$Treatment == 'cont'] <- 1
KARRgrowth$Treatment[KARRgrowth$Treatment == 'pit'] <- 2
KARRgrowth$Treatment[KARRgrowth$Treatment == 'finclip'] <- 3
KARRgrowth$Treatment <- as.factor(KARRgrowth$Treatment)


#Running Tank ID as 2 categories, D and E, as these were connected
KARRgrowth$TankID.combo <- KARRgrowth$TankID
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "D_4"] <- "D"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "D_3"] <- "D"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "D_2"] <- "D"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "D_1"] <- "D"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "E_4"] <- "E"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "E_3"] <- "E"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "E_2"] <- "E"
KARRgrowth$TankID.combo[KARRgrowth$TankID.combo == "E_1"] <- "E"


#Response variables
# Change in total length (dTL)
# Change in standard length (dSL)
# Change in weight (dWeight)
# Change in ConditionFactor (dConditionFactor)


#Creating variables
KARRgrowth$dTL <- KARRgrowth$TL.y - KARRgrowth$TL.x
KARRgrowth$dSL <- KARRgrowth$SL.y - KARRgrowth$SL.x
KARRgrowth$dWeight <- KARRgrowth$Weight.y - KARRgrowth$Weight.x
KARRgrowth$dConditionFactor <- KARRgrowth$ConditionFactor.y - KARRgrowth$ConditionFactor.x


#Using TankID as a random effect, Sex + InfectionScore + Treatment as fixed effects
KARR_treat.dTL <- lmer(dTL ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
Anova(KARR_treat.dTL) #Sex effect (positive effect for males)
summary(KARR_treat.dTL) #Substantial prop of variance associated with TankID
plot(KARR_treat.dTL) #no patterns
r2_nakagawa(KARR_treat.dTL) #Conditional R2: 0.485, Marginal R2: 0.212

KARR_treat.dSL <- lmer(dSL ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
Anova(KARR_treat.dSL) #No fixed effects
summary(KARR_treat.dSL) #Some variance associated with TankID
plot(KARR_treat.dSL) #no patterns
r2_nakagawa(KARR_treat.dSL) #  Conditional R2: 0.244, Marginal R2: 0.135

KARR_treat.dWeight <- lmer(dWeight ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
Anova(KARR_treat.dWeight) #Sex effect (positive effect for males)
summary(KARR_treat.dWeight) #Very little variance associated with TankID
plot(KARR_treat.dWeight) #no patterns
r2_nakagawa(KARR_treat.dWeight) # Conditional R2: 0.166, Marginal R2: 0.165

KARR_treat.dConditionFactor <- lmer(dConditionFactor ~ Sex + InfectionScore + Treatment + (1|TankID.combo), data=KARRgrowth)
Anova(KARR_treat.dConditionFactor) #No fixed effects
summary(KARR_treat.dConditionFactor) #Substantial prop of variance associated with TankID
Anova(KARR_treat.dConditionFactor)#no patterns
r2_nakagawa(KARR_treat.dConditionFactor) #Conditional R2: 0.408, Marginal R2: 0.073

save(KARR_treat.dTL, file = "./Output_Karrebaek/KARR_treat.dTL.RData")
save(KARR_treat.dSL, file = "./Output_Karrebaek/KARR_treat.dSL.RData")
save(KARR_treat.dWeight, file = "./Output_Karrebaek/KARR_treat.dWeight.RData")
save(KARR_treat.dConditionFactor, file = "./Output_Karrebaek/KARR_treat.dConditionFactor.RData")


#Saving growth data
labels(KARRgrowth)
KARRgrowth$Treatment <- as.numeric(KARRgrowth$Treatment)
KARRgrowth$Treatment[KARRgrowth$Treatment == 1] <- 'control'
KARRgrowth$Treatment[KARRgrowth$Treatment == 2] <- 'PITtagged'
KARRgrowth$Treatment[KARRgrowth$Treatment == 3] <- 'PIT+clip'
KARRgrowth <- select(KARRgrowth, -c(Notes.x, Notes.y, Weight.x, Weight.y, TL.x, TL.y, ConditionFactor.x, ConditionFactor.y))
write.csv(KARRgrowth, "~/trophicpersonalities_A/Data_Karrebaek/KARRgrowth.csv")




#### #### 


