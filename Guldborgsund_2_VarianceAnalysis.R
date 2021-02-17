# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



#### Guldborgsund 2. Variance Analysis ####

Sys.setenv(LANG = "en")

#Loading required packages- 
library(dplyr); library(ggplot2); 
library(lme4); library(lmerTest); library(car); library(rptR); library(performance)

#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, size = 1))

## G.2.1. ACT distributions and variance analysis ----
GULDact.processed <- read.csv("~/trophicpersonalities_A/Data_Guldborgsund/GULD_ACTdat_processed.csv")
nrow(GULDact.processed) #118 rows
n_distinct(GULDact.processed$FishID) #43 fish included in analysis

#Z-transformation/scaling of continuous fixed effects
GULDact.processed$TL.C <- scale(GULDact.processed$TL)  
GULDact.processed$ConditionFactor.C <- scale(GULDact.processed$ConditionFactor)  
GULDact.processed$InfectionScore.C <- scale(GULDact.processed$InfectionScore)  


# in: avespeed_tot
#     avespeed_mob
#     aveaccler
#     propmoving
#     dist        
#     frozenevents
#     timefrozen_tot
#     centrescore
#     centretime50        
#     centretime75
#     centretime100 

#Assessing distributions-
#avespeed_tot: (mm/s) the average speed of the individual accross the full trial period
ggplot(GULDact.processed) + aes(x = avespeed_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 4.5) + simpletheme 
ggqqplot(GULDact.processed$avespeed_tot) #approximately normal

#avespeed_mob: (mm/s) the average speed of the individual excluding periods when it was immobile
ggplot(GULDact.processed) + aes(x = avespeed_mob) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(GULDact.processed$avespeed_mob) #minor issue with 4-5 outliers at the very low end

#aveacceler: (mm/s^2) average rate of acceleration accross the trial
ggplot(GULDact.processed) + aes(x = aveacceler) + geom_histogram(color="black", fill="lightblue", binwidth = 14) + simpletheme 
ggqqplot(GULDact.processed$aveacceler) #approximately normal

#propmoving: (proportional) proportion of time mobile
ggplot(GULDact.processed) + aes(x = propmoving) + geom_histogram(color="black", fill="lightblue", binwidth = 0.055) + simpletheme 
ggqqplot(GULDact.processed$propmoving) #some left skew
ggplot(GULDact.processed) + aes(x = log(1 - propmoving)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.1) + simpletheme 
ggqqplot(log(1 - GULDact.processed$propmoving)) #inverse log approximately normals

#dist: (mm) total distance travelled during trial
ggplot(GULDact.processed) + aes(x = dist) + geom_histogram(color="black", fill="lightblue", binwidth = 4800) + simpletheme 
ggqqplot(GULDact.processed$dist)  #minor issue with 5-10 outliers at the very low end

#frozenevents: (count) the number of times spent frozen during trial (min. frozen period 3 secs)
ggplot(GULDact.processed) + aes(x = frozenevents) + geom_histogram(color="black", fill="lightblue", binwidth = 8) + simpletheme 
ggqqplot(GULDact.processed$frozenevents)  #right skewed
ggplot(GULDact.processed) + aes(x = sqrt(frozenevents)) + geom_histogram(color="black", fill="lightblue", binwidth = 1) + simpletheme 
ggqqplot(sqrt(GULDact.processed$frozenevents)) #approximately normal

#timefrozen_tot: (s) total time spent frozen during trial
ggplot(GULDact.processed) + aes(x = timefrozen_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 85) + simpletheme 
ggqqplot(GULDact.processed$timefrozen_tot) #right skewed 
ggplot(GULDact.processed) + aes(x = sqrt(timefrozen_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
ggqqplot(sqrt(GULDact.processed$timefrozen_tot)) #sqrt transformation closer to normal

#centrescore: (NA) calculated from the proportion of time spent in each area
ggplot(GULDact.processed) + aes(x = centrescore) + geom_histogram(color="black", fill="lightblue", binwidth = 0.2) + simpletheme 
ggqqplot(GULDact.processed$centrescore) #potential minimal positive skew
ggplot(GULDact.processed) + aes(x = sqrt(centrescore)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.05) + simpletheme 
ggqqplot(sqrt(GULDact.processed$centrescore)) #sqrt transforamtion is better but maybe not neccessary

#centretime50: (s) time >5cm away from edge
ggplot(GULDact.processed) + aes(x = centretime50) + geom_histogram(color="black", fill="lightblue", binwidth = 50) + simpletheme 
ggqqplot(GULDact.processed$centretime50) #positive skew
ggplot(GULDact.processed) + aes(x = sqrt(centretime50)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
ggqqplot(sqrt(GULDact.processed$centretime50)) #sqrt transformation close to normal

#centretime75: (s) time >7.5cm away from edge
ggplot(GULDact.processed) + aes(x = centretime75) + geom_histogram(color="black", fill="lightblue", binwidth = 50) + simpletheme 
ggqqplot(GULDact.processed$centretime75) #strong positive skew
ggplot(GULDact.processed) + aes(x = sqrt(centretime75)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
ggqqplot(sqrt(GULDact.processed$centretime75)) #sqrt transformation closer to normal

#centretime100: (s) time >10cm away from edge
ggplot(GULDact.processed) + aes(x = centretime100) + geom_histogram(color="black", fill="lightblue", binwidth = 10) + simpletheme 
ggqqplot(GULDact.processed$centretime100) #strong skew
ggplot(GULDact.processed) + aes(x = sqrt(centretime100)) + geom_histogram(color="black", fill="lightblue", binwidth = 1) + simpletheme 
ggqqplot(sqrt(GULDact.processed$centretime100)) #sqrt transformation closer to normal



#Full Models- including all random and fixed effects
#  All ran as linear mixed effects models as distributions were/were transformed to approximately normal

#  Variable        Transformation    Random effects                                 Fixed effects
#  avespeed_tot    nil               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  avespeed_mob    nil               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  aveaccler       nii               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  propmoving      log(1 - x)        TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  dist            nil               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  frozenevents    sqrt              TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  timefrozen_tot  sqrt              TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  centrescore     nil               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  centretime50    sqrt              TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  centretime75    sqrt              TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  centretime100   sqrt              TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore


GULD_avespeed_tot.mod <- lmer(avespeed_tot ~ 
                         Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_avespeed_tot.mod)                 #Sex, ConditionFactor.C effects
summary(GULD_avespeed_tot.mod)               #TankID resolves no variance, TrialRound extremely little
plot(GULD_avespeed_tot.mod)                  #No clustering issues
r2_nakagawa(GULD_avespeed_tot.mod)           #random structure error


GULD_avespeed_mob.mod <- lmer(avespeed_mob ~ 
                                Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_avespeed_mob.mod)                 #Sex effects
summary(GULD_avespeed_mob.mod)               #ArenaID resolves no variance, TankID extremely little
plot(GULD_avespeed_mob.mod)                  #No clustering issues
r2_nakagawa(GULD_avespeed_mob.mod)           #random structure error


GULD_aveacceler.mod <- lmer(aveacceler ~ 
                                Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_aveacceler.mod)                 #Sex effects
summary(GULD_aveacceler.mod)               #TrialRound resolves no variance, TankID extremely little
plot(GULD_aveacceler.mod)                  #No clustering issues
r2_nakagawa(GULD_aveacceler.mod)           #random structure error


GULD_propmoving.invlog.mod <- lmer(log(1-propmoving) ~ 
                              Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_propmoving.invlog.mod)                 #Sex, ConditionFactor.C effects
summary(GULD_propmoving.invlog.mod)               #TankID and TrialRound resolves no variance
plot(GULD_propmoving.invlog.mod)                  #No clustering issues
r2_nakagawa(GULD_propmoving.invlog.mod)           #random structure error


GULD_dist.mod <- lmer(dist ~ 
                                     Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_dist.mod)                 #Sex, ConditionFactor.C effects
summary(GULD_dist.mod)               #TankID resolves no variance, Trial round very little
plot(GULD_dist.mod)                  #No clustering issues
r2_nakagawa(GULD_dist.mod)           #random structure error


GULD_frozenevents.sqrt.mod <- lmer(sqrt(frozenevents) ~ 
                        Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_frozenevents.sqrt.mod)                 #ConditionFactor effect
summary(GULD_frozenevents.sqrt.mod)               #TrialRound resolves no variance, TankID extremely little
plot(GULD_frozenevents.sqrt.mod)                  #No clustering issues
r2_nakagawa(GULD_frozenevents.sqrt.mod)           #random structure error


GULD_timefrozen_tot.sqrt.mod <- lmer(sqrt(timefrozen_tot) ~ 
                                     Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_timefrozen_tot.sqrt.mod)                 #Sex, ConditionFactor.C effects
summary(GULD_timefrozen_tot.sqrt.mod)               #TankID and TrialRound resolve no variance
plot(GULD_timefrozen_tot.sqrt.mod)                  #No clustering issues
r2_nakagawa(GULD_timefrozen_tot.sqrt.mod)           #random structure error


GULD_centrescore.mod <- lmer(centrescore ~ 
                                       Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centrescore.mod)                 #TL effect
summary(GULD_centrescore.mod)               #TrialDay resolve no variance, TankID extremely little
plot(GULD_centrescore.mod)                  #No clustering issues
r2_nakagawa(GULD_centrescore.mod)           #random structure error


GULD_centretime50.sqrt.mod <- lmer(sqrt(centretime50) ~ 
                               Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime50.sqrt.mod)                 #No effects
summary(GULD_centretime50.sqrt.mod)               #TrialRound resolves no variance, TankID extremely little
plot(GULD_centretime50.sqrt.mod)                  #No clustering issues
r2_nakagawa(GULD_centretime50.sqrt.mod)           #random structure error


GULD_centretime75.sqrt.mod <- lmer(sqrt(centretime75) ~ 
                                     Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime75.sqrt.mod)                 #No effects
summary(GULD_centretime75.sqrt.mod)               #TrialRound resolves no variance, TrialDay and TankID extremely little
plot(GULD_centretime75.sqrt.mod)                  #No clustering issues
r2_nakagawa(GULD_centretime75.sqrt.mod)           #random structure error



GULD_centretime100.sqrt.mod <- lmer(sqrt(centretime100) ~ 
                                     Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime100.sqrt.mod)                 #No effects
summary(GULD_centretime100.sqrt.mod)               #TrialRound resolves no variance, TrialDay and TankID extremely little
plot(GULD_centretime100.sqrt.mod)                  #TrialRound, TrialDay, TankID resolve no variance
r2_nakagawa(GULD_centretime100.sqrt.mod)           #random structure error


save(GULD_avespeed_tot.mod, file = "./Output_Guldborgsund/GULD_avespeed_tot.mod.RData")
save(GULD_avespeed_mob.mod, file = "./Output_Guldborgsund/GULD_avespeed_mob.mod.RData")
save(GULD_aveacceler.mod, file = "./Output_Guldborgsund/GULD_aveaccler.mod.RData")
save(GULD_propmoving.invlog.mod, file = "./Output_Guldborgsund/GULD_propmoving.invlog.mod.RData")
save(GULD_dist.mod, file = "./Output_Guldborgsund/GULD_dist.mod.RData")
save(GULD_frozenevents.sqrt.mod, file = "./Output_Guldborgsund/GULD_frozenevents.sqrt.mod.RData")
save(GULD_timefrozen_tot.sqrt.mod, file = "./Output_Guldborgsund/GULD_timefrozen_tot.sqrt.mod.RData")
save(GULD_centrescore.mod, file = "./Output_Guldborgsund/GULD_centrescore.mod.RData")
save(GULD_centretime50.sqrt.mod, file = "./Output_Guldborgsund/GULD_centretime50.sqrt.mod.RData")
save(GULD_centretime75.sqrt.mod, file = "./Output_Guldborgsund/GULD_centretime75.sqrt.mod.RData")
save(GULD_centretime100.sqrt.mod, file = "./Output_Guldborgsund/GULD_centretime100.sqrt.mod.RData")



#Reduced Models- excluding random effects that collapse to zero (or extremely close to zero)
#  All ran as linear mixed effects models as distributions were/were transformed to approximately normal

#  Variable        Transformation    Random effects                 Fixed effects
#  avespeed_tot    nil               TrialDay, ArenaID, FishID      Sex + TL + ConditionFactor + InfectionScore
#  avespeed_mob    nil               TrialDay, TrialRound. FishID   Sex + TL + ConditionFactor + InfectionScore
#  aveaccler       nii               TrialDay, ArenaID, FishID      Sex + TL + ConditionFactor + InfectionScore
#  propmoving      log(1 - x)        TrialDay, ArenaID, FishID      Sex + TL + ConditionFactor + InfectionScore
#  dist            nil               TrialDay, ArenaID, FishID      Sex + TL + ConditionFactor + InfectionScore
#  frozenevents    sqrt              TrialDay, ArenaID, FishID      Sex + TL + ConditionFactor + InfectionScore
#  timefrozen_tot  sqrt              TrialDay, ArenaID, FishID      Sex + TL + ConditionFactor + InfectionScore
#  centrescore     nil               TrialRound. ArenaID, FishID    Sex + TL + ConditionFactor + InfectionScore
#  centretime50    sqrt              TrialDay, ArenaID, FishID      Sex + TL + ConditionFactor + InfectionScore
#  centretime75    sqrt              ArenaID, FishID                Sex + TL + ConditionFactor + InfectionScore
#  centretime100   sqrt              ArenaID, FishID                Sex + TL + ConditionFactor + InfectionScore


GULD_avespeed_tot.mod.red <- lmer(avespeed_tot ~ 
                                Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialDay) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_avespeed_tot.mod.red)                 #Sex, ConditionFactor.C effects
summary(GULD_avespeed_tot.mod.red)               #A lot of variance associated with Fish ID and TrialDay
plot(GULD_avespeed_tot.mod.red)                  #No clustering issues
r2_nakagawa(GULD_avespeed_tot.mod.red)           #Conditional R2: 0.775, Marginal R2: 0.179


GULD_avespeed_mob.mod.red <- lmer(avespeed_mob ~ 
                                Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialDay) + (1|TrialRound) + (1|FishID), data=GULDact.processed)
Anova(GULD_avespeed_mob.mod)                 #Sex effects
summary(GULD_avespeed_mob.mod)               #A lot of variance associated with Fish ID and TrialDay
plot(GULD_avespeed_mob.mod)                  #No clustering issues
r2_nakagawa(GULD_avespeed_mob.mod)           #Conditional R2: 0.666, Marginal R2: 0.104


GULD_aveacceler.mod.red <- lmer(aveacceler ~ 
                              Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialDay) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_aveacceler.mod.red)                 #Sex effects
summary(GULD_aveacceler.mod.red)               #A lot of variance associated with Fish ID and TrialDay, a bit to do with ArenaID
plot(GULD_aveacceler.mod.red)                  #No clustering issues
r2_nakagawa(GULD_aveacceler.mod.red)           #Conditional R2: 0.736, Marginal R2: 0.180


GULD_propmoving.invlog.mod.red <- lmer(log(1-propmoving) ~ 
                                     Sex + TL.C + ConditionFactor.C + InfectionScore.C  + (1|TrialDay) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_propmoving.invlog.mod.red)                 #Sex, ConditionFactor.C effects
summary(GULD_propmoving.invlog.mod.red)               #TankID and TrialRound resolves no variance
plot(GULD_propmoving.invlog.mod.red)                  #No clustering issues
r2_nakagawa(GULD_propmoving.invlog.mod.red)           #random structure error


GULD_dist.mod.red <- lmer(dist ~ 
                        Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialDay) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_dist.mod.red)                 #Sex, ConditionFactor.C effects
summary(GULD_dist.mod.red)               #TankID resolves no variance
plot(GULD_dist.mod.red)                  #No clustering issues
r2_nakagawa(GULD_dist.mod.red)           #random structure error


GULD_frozenevents.sqrt.mod.red <- lmer(sqrt(frozenevents) ~ 
                                     Sex + TL.C + ConditionFactor.C + InfectionScore.C  + (1|TrialDay) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_frozenevents.sqrt.mod.red)                 #ConditionFactor effect
summary(GULD_frozenevents.sqrt.mod.red)               #TrialRound resolves no variance, TankID extremely little
plot(GULD_frozenevents.sqrt.mod.red)                  #No clustering issues
r2_nakagawa(GULD_frozenevents.sqrt.mod.red)           #random structure error


GULD_timefrozen_tot.sqrt.mod.red <- lmer(sqrt(timefrozen_tot) ~ 
                                       Sex + TL.C + ConditionFactor.C + InfectionScore.C  + (1|TrialDay) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_timefrozen_tot.sqrt.mod.red)                 #Sex, ConditionFactor.C effects
summary(GULD_timefrozen_tot.sqrt.mod.red)               #TankID and TrialRound resolve no variance
plot(GULD_timefrozen_tot.sqrt.mod.red)                  #No clustering issues
r2_nakagawa(GULD_timefrozen_tot.sqrt.mod.red)           #random structure error


GULD_centrescore.mod.red <- lmer(centrescore ~ 
                               Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centrescore.mod.red)                 #TL effect
summary(GULD_centrescore.mod.red)               #TrialDay resolve no variance, TankID extremely little
plot(GULD_centrescore.mod.red)                  #No clustering issues
r2_nakagawa(GULD_centrescore.mod.red)           #random structure error


GULD_centretime50.sqrt.mod.red <- lmer(sqrt(centretime50) ~ 
                                     Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialDay) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime50.sqrt.mod.red)                 #No effects
summary(GULD_centretime50.sqrt.mod.red)               #TrialRound resolves no variance, TankID extremely little
plot(GULD_centretime50.sqrt.mod.red)                  #No clustering issues
r2_nakagawa(GULD_centretime50.sqrt.mod.red)           #random structure error


GULD_centretime75.sqrt.mod.red <- lmer(sqrt(centretime75) ~ 
                                     Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime75.sqrt.mod.red)                 #No effects
summary(GULD_centretime75.sqrt.mod.red)               #TrialRound resolves no variance, TrialDay and TankID extremely little
plot(GULD_centretime75.sqrt.mod.red)                  #No clustering issues
r2_nakagawa(GULD_centretime75.sqrt.mod.red)           #random structure error


GULD_centretime100.sqrt.mod.red <- lmer(sqrt(centretime100) ~ 
                                      Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime100.sqrt.mod.red)                 #No effects
summary(GULD_centretime100.sqrt.mod.red)               #TrialRound resolves no variance, TrialDay and TankID extremely little
plot(GULD_centretime100.sqrt.mod.red)                  #TrialRound, TrialDay, TankID resolve no variance
r2_nakagawa(GULD_centretime100.sqrt.mod.red)           #random structure error


save(GULD_avespeed_tot.mod.red, file = "./Output_Guldborgsund/GULD_avespeed_tot.mod.red.RData")
save(GULD_avespeed_mob.mod.red, file = "./Output_Guldborgsund/GULD_avespeed_mob.mod.red.RData")
save(GULD_aveacceler.mod.red, file = "./Output_Guldborgsund/GULD_aveaccler.mod.red.RData")
save(GULD_propmoving.invlog.mod.red, file = "./Output_Guldborgsund/GULD_propmoving.invlog.mod.red.RData")
save(GULD_dist.mod.red, file = "./Output_Guldborgsund/GULD_dist.mod.red.RData")
save(GULD_frozenevents.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_frozenevents.sqrt.mod.red.RData")
save(GULD_timefrozen_tot.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_timefrozen_tot.sqrt.mod.red.RData")
save(GULD_centrescore.mod.red, file = "./Output_Guldborgsund/GULD_centrescore.mod.red.RData")
save(GULD_centretime50.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_centretime50.sqrt.mod.red.RData")
save(GULD_centretime75.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_centretime75.sqrt.mod.red.RData")
save(GULD_centretime100.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_centretime100.sqrt.mod.red.RData")



## G.2.1. ACT repeatability analysis ----
#Calculating:             
# - Raw repeatability       
# - Adj repeatability (random effects included)

GULD_avespeed_tot.mod.rpt1 <- rpt(avespeed_tot ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
GULD_avespeed_tot.mod.rpt1
GULD_avespeed_tot.mod.rpt2 <- rpt(avespeed_tot ~ (1|TrialDay) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
GULD_avespeed_tot.mod.rpt2

GULD_avespeed_mob.mod.rpt1 <- rpt(avespeed_mob ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_avespeed_mob.mod.rpt1
GULD_avespeed_mob.mod.rpt2 <- rpt(avespeed_mob ~ (1|TrialDay) + (1|TrialRound) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_avespeed_mob.mod.rpt2

GULD_aveacceler.mod.rpt1 <- rpt(aveacceler ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_aveacceler.mod.rpt1
GULD_aveacceler.mod.rpt2 <- rpt(aveacceler ~ (1|TrialDay) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_aveacceler.mod.rpt2

GULD_propmoving.invlog.mod.rpt1 <- rpt(log(1-propmoving)  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
GULD_propmoving.invlog.mod.rpt1
GULD_propmoving.invlog.mod.rpt2 <- rpt(log(1-propmoving) ~ (1|TrialDay) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
GULD_propmoving.invlog.mod.rpt2

GULD_dist.mod.rpt1 <- rpt(dist  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_dist.mod.rpt1
GULD_dist.mod.rpt2 <- rpt(dist ~ (1|TrialDay) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_dist.mod.rpt2

GULD_frozenevents.sqrt.mod.rpt1 <- rpt(sqrt(frozenevents)  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                          nboot = 100, npermut = 0)
GULD_frozenevents.sqrt.mod.rpt1
GULD_frozenevents.sqrt.mod.rpt2 <- rpt(sqrt(frozenevents) ~ (1|TrialDay) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                          nboot = 100, npermut = 0)
GULD_frozenevents.sqrt.mod.rpt2

GULD_timefrozen_tot.sqrt.mod.rpt1 <- rpt(sqrt(timefrozen_tot)  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_timefrozen_tot.sqrt.mod.rpt1
GULD_timefrozen_tot.sqrt.mod.rpt2 <- rpt(sqrt(timefrozen_tot) ~ (1|TrialDay) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_timefrozen_tot.sqrt.mod.rpt2

GULD_centrescore.mod.rpt1 <- rpt(centrescore  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                         nboot = 100, npermut = 0)
GULD_centrescore.mod.rpt1
GULD_centrescore.mod.rpt2 <- rpt(centrescore ~ (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                         nboot = 100, npermut = 0)
GULD_centrescore.mod.rpt2

GULD_centretime50.sqrt.mod.rpt1 <- rpt(sqrt(centretime50)  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_centretime50.sqrt.mod.rpt1
GULD_centretime50.sqrt.mod.rpt2 <- rpt(sqrt(centretime50) ~ (1|TrialDay) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_centretime50.sqrt.mod.rpt2

GULD_centretime75.sqrt.mod.rpt1 <- rpt(sqrt(centretime75)  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_centretime75.sqrt.mod.rpt1
GULD_centretime75.sqrt.mod.rpt2 <- rpt(sqrt(centretime75) ~ (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_centretime75.sqrt.mod.rpt2

GULD_centretime100.sqrt.mod.rpt1 <- rpt(sqrt(centretime100)  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_centretime100.sqrt.mod.rpt1
GULD_centretime100.sqrt.mod.rpt2 <- rpt(sqrt(centretime100) ~ (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                       nboot = 100, npermut = 0)
GULD_centretime100.sqrt.mod.rpt2


save(GULD_avespeed_tot.mod.rpt1, file = "./Output_Guldborgsund/GULD_avespeed_tot.mod.rpt1.RData")
save(GULD_avespeed_mob.mod.rpt1, file = "./Output_Guldborgsund/GULD_avespeed_mob.mod.rpt1.RData")
save(GULD_aveacceler.mod.rpt1, file = "./Output_Guldborgsund/GULD_aveaccler.mod.rpt1.RData")
save(GULD_propmoving.invlog.mod.rpt1, file = "./Output_Guldborgsund/GULD_propmoving.invlog.mod.rpt1.RData")
save(GULD_dist.mod.rpt1, file = "./Output_Guldborgsund/GULD_dist.mod.rpt1.RData")
save(GULD_frozenevents.sqrt.mod.rpt1, file = "./Output_Guldborgsund/GULD_frozenevents.sqrt.mod.rpt1.RData")
save(GULD_timefrozen_tot.sqrt.mod.rpt1, file = "./Output_Guldborgsund/GULD_timefrozen_tot.sqrt.mod.rpt1.RData")
save(GULD_centrescore.mod.rpt1, file = "./Output_Guldborgsund/GULD_centrescore.mod.rpt1.RData")
save(GULD_centretime50.sqrt.mod.rpt1, file = "./Output_Guldborgsund/GULD_centretime50.sqrt.mod.rpt1.RData")
save(GULD_centretime75.sqrt.mod.rpt1, file = "./Output_Guldborgsund/GULD_centretime75.sqrt.mod.rpt1.RData")
save(GULD_centretime100.sqrt.mod.rpt1, file = "./Output_Guldborgsund/GULD_centretime100.sqrt.mod.rpt1.RData")


save(GULD_avespeed_tot.mod.rpt2, file = "./Output_Guldborgsund/GULD_avespeed_tot.mod.rpt2.RData")
save(GULD_avespeed_mob.mod.rpt2, file = "./Output_Guldborgsund/GULD_avespeed_mob.mod.rpt2.RData")
save(GULD_aveacceler.mod.rpt2, file = "./Output_Guldborgsund/GULD_aveaccler.mod.rpt2.RData")
save(GULD_propmoving.invlog.mod.rpt2, file = "./Output_Guldborgsund/GULD_propmoving.invlog.mod.rpt2.RData")
save(GULD_dist.mod.rpt2, file = "./Output_Guldborgsund/GULD_dist.mod.rpt2.RData")
save(GULD_frozenevents.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_frozenevents.sqrt.mod.rpt2.RData")
save(GULD_timefrozen_tot.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_timefrozen_tot.sqrt.mod.rpt2.RData")
save(GULD_centrescore.mod.rpt2, file = "./Output_Guldborgsund/GULD_centrescore.mod.rpt2.RData")
save(GULD_centretime50.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_centretime50.sqrt.mod.rpt2.RData")
save(GULD_centretime75.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_centretime75.sqrt.mod.rpt2.RData")
save(GULD_centretime100.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_centretime100.sqrt.mod.rpt2.RData")








## G.2.3. EXLP distributions and variance analysis ----
GULDexpl.processed <- read.csv("~/trophicpersonalities_A/Data_Guldborgsund/GULD_EXPLdat_processed.csv")



#endpointspeed: (s) latency to explore to the endpoint from time of emergence
ggplot(GULD_EXPL.excl) + aes(x = endpointspeed) + geom_histogram(color="black", fill="lightblue", binwidth = 100) + simpletheme 
ggqqplot(GULD_EXPL.excl$endpointspeed) 
ggplot(GULD_EXPL.excl) + aes(x = log(endpointspeed)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULD_EXPL.excl$endpointspeed)) #still some right skewed but better

#refugereturnlat: (s) latency to return to refuge after first emergence
ggplot(GULD_EXPL.excl) + aes(x = refugereturnlat) + geom_histogram(color="black", fill="lightblue", binwidth = 40) + simpletheme 
ggqqplot(GULD_EXPL.excl$refugereturnlat) 
ggplot(GULD_EXPL.excl) + aes(x = log(refugereturnlat)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULD_EXPL.excl$refugereturnlat)) #near normal


#Summary-
#emergelat:         _bimodal
#endpointlat:       _bimodal
#endpointspeed      _heavily right skewed, log transformation is better, 46 NAs
#refugereturnlat    _heavily right skewed, log transformation is approximately normal, 30 NAs 


#Variables to be used for repeatability analysis-
#emergelat.bin          endpointlat.bin       
#endpointspeed          endpointspeed.ln
#refugereturnlat        refugereturnlat.ln


#Transformations-
GULD_EXPL.excl$endpointspeed.ln <- log(GULD_EXPL.excl$endpointspeed)
GULD_EXPL.excl$refugereturnlat.ln <- log(GULD_EXPL.excl$refugereturnlat)


#Variance analysis-
GULD_emergelat.bin.mod <- glmer(emergelat.bin ~ (1|FishID), family = binomial, data=GULD_EXPL.excl)
summary(GULD_emergelat.bin.mod)
GULD_emergelat.bin.rpt <- rpt(emergelat.bin ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Binary", 
                                 nboot = 100, npermut = 0)
GULD_emergelat.bin.rpt


GULD_endpointlat.bin.mod <- glmer(endpointlat.bin~ (1|FishID), family = binomial, data=GULD_EXPL.excl)
summary(GULD_endpointlat.bin.mod)
GULD_endpointlat.bin.rpt <- rpt(endpointlat.bin ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Binary", 
                                nboot = 100, npermut = 0)
GULD_endpointlat.bin.rpt


GULD_endpointspeed.mod <- lmer(endpointspeed ~ (1|FishID), data=GULD_EXPL.excl)
summary(GULD_endpointspeed.mod)
plot(GULD_endpointspeed.mod) #significant assymmetry
GULD_endpointspeed.rpt <- rpt(endpointspeed ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_endpointspeed.rpt
GULD_endpointspeed.ln.mod <- lmer(endpointspeed.ln ~ (1|FishID), data=GULD_EXPL.excl)
summary(GULD_endpointspeed.ln.mod)
plot(GULD_endpointspeed.ln.mod) #good
GULD_endpointspeed.ln.rpt <- rpt(endpointspeed.ln ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Gaussian", 
                                   nboot = 100, npermut = 0)
GULD_endpointspeed.ln.rpt


GULD_refugereturnlat.mod <- lmer(refugereturnlat ~ (1|FishID), data=GULD_EXPL.excl)
summary(GULD_refugereturnlat.mod)
plot(GULD_refugereturnlat.mod) #significant assymmetry
GULD_refugereturnlat.rpt <- rpt(refugereturnlat ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Gaussian", 
                                   nboot = 100, npermut = 0)
GULD_refugereturnlat.rpt
GULD_refugereturnlat.ln.mod <- lmer(refugereturnlat.ln ~ (1|FishID), data=GULD_EXPL.excl)
summary(GULD_refugereturnlat.ln.mod)
plot(GULD_refugereturnlat.ln.mod) #good
GULD_refugereturnlat.ln.rpt <- rpt(refugereturnlat.ln ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_refugereturnlat.ln.rpt


#Summary-
#emergelat.bin        _ 0.678 [0.355, 0.838] ***
#endpointlat.bin      _ 0.626 [0.248, 0.804] *** 
#endpointspeed        _ 0.098 [0, 0.34]      ns 
#endpointspeed.ln     _ 0.222 [0, 0.489]     . 
#refugereturnlat.ln   _ 0.243 [0, 0.45]      ns 
#refugereturnlat.ln   _ 0.158 [0, 0.411]     ns   





## Bullding data frame for SIA correlation analysis


beta <- coef(GULD_avespeed_tot.mod.red)
colnames(beta) <- c("Intercept", "Slope")
beta

write.csv(GULD_EXPL.excl, '~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis/GULD_EXPL.processing.csv')
write.csv(GULD_ACT.excl, '~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis/GULD_ACT.processing.csv')


