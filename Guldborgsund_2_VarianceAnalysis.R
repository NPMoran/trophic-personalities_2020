# Title: Behavioural and trophic variation within a well-established invasive round goby population 
# Author: Nicholas Moran
# Date: March 2023



#### 2. Behavioural analysis ####
Sys.setenv(LANG = "en")
#Loading required packages- 
library(dplyr); library(ggplot2); library(ggpubr)
library(lme4); library(lmerTest); library(car); library(rptR); library(performance)
#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, size = 1))



## 2.1. Activity, distributions and variance analysis ----
GULDact.processed <- read.csv("~/trophicpersonalities_A/Data_Guldborgsund/GULD_ACTdat_processed.csv")
nrow(GULDact.processed) #118 rows
n_distinct(GULDact.processed$FishID) #43 fish included in analysis

#Z-transformation/scaling of continuous fixed effects
GULDact.processed$TL.C <- scale(GULDact.processed$TL)  
GULDact.processed$ConditionFactor.C <- scale(GULDact.processed$ConditionFactor)  
GULDact.processed$InfectionScore.C <- scale(GULDact.processed$InfectionScore)  


# in: avespeed_tot
#     avespeed_mob
#     aveacceler
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
#  aveacceler       nii               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
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
save(GULD_aveacceler.mod, file = "./Output_Guldborgsund/GULD_aveacceler.mod.RData")
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
#  aveacceler       nii               TrialDay, ArenaID, FishID      Sex + TL + ConditionFactor + InfectionScore
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
save(GULD_aveacceler.mod.red, file = "./Output_Guldborgsund/GULD_aveacceler.mod.red.RData")
save(GULD_propmoving.invlog.mod.red, file = "./Output_Guldborgsund/GULD_propmoving.invlog.mod.red.RData")
save(GULD_dist.mod.red, file = "./Output_Guldborgsund/GULD_dist.mod.red.RData")
save(GULD_frozenevents.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_frozenevents.sqrt.mod.red.RData")
save(GULD_timefrozen_tot.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_timefrozen_tot.sqrt.mod.red.RData")
save(GULD_centrescore.mod.red, file = "./Output_Guldborgsund/GULD_centrescore.mod.red.RData")
save(GULD_centretime50.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_centretime50.sqrt.mod.red.RData")
save(GULD_centretime75.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_centretime75.sqrt.mod.red.RData")
save(GULD_centretime100.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_centretime100.sqrt.mod.red.RData")



## G.2.2. ACT repeatability analysis ----
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
save(GULD_aveacceler.mod.rpt1, file = "./Output_Guldborgsund/GULD_aveacceler.mod.rpt1.RData")
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
save(GULD_aveacceler.mod.rpt2, file = "./Output_Guldborgsund/GULD_aveacceler.mod.rpt2.RData")
save(GULD_propmoving.invlog.mod.rpt2, file = "./Output_Guldborgsund/GULD_propmoving.invlog.mod.rpt2.RData")
save(GULD_dist.mod.rpt2, file = "./Output_Guldborgsund/GULD_dist.mod.rpt2.RData")
save(GULD_frozenevents.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_frozenevents.sqrt.mod.rpt2.RData")
save(GULD_timefrozen_tot.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_timefrozen_tot.sqrt.mod.rpt2.RData")
save(GULD_centrescore.mod.rpt2, file = "./Output_Guldborgsund/GULD_centrescore.mod.rpt2.RData")
save(GULD_centretime50.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_centretime50.sqrt.mod.rpt2.RData")
save(GULD_centretime75.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_centretime75.sqrt.mod.rpt2.RData")
save(GULD_centretime100.sqrt.mod.rpt2, file = "./Output_Guldborgsund/GULD_centretime100.sqrt.mod.rpt2.RData")



## G.2.3. EXPL distributions and variance analysis ----
GULDexpl.processed <- read.csv("~/trophicpersonalities_A/Data_Guldborgsund/GULD_EXPLdat_processed.csv")
nrow(GULDexpl.processed) #113 rows
n_distinct(GULDexpl.processed$FishID) #43 fish included in analysis

#Z-transformation/scaling of continuous fixed effects
GULDexpl.processed$TL.C <- scale(GULDexpl.processed$TL)  
GULDexpl.processed$ConditionFactor.C <- scale(GULDexpl.processed$ConditionFactor)  
GULDexpl.processed$InfectionScore.C <- scale(GULDexpl.processed$InfectionScore)  


# in: emergelat.bin
#     endpointlat.bin     
#     endpointspeed     
#     refugereturnlat


#Assessing distributions (excluding binomial variables)-
#endpointspeed: (s) latency to explore to the endpoint from time of emergence (note: 46 NAs, where fish did not reach the end)
ggplot(GULDexpl.processed) + aes(x = endpointspeed) + geom_histogram(color="black", fill="lightblue", binwidth = 100) + simpletheme 
ggqqplot(GULDexpl.processed$endpointspeed) 
ggplot(GULDexpl.processed) + aes(x = log(endpointspeed)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULDexpl.processed$endpointspeed)) #still some right skew but much better

#refugereturnlat: (s) latency to return to refuge after first emergence (note: 30 NAs, where fish did not return, or emerge at all)
ggplot(GULDexpl.processed) + aes(x = refugereturnlat) + geom_histogram(color="black", fill="lightblue", binwidth = 40) + simpletheme 
ggqqplot(GULDexpl.processed$refugereturnlat) 
ggplot(GULDexpl.processed) + aes(x = log(refugereturnlat)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULDexpl.processed$refugereturnlat)) #near normal



#Full Models- including all random and fixed effects

#  Variable         Transformation    Random effects                                 Fixed effects
#  emergelat.bin    NA (binomial)     TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  endpointlat.bin  NA (binomial)     TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  endpointspeed    log               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  refugereturnlat  log               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore


GULD_emergelat.bin.mod <- glmer(emergelat.bin ~ 
                                  Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), family = binomial, data=GULDexpl.processed)
Anova(GULD_emergelat.bin.mod)                #No effects
summary(GULD_emergelat.bin.mod)              #TrialRound, ArenaID and TrialDay resolve extremely low variance
plot(GULD_emergelat.bin.mod)                 #No issues
r2_nakagawa(GULD_emergelat.bin.mod)          #Conditional R2: 0.759, Marginal R2: 0.111
#convergence failure

GULD_endpointlat.bin.mod <- glmer(endpointlat.bin~ 
                                    Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), family = binomial, data=GULDexpl.processed)
Anova(GULD_endpointlat.bin.mod)               #No effects
summary(GULD_endpointlat.bin.mod)             #TankID, ArenaID and TrialDay resolve extremely low variance
plot(GULD_endpointlat.bin.mod)                #No issues
r2_nakagawa(GULD_endpointlat.bin.mod)         #Conditional R2: 0.784, Marginal R2: 0.102


GULD_endpointspeed.ln.mod <- lmer(log(endpointspeed) ~ 
                                    Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDexpl.processed)
Anova(GULD_endpointspeed.ln.mod)               #No effects
summary(GULD_endpointspeed.ln.mod)             #TankID resolved zero variance, TrialDay resolves extremely low variance
plot(GULD_endpointspeed.ln.mod)                #No issues
r2_nakagawa(GULD_endpointspeed.ln.mod)         #fails


GULD_refugereturnlat.ln.mod <- lmer(log(refugereturnlat) ~ 
                                    Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDexpl.processed)
Anova(GULD_refugereturnlat.ln.mod)               #No effects
summary(GULD_refugereturnlat.ln.mod)             #ArenaID resolves extremely low variance
plot(GULD_refugereturnlat.ln.mod)                #No issues
r2_nakagawa(GULD_refugereturnlat.ln.mod)         #Conditional R2: 0.491, Marginal R2: 0.040


save(GULD_emergelat.bin.mod, file = "./Output_Guldborgsund/GULD_emergelat.bin.mod.RData")
save(GULD_endpointlat.bin.mod, file = "./Output_Guldborgsund/GULD_endpointlat.bin.mod.RData")
save(GULD_endpointspeed.ln.mod, file = "./Output_Guldborgsund/GULD_endpointspeed.ln.mod.RData")
save(GULD_refugereturnlat.ln.mod, file = "./Output_Guldborgsund/GULD_refugereturnlat.ln.mod.RData")



#Reduced Models- excluding random effects that collapse to zero (or extremely close to zero)

#  Variable         Transformation    Random effects                                 Fixed effects
#  emergelat.bin    NA (binomial)     TankID, FishID                                 Sex + TL + ConditionFactor + InfectionScore
#  endpointlat.bin  NA (binomial)     TrialRound, FishID                             Sex + TL + ConditionFactor + InfectionScore
#  endpointspeed    log               TrialRound. ArenaID, FishID                    Sex + TL + ConditionFactor + InfectionScore
#  refugereturnlat  log               TankID, TrialDay, TrialRound, FishID           Sex + TL + ConditionFactor + InfectionScore



GULD_emergelat.bin.mod.red <- glmer(emergelat.bin ~ 
                                  Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|FishID), family = binomial, data=GULDexpl.processed)
Anova(GULD_emergelat.bin.mod.red)                #No effects
summary(GULD_emergelat.bin.mod.red)              #Most variance associated with FishID
plot(GULD_emergelat.bin.mod.red)                 #No issues
r2_nakagawa(GULD_emergelat.bin.mod.red)          #Conditional R2: 0.759, Marginal R2: 0.111
#convergence failure

GULD_endpointlat.bin.mod.red <- glmer(endpointlat.bin~ 
                                    Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialRound) + (1|FishID), family = binomial, data=GULDexpl.processed)
Anova(GULD_endpointlat.bin.mod.red)               #No effects
summary(GULD_endpointlat.bin.mod.red)             #Most variance associated with FishID
plot(GULD_endpointlat.bin.mod.red)                #No issues
r2_nakagawa(GULD_endpointlat.bin.mod.red)         #Conditional R2: 0.784, Marginal R2: 0.102


GULD_endpointspeed.ln.mod.red <- lmer(log(endpointspeed) ~ 
                                    Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDexpl.processed)
Anova(GULD_endpointspeed.ln.mod.red)               #No effects
summary(GULD_endpointspeed.ln.mod.red)             #FishID is largest component (other than residual)
plot(GULD_endpointspeed.ln.mod.red)                #No issues
r2_nakagawa(GULD_endpointspeed.ln.mod.red)         #Conditional R2: 0.391, Marginal R2: 0.034


GULD_refugereturnlat.ln.mod.red <- lmer(log(refugereturnlat) ~ 
                                      Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|FishID), data=GULDexpl.processed)
Anova(GULD_refugereturnlat.ln.mod.red)               #No effects
summary(GULD_refugereturnlat.ln.mod.red)             #FishID is largest component (other than residual)
plot(GULD_refugereturnlat.ln.mod.red)                #No issues
r2_nakagawa(GULD_refugereturnlat.ln.mod.red)         #Conditional R2: 0.491, Marginal R2: 0.040


save(GULD_emergelat.bin.mod.red, file = "./Output_Guldborgsund/GULD_emergelat.bin.mod.red.RData")
save(GULD_endpointlat.bin.mod.red, file = "./Output_Guldborgsund/GULD_endpointlat.bin.mod.red.RData")
save(GULD_endpointspeed.ln.mod.red, file = "./Output_Guldborgsund/GULD_endpointspeed.ln.mod.red.RData")
save(GULD_refugereturnlat.ln.mod.red, file = "./Output_Guldborgsund/GULD_refugereturnlat.ln.mod.red.RData")



## G.2.4. EXPL repeatability analysis ----
#Calculating:             
# - Raw repeatability       
# - Adj repeatability (random effects included)


GULD_emergelat.bin.rpt1 <- rpt(emergelat.bin ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                                 nboot = 100, npermut = 0)
GULD_emergelat.bin.rpt1
GULD_emergelat.bin.rpt2 <- rpt(emergelat.bin ~ (1|TankID) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                               nboot = 100, npermut = 0)
GULD_emergelat.bin.rpt2

GULD_endpointlat.bin.rpt1 <- rpt(endpointlat.bin ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                               nboot = 100, npermut = 0)
GULD_endpointlat.bin.rpt1
GULD_endpointlat.bin.rpt2 <- rpt(endpointlat.bin ~ (1|TrialRound) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                               nboot = 100, npermut = 0)
GULD_endpointlat.bin.rpt2

GULD_endpointspeed.ln.rpt1 <- rpt(log(endpointspeed) ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_endpointspeed.ln.rpt1 #overlaps zero
GULD_endpointspeed.ln.rpt2 <- rpt(log(endpointspeed) ~ (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_endpointspeed.ln.rpt2

GULD_refugereturnlat.ln.rpt1 <- rpt(log(refugereturnlat) ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_refugereturnlat.ln.rpt1 #overlaps zero
GULD_refugereturnlat.ln.rpt2 <- rpt(log(refugereturnlat) ~ (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|FishID), grname = c("FishID"), data = GULDexpl.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_refugereturnlat.ln.rpt2 


save(GULD_emergelat.bin.rpt1, file = "./Output_Guldborgsund/GULD_emergelat.bin.rpt1.RData")
save(GULD_endpointlat.bin.rpt1, file = "./Output_Guldborgsund/GULD_endpointlat.bin.rpt1.RData")
save(GULD_endpointspeed.ln.rpt1, file = "./Output_Guldborgsund/GULD_endpointspeed.ln.rpt1.RData")
save(GULD_refugereturnlat.ln.rpt1, file = "./Output_Guldborgsund/GULD_refugereturnlat.ln.rpt1.RData")

save(GULD_emergelat.bin.rpt2, file = "./Output_Guldborgsund/GULD_emergelat.bin.rpt2.RData")
save(GULD_endpointlat.bin.rpt2, file = "./Output_Guldborgsund/GULD_endpointlat.bin.rpt2.RData")
save(GULD_endpointspeed.ln.rpt2, file = "./Output_Guldborgsund/GULD_endpointspeed.ln.rpt2.RData")
save(GULD_refugereturnlat.ln.rpt2, file = "./Output_Guldborgsund/GULD_refugereturnlat.ln.rpt2.RData")


## G.2.5. Building data frame for SIA correlation analysis ----
# - Behavioural predictor variations for SIA analysis
#   1- Initial behavioural response (trial 1 response)
#   2- Average behavioural response (trial 1-3 average, limited to individuals completing all 3 trails, due to treatment effects)
#   3- Model coefficients estimates for each individual from reduced models (conditional mode for each individuals, reflects an adjusted average behaviour) 

#Extracting the first trial and average behaviour accross trials
# - First Trial used as a single phenotypic measure most closely reflect their behaviour in the field
# - As there is a TrialDay effect in most variables, only individuals with scores for all three trials are used for averages. 
#Activity
behavACTT1 <- NULL
behavACTT1$FishID <- subset(GULDact.processed, TrialDay == 'trial 1')$FishID
behavACTT1$avespeed_tot.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$avespeed_tot
behavACTT1$avespeed_mob.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$avespeed_mob
behavACTT1$aveacceler.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$aveacceler
behavACTT1$propmoving.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$propmoving
behavACTT1$dist.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$dist
behavACTT1$frozenevents.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$frozenevents
behavACTT1$timefrozen_tot.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$timefrozen_tot
behavACTT1$centrescore.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$centrescore
behavACTT1$centretime50.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$centretime50
behavACTT1$centretime75.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$centretime75
behavACTT1$centretime100.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$centretime100
behavACTT1 <- as.data.frame(behavACTT1)

behavACTT2 <- NULL
behavACTT2$FishID <- subset(GULDact.processed, TrialDay == 'trial 2')$FishID
behavACTT2$avespeed_tot.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$avespeed_tot
behavACTT2$avespeed_mob.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$avespeed_mob
behavACTT2$aveacceler.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$aveacceler
behavACTT2$propmoving.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$propmoving
behavACTT2$dist.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$dist
behavACTT2$frozenevents.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$frozenevents
behavACTT2$timefrozen_tot.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$timefrozen_tot
behavACTT2$centrescore.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$centrescore
behavACTT2$centretime50.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$centretime50
behavACTT2$centretime75.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$centretime75
behavACTT2$centretime100.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$centretime100
behavACTT2 <- as.data.frame(behavACTT2)

behavACTT3 <- NULL
behavACTT3$FishID <- subset(GULDact.processed, TrialDay == 'trial 3')$FishID
behavACTT3$avespeed_tot.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$avespeed_tot
behavACTT3$avespeed_mob.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$avespeed_mob
behavACTT3$aveacceler.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$aveacceler
behavACTT3$propmoving.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$propmoving
behavACTT3$dist.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$dist
behavACTT3$frozenevents.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$frozenevents
behavACTT3$timefrozen_tot.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$timefrozen_tot
behavACTT3$centrescore.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$centrescore
behavACTT3$centretime50.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$centretime50
behavACTT3$centretime75.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$centretime75
behavACTT3$centretime100.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$centretime100
behavACTT3 <- as.data.frame(behavACTT3)
nrow(behavACTT3)

behavMEANWorking <- merge(behavACTT1, behavACTT2, by = 'FishID', all.x = FALSE)
behavMEANWorking <- merge(behavMEANWorking, behavACTT3, by = 'FishID', all.x = FALSE)

behavMEAN <- NULL
behavMEAN$FishID <- behavMEANWorking$FishID
behavMEAN$avespeed_tot.MEAN <- (behavMEANWorking$avespeed_tot.ACTT1+behavMEANWorking$avespeed_tot.ACTT2+behavMEANWorking$avespeed_tot.ACTT3)/3
behavMEAN$avespeed_mob.MEAN <- (behavMEANWorking$avespeed_mob.ACTT1+behavMEANWorking$avespeed_mob.ACTT2+behavMEANWorking$avespeed_mob.ACTT3)/3
behavMEAN$aveacceler.MEAN <- (behavMEANWorking$aveacceler.ACTT1+behavMEANWorking$aveacceler.ACTT2+behavMEANWorking$aveacceler.ACTT3)/3
behavMEAN$propmoving.MEAN <- (behavMEANWorking$propmoving.ACTT1+behavMEANWorking$propmoving.ACTT2+behavMEANWorking$propmoving.ACTT3)/3
behavMEAN$dist.MEAN <- (behavMEANWorking$dist.ACTT1+behavMEANWorking$dist.ACTT2+behavMEANWorking$dist.ACTT3)/3
behavMEAN$frozenevents.MEAN <- (behavMEANWorking$frozenevents.ACTT1+behavMEANWorking$frozenevents.ACTT2+behavMEANWorking$frozenevents.ACTT3)/3
behavMEAN$timefrozen_tot.MEAN <- (behavMEANWorking$timefrozen_tot.ACTT1+behavMEANWorking$timefrozen_tot.ACTT2+behavMEANWorking$timefrozen_tot.ACTT3)/3
behavMEAN$centrescore.MEAN <- (behavMEANWorking$centrescore.ACTT1+behavMEANWorking$centrescore.ACTT2+behavMEANWorking$centrescore.ACTT3)/3
behavMEAN$centretime50.MEAN <- (behavMEANWorking$centretime50.ACTT1+behavMEANWorking$centretime50.ACTT2+behavMEANWorking$centretime50.ACTT3)/3
behavMEAN$centretime75.MEAN <- (behavMEANWorking$centretime75.ACTT1+behavMEANWorking$centretime75.ACTT2+behavMEANWorking$centretime75.ACTT3)/3
behavMEAN$centretime100.MEAN <- (behavMEANWorking$centretime100.ACTT1+behavMEANWorking$centretime100.ACTT2+behavMEANWorking$centretime100.ACTT3)/3
behavMEAN <- as.data.frame(behavMEAN)

#Exploratory data
behavEXPLT1 <- NULL
behavEXPLT1$FishID <- subset(GULDexpl.processed, TrialDay == 'trial 1')$FishID
behavEXPLT1$emergelat.bin.EXPLT1 <- subset(GULDexpl.processed, TrialDay == 'trial 1')$emergelat.bin
behavEXPLT1$endpointlat.bin.EXPLT1 <- subset(GULDexpl.processed, TrialDay == 'trial 1')$endpointlat.bin
behavEXPLT1$endpointspeed.EXPLT1 <- subset(GULDexpl.processed, TrialDay == 'trial 1')$endpointspeed
behavEXPLT1$refugereturnlat.EXPLT1 <- subset(GULDexpl.processed, TrialDay == 'trial 1')$refugereturnlat
behavEXPLT1 <- as.data.frame(behavEXPLT1)

behavEXPLT2 <- NULL
behavEXPLT2$FishID <- subset(GULDexpl.processed, TrialDay == 'trial 2')$FishID
behavEXPLT2$emergelat.bin.EXPLT2 <- subset(GULDexpl.processed, TrialDay == 'trial 2')$emergelat.bin
behavEXPLT2$endpointlat.bin.EXPLT2 <- subset(GULDexpl.processed, TrialDay == 'trial 2')$endpointlat.bin
behavEXPLT2$endpointspeed.EXPLT2 <- subset(GULDexpl.processed, TrialDay == 'trial 2')$endpointspeed
behavEXPLT2$refugereturnlat.EXPLT2 <- subset(GULDexpl.processed, TrialDay == 'trial 2')$refugereturnlat
behavEXPLT2 <- as.data.frame(behavEXPLT2)

behavEXPLT3 <- NULL
behavEXPLT3$FishID <- subset(GULDexpl.processed, TrialDay == 'trial 3')$FishID
behavEXPLT3$emergelat.bin.EXPLT3 <- subset(GULDexpl.processed, TrialDay == 'trial 3')$emergelat.bin
behavEXPLT3$endpointlat.bin.EXPLT3 <- subset(GULDexpl.processed, TrialDay == 'trial 3')$endpointlat.bin
behavEXPLT3$endpointspeed.EXPLT3 <- subset(GULDexpl.processed, TrialDay == 'trial 3')$endpointspeed
behavEXPLT3$refugereturnlat.EXPLT3 <- subset(GULDexpl.processed, TrialDay == 'trial 3')$refugereturnlat
behavEXPLT3 <- as.data.frame(behavEXPLT3)

behavMEANWorking <- merge(behavEXPLT1, behavEXPLT2, by = 'FishID', all.x = FALSE)
behavMEANWorking <- merge(behavMEANWorking, behavEXPLT3, by = 'FishID', all.x = FALSE)

behavMEAN2 <- NULL
behavMEAN2$FishID <- behavMEANWorking$FishID
behavMEAN2$emergelat.bin.MEAN <- (behavMEANWorking$emergelat.bin.EXPLT1+behavMEANWorking$emergelat.bin.EXPLT2+behavMEANWorking$emergelat.bin.EXPLT3)/3
behavMEAN2$endpointlat.bin.MEAN <- (behavMEANWorking$endpointlat.bin.EXPLT1+behavMEANWorking$endpointlat.bin.EXPLT2+behavMEANWorking$endpointlat.bin.EXPLT3)/3
behavMEAN2$endpointspeed.MEAN <- (behavMEANWorking$endpointspeed.EXPLT1+behavMEANWorking$endpointspeed.EXPLT2+behavMEANWorking$endpointspeed.EXPLT3)/3
behavMEAN2$refugereturnlat.MEAN <- (behavMEANWorking$refugereturnlat.EXPLT1+behavMEANWorking$refugereturnlat.EXPLT2+behavMEANWorking$refugereturnlat.EXPLT3)/3
behavMEAN2 <- as.data.frame(behavMEAN2)

#Combining Expl and Act data
behavMEAN <- merge(behavMEAN, behavMEAN2, by = 'FishID', all.x = TRUE)
behavTRIAL1 <- merge(behavACTT1, behavEXPLT1, by = 'FishID', all.x = TRUE)


#Extracting the coefficient for each individual for each behavioural variable. 
# - equivalent to a conditional mode for each level of the random effect (i.e. each individual fish)
#   i.e. the predicted most likely value for each individual FishID, accounting for other Random effects
# - equivalent to their BLUP + the overall model intercept

# Activity variables
behvar3a <- coef(GULD_avespeed_tot.mod.red)$FishID
behvar3b <- coef(GULD_avespeed_mob.mod.red)$FishID
behvar3c <- coef(GULD_aveacceler.mod.red)$FishID
behvar3d <- coef(GULD_propmoving.invlog.mod.red)$FishID
behvar3e <- coef(GULD_dist.mod.red)$FishID
behvar3f <- coef(GULD_frozenevents.sqrt.mod.red)$FishID
behvar3g <- coef(GULD_timefrozen_tot.sqrt.mod.red)$FishID
behvar3h <- coef(GULD_centrescore.mod.red)$FishID
behvar3i <- coef(GULD_centretime50.sqrt.mod.red)$FishID
behvar3j <- coef(GULD_centretime75.sqrt.mod.red)$FishID
behvar3k <- coef(GULD_centretime100.sqrt.mod.red)$FishID

colnames(behvar3a)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3b)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3c)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3d)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3e)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3f)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3g)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3h)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3i)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3j)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3k)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")

behavCOEFs <- NULL
behavCOEFs$FishID <- rownames(behvar3a)
behavCOEFs$avespeed_tot.COEF <- behvar3a$Intercept
behavCOEFs$avespeed_mob.COEF <- behvar3b$Intercept
behavCOEFs$aveacceler.COEF <- behvar3c$Intercept
behavCOEFs$propmoving.invlog.COEF <- behvar3d$Intercept
behavCOEFs$dist.COEF <- behvar3e$Intercept
behavCOEFs$frozenevents.sqrt.COEF <- behvar3f$Intercept
behavCOEFs$timefrozen_tot.sqrt.COEF <- behvar3g$Intercept
behavCOEFs$centrescore.COEF <- behvar3h$Intercept
behavCOEFs$centretime50.sqrt.COEF <- behvar3i$Intercept
behavCOEFs$centretime75.sqrt.COEF <- behvar3j$Intercept
behavCOEFs$centretime100.sqrt.COEF <- behvar3k$Intercept
behavCOEFs <- as.data.frame(behavCOEFs)

#Exploration variables:
behvar3l <- coef(GULD_emergelat.bin.mod.red)$FishID
behvar3m <- coef(GULD_endpointlat.bin.mod.red)$FishID
behvar3n <- coef(GULD_endpointspeed.ln.mod.red)$FishID
behvar3o <- coef(GULD_refugereturnlat.ln.mod.red)$FishID

colnames(behvar3l)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3m)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3n)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3o)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")

behvar3l$FishID <- rownames(behvar3l)
behvar3m$FishID <- rownames(behvar3m)
behvar3n$FishID <- rownames(behvar3n)
behvar3o$FishID <- rownames(behvar3o)

behvar3l$emergelat.bin.COEF <- behvar3l$Intercept
behvar3m$endpointlat.bin.COEF <- behvar3m$Intercept
behvar3n$endpointspeed.ln.COEF <- behvar3n$Intercept
behvar3o$refugereturnlat.ln.COEF <- behvar3o$Intercept

behvar3l <- select(behvar3l, -c(Intercept, Sexm, TL.C, ConditionFactor.C, InfectionScore.C))
behvar3m <- select(behvar3m, -c(Intercept, Sexm, TL.C, ConditionFactor.C, InfectionScore.C))
behvar3n <- select(behvar3n, -c(Intercept, Sexm, TL.C, ConditionFactor.C, InfectionScore.C))
behvar3o <- select(behvar3o, -c(Intercept, Sexm, TL.C, ConditionFactor.C, InfectionScore.C))

behavCOEFs <- merge(behavCOEFs, behvar3l, by = 'FishID', all.x = TRUE)
behavCOEFs <- merge(behavCOEFs, behvar3m, by = 'FishID', all.x = TRUE)
behavCOEFs <- merge(behavCOEFs, behvar3n, by = 'FishID', all.x = TRUE)
behavCOEFs <- merge(behavCOEFs, behvar3o, by = 'FishID', all.x = TRUE)


#Combining all into a big fish phenotype database
# - adding in non-behavioural data as of first trial
GULDact.processed <- read.csv('~/trophicpersonalities_A/Data_Guldborgsund/GULD_ACTdat_processed.csv', strip.white = TRUE)
GULD_phenotypes <- subset(GULDact.processed, TrialDay == 'trial 1')
GULD_phenotypes <- select(GULD_phenotypes, -c(centretime100, Notes, avespeed_tot, avespeed_mob, aveacceler, propmoving, 
                                              dist, frozenevents, timefrozen_tot, centrescore, centretime50, centretime75,
                                              Date, TimeLoaded, TrialType, TrialDay, TrialRound, ArenaID, UniqueID, X, TankID))

GULD_phenotypes <- merge(GULD_phenotypes, behavACTT1, by = 'FishID', all.x = TRUE)
GULD_phenotypes <- merge(GULD_phenotypes, behavEXPLT1, by = 'FishID', all.x = TRUE)
GULD_phenotypes <- merge(GULD_phenotypes, behavMEAN, by = 'FishID', all.x = TRUE)
GULD_phenotypes <- merge(GULD_phenotypes, behavCOEFs, by = 'FishID', all.x = TRUE)

write.csv(GULD_phenotypes, '~/trophicpersonalities_A/Data_Guldborgsund/GULD_phenotypes.csv')



#### #### 


