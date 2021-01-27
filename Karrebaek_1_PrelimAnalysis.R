# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



#Karrabaek.1. Preliminary data processing and analysis ----


Sys.setenv(LANG = "en")


#Loading required packages- 
library(dplyr); library(ggplot2); library(ggpubr);
library(lme4); library(lmerTest); library(rptR); library(car)


#Loading required datasets-
KARRact <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARR_ACTdat.csv")
labels(KARRact)


#Re-creating the unique id for each trial-
KARRact$UniqueID <- paste(KARRact$TrialType, KARRact$TrialDay, sep = "")
KARRact$UniqueID <- paste(KARRact$UniqueID, KARRact$TrialRound, sep = "_")
KARRact$UniqueID <- paste(KARRact$UniqueID, KARRact$ArenaID, sep = "")


#Creating a general theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"),
                       axis.text.x = element_text(size = 10, colour = "black"), 
                       panel.background = element_rect(fill = "white"),
                       axis.title.y  = element_text(size=12, vjust = 2),
                       axis.title.x  = element_text(size=12, vjust = 0.1),
                       panel.border = element_rect(colour = "black", fill=NA, size = 1))



### K.1.1 Calibration of ToxTrac (ACT trials) ----


#Calibration pixel/mm ratios are calculated manually for each arena:

# - Pixel length/width calculated via ImageJ at the midpoint of each ACT arena
#    (Schneider, C. A.; Rasband, W. S. & Eliceiri, K. W. (2012), "NIH Image to ImageJ: 25 years of image analysis", Nature methods 9(7): 671-675)
#    ImageJ files in: "~/trophicpersonalities_A/1_PrelimAnalysis_Karrebaek/ImageJ_Calibration.csv 
# - Pixel/mm ratio calculated using known length/width of arenas
#    length = 492.5mm
#    width = 322.5mm
# - Pixel/mm ratios manually entered individually into each Toxtrac tracking file. 


#Visualizing calibration ratios across arenas-
ppmm_x <- ggplot(KARRact, aes(x = ArenaID, y = ppmm_x_calc)) + geom_boxplot(fill = 'deepskyblue') + simpletheme + ylim(0.45, 0.6)
ppmm_y <- ggplot(KARRact, aes(x = ArenaID, y = ppmm_y_calc)) + geom_boxplot(fill = 'deeppink') + simpletheme + ylim(0.45, 0.6)

ppmm_xy <- ggarrange(ppmm_x, ppmm_y,
          ncol = 2, nrow = 1)
ppmm_xy


#Visualizing calculated arena measurement from Toxtrac tracking files
#  for toxtrac, arena dimensions were accepted when within 10mm of actual measured length
width_mm <- ggplot(KARRact, aes(x = ArenaID, y = width_mm)) + 
  geom_boxplot(fill = 'deepskyblue') + 
  simpletheme + 
  geom_hline(yintercept = 322.5, linetype = 2, colour = "black", size = 0.75) +
  geom_hline(yintercept = 312.5, linetype = 2, colour = "black", size = 0.5) +
  geom_hline(yintercept = 332.5, linetype = 2, colour = "black", size = 0.5) +
  scale_y_continuous(limits = c(280,360), expand = c(0, 0), breaks=c(282.5, 302.5, 322.5, 342.5))

height_mm <- ggplot(KARRact, aes(x = ArenaID, y = height_mm)) + 
  geom_boxplot(fill = 'deeppink') + 
  simpletheme + 
  geom_hline(yintercept = 492.5, linetype = 2, colour = "black", size = 0.75) +
  geom_hline(yintercept = 502.5, linetype = 2, colour = "black", size = 0.5) +
  geom_hline(yintercept = 482.5, linetype = 2, colour = "black", size = 0.5) +
  scale_y_continuous(limits = c(450,530), expand = c(0, 0), breaks=c(452.5, 472.5, 492.5, 512.5))
  
height_width_arenas <- ggarrange(width_mm, height_mm,
                     ncol = 2, nrow = 1)
height_width_arenas
#Calibration appears successful within the acceptable range



### K.1.2 Optimization of ToxTrac (ACT trials) ----
# Project Initials Settings: 
#  Start at (min/s)- 5:00, Finish at(min/s)- 25:00 
#  Fill Temp. Holes, Max Size 25 Frames
#  Fill Spacial Holes, Max Size 25 Frames
#  Fill Extremes

# Tracking Settings: Use defaults

# Statistics Advanced Options:
#  Mob min. speed- 10mm/s (originally set to 1mm/s)
Settings_test <- NULL
Settings_test$minmobilityspeedset <- c(0, 1, 3, 6, 9, 12, 15, 18, 20) # Frozen event dist set to 20mm
Settings_test$mobrate_act5_4a <- c(1, 0.32, 0.05, 0.01, 0.00, 0.00, 0.00, 0.00, 0.00) #Inactive fish (ACT5_4A)
Settings_test$mobrate_act4_3g <- c(1, 0.93, 0.84, 0.76, 0.71, 0.68, 0.65, 0.62, 0.61) #Very active (ACT4_3G)
Settings_test$mobratediff <- (Settings_test$mobrate_act4_3g - Settings_test$mobrate_act5_4a)
par(mfrow=c(1,3))    
plot(Settings_test$minmobilityspeedset, Settings_test$mobrate_act5_4a) #Curve begins to flatten around 9mm/s
plot(Settings_test$minmobilityspeedset, Settings_test$mobrate_act4_3g) #Reaches 0 by 9mm/s 
plot(Settings_test$minmobilityspeedset, Settings_test$mobratediff) 
#Set to 10mm/s capture difference between behaviours 
#(may try to bring down to 5-10mm)

#  Frozen event. Max. Dist- 20mm (originally set to 5mm)
Settings_test <- NULL
Settings_test$frozenmaxdist <- c(0, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30) #Mob min. speed set to 10mm/s
Settings_test$timefrozen_act5_4a <- c(0, 0, 352, 1171, 1195, 1198, 1199, 1199, 1199, 1199, 1199, 1199) #Inactive fish (ACT5_4A)
Settings_test$timefrozen_act4_3g <- c(0, 0, 0, 27, 47, 77, 83, 105, 116, 138, 145, 154) #Very active (ACT4_3G)
Settings_test$timefrozendiff <- (Settings_test$timefrozen_act4_3g - Settings_test$timefrozen_act5_4a)
plot(Settings_test$frozenmaxdist, Settings_test$timefrozen_act5_4a) #Maxes out at approx 10mm
plot(Settings_test$frozenmaxdist, Settings_test$timefrozen_act4_3g) #Some Flattening between 12-21mm
plot(Settings_test$frozenmaxdist, Settings_test$timefrozendiff) 
#Set to 20mm capture difference between behaviours 
#(can try to bring down to 10-15)

#  Frozen Event Min. Time- 3s
#  Transitions time Int.- 7s (not used)


### K.1.3 Distribution checks for behavioural variables (ACT) ----
#avespeed_tot: (mm/s) the average speed of the individual accross the full trial period
ggplot(KARRact) + aes(x = avespeed_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 4.5) + simpletheme 
ggqqplot(KARRact$avespeed_tot) #positive skew
ggplot(KARRact) + aes(x = sqrt(avespeed_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(sqrt(KARRact$avespeed_tot)) #root transformation is improved

#avespeed_mob: (mm/s) the average speed of the individual excluding periods when it was immobile
ggplot(KARRact) + aes(x = avespeed_mob) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(KARRact$avespeed_mob) #some negative skew due to 5 - 10 very inactive fish

#aveacceler: (mm/s^2) average rate of acceleration accross the trial
ggplot(KARRact) + aes(x = aveacceler) + geom_histogram(color="black", fill="lightblue", binwidth = 14) + simpletheme 
ggqqplot(KARRact$aveacceler) #minimal positive skew

#propmoving: (proportional) proportion of time mobile
ggplot(KARRact) + aes(x = propmoving) + geom_histogram(color="black", fill="lightblue", binwidth = 0.05) + simpletheme 
ggqqplot(KARRact$propmoving) #no clear skew, potential issues with high proportion of low activity fish

#dist: (mm) total distance travelled during trial
ggplot(KARRact) + aes(x = dist) + geom_histogram(color="black", fill="lightblue", binwidth = 4800) + simpletheme 
ggqqplot(KARRact$dist) #positive skew
ggplot(KARRact) + aes(x = sqrt(dist)) + geom_histogram(color="black", fill="lightblue", binwidth = 12) + simpletheme 
ggqqplot(sqrt(KARRact$dist)) #root transformation is improved

#frozenevents: (count) 
ggplot(KARRact) + aes(x = frozenevents) + geom_histogram(color="black", fill="lightblue", binwidth = 8) + simpletheme 
ggqqplot(KARRact$frozenevents) #no clear deviation from normal, may be poisson

#timefrozen_tot: (s) total time spent frozen during trial
ggplot(KARRact) + aes(x = timefrozen_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 85) + simpletheme 
ggqqplot(KARRact$timefrozen_tot) #no clear skew, potential issues with high proportion of low activity fish

#timefrozen_ave: (s) total duration of frozen periods
ggplot(KARRact) + aes(x = timefrozen_ave) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
ggqqplot(KARRact$timefrozen_ave) #severe positive skew zero so log transformation applied (consider running as a poisson distribution)
ggplot(KARRact) + aes(x = log(timefrozen_ave)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.45) + simpletheme 
ggqqplot(log(KARRact$timefrozen_ave)) #log transformation is improved but still quite skewed,

#centretime50: (s) time >5cm away from edge
ggplot(KARRact) + aes(x = centretime50) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
ggqqplot(KARRact$centretime50) #positive skew with zeros so log(x+1) and sqrt transformation applied
ggplot(KARRact) + aes(x = log(centretime50 + 1)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(KARRact$centretime50 + 1)) #left skewed
ggplot(KARRact) + aes(x = sqrt(centretime50)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
ggqqplot(sqrt(KARRact$centretime50)) #approximately normal

#centretime75: (s) time >7.5cm away from edge
ggplot(KARRact) + aes(x = centretime75) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
ggqqplot(KARRact$centretime75) #positive skew with zeros so log(x+1) and sqrt transformation applied
ggplot(KARRact) + aes(x = log(centretime75 + 1)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(KARRact$centretime75 + 1)) #negative skewed
ggplot(KARRact) + aes(x = sqrt(centretime75)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
ggqqplot(sqrt(KARRact$centretime75)) #close to normal

#centretime100: (s) time >10cm away from edge
ggplot(KARRact) + aes(x = centretime100) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
ggqqplot(KARRact$centretime100) #positive skew with zeros so log(x+1) and sqrt transformation applied
ggplot(KARRact) + aes(x = log(centretime100 + 1)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(KARRact$centretime100 + 1)) #negative skewed
ggplot(KARRact) + aes(x = sqrt(centretime100)) + geom_histogram(color="black", fill="lightblue", binwidth = 1.5) + simpletheme 
ggqqplot(sqrt(KARRact$centretime100)) #closer to normal



#Summary of behavioural variables
#     avespeed_tot	  - sqrt transformation
#     avespeed_mob	  - no transformation needed
#     aveacceler	    - no transformation needed
#     propmoving	    - UNCLEAR issue with high proportion of inactive fish, use no transformation for now
#     dist	          - sqrt transformation
#     frozenevents	  - no transformation needed
#     timefrozen_tot	- UNCLEAR issue with high proportion of inactive fish, use no transformation for now
#     timefrozen_ave  - log transformation 
#     centretime50	  - sqrt transformation
#     centretime75	  - sqrt transformation
#     centretime100   - sqrt transformation


### K.1.4 preparing datasets for treatment analysis 
#Behaviour variable transformations
KARRact$avespeed_tot.sqrt <- sqrt(KARRact$avespeed_tot)
KARRact$dist.sqrt <- sqrt(KARRact$dist)
KARRact$timefrozen_ave.ln <- log(KARRact$timefrozen_ave)
KARRact$centretime50.sqrt <- sqrt(KARRact$centretime50)
KARRact$centretime75.sqrt <- sqrt(KARRact$centretime75)
KARRact$centretime100.sqrt <- sqrt(KARRact$centretime100)


#Adjusting TankID factor
#Running Tank ID as 2 categories, D and E, as D_1 - D_4 were interconnected so considered a better grouping variable
KARRact$TankID.combo <- KARRact$TankID
KARRact$TankID.combo[KARRact$TankID.combo == "D_4"] <- "D"
KARRact$TankID.combo[KARRact$TankID.combo == "D_3"] <- "D"
KARRact$TankID.combo[KARRact$TankID.combo == "D_2"] <- "D"
KARRact$TankID.combo[KARRact$TankID.combo == "D_1"] <- "D"
KARRact$TankID.combo[KARRact$TankID.combo == "E_4"] <- "E"
KARRact$TankID.combo[KARRact$TankID.combo == "E_3"] <- "E"
KARRact$TankID.combo[KARRact$TankID.combo == "E_2"] <- "E"
KARRact$TankID.combo[KARRact$TankID.combo == "E_1"] <- "E"


#Adding state variables using data as measured on Day 0 trials
#  day O measurements are considered more accurate, as later measurements were taken just to confirm identification, so day 0 used throughout
KARRfish <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARRfish_10112020.csv")
KARRfish <- select(KARRfish, -c(RANDBETWEEN.0.1000000., TankID, Treatment, TrialID, ArenaID, PITID, Sex))

#  removing SL, TL + weights measured at trials, 
KARRact <- select(KARRact, -c(TL, SL, Weight))

#  merging databases
KARRact.processed <- merge(KARRfish, KARRact, by = "FishID", all.x = TRUE)


#Checking fish state variables
#    Sex
n_distinct(subset(KARRact.processed,  Sex == 'F')$FishID) #n = 16
n_distinct(subset(KARRact.processed, Sex == 'M')$FishID) #n = 32

#    TL
summary(KARRact.processed$TL) #range 10.1 - 18.6 cm
#cor.test(KARRact.processed$TL, KARRact.processed$SL)
#TL and SL strongly correlated, so TL used as this is the one used in GULD fish
#cor.test(KARRact.processed$TL, KARRact.processed$Weight)
#TL and SL also strongly correlated, so only TL used here

#    Condition factor
KARRact.processed$ConditionFactor <- 100* (KARRact.processed$Weight / ((KARRact.processed$TL)^3))

#    InfectionScore
summary(KARRact.processed$InfectionScore) 
#Score of 1-3: 
#1 = 0 - 10% of fins with ectoparasite/fungal infection
#2 = 10 - 50% of fins with ectoparasite/fungal infection
#3 = 50 - 100% of fins with ectoparasite/fungal infection


#Minor formatting adjustments:

#  changing name of treatment group, so control group is the intercept in treatment models  
KARRact.processed$Treatment[KARRact.processed$Treatment == "clip"] <- "finclip"


#  removing obsolete columns
KARRact.processed <- select(KARRact.processed, -c(arenaarea_m2, timefrozen_tot.raw, timefrozen_ave.raw, offset))

labels(KARRact.processed)
#  reordering columns
KARRact.processed <- KARRact.processed[, c(1,2,3,4,51,5,7,8,9,10,11,12,43,13,50,14,15,16,17,18,19,20,21,22,23,24,25,26,44,27,28,29,30,45,31,32,33,46,34,35,36,37,38,39,40,47,41,48,42,49,6)]


#Writing dataset
write.csv(KARRact.processed, "~/trophicpersonalities_A/Data_Karrebaek/KARR_ACTdat_processed.csv")





#### K.1.4 Testing for systematic variation in behavioural variables ----
#
##Variables of interest-
##  avespeed_tot.sqrt
##  avespeed_mob
##  aveacceler
##  propmoving
##  dist.sqrt
##  frozenevents
##  timefrozen_tot
##  timefrozen_ave.ln
##  centretime50.sqrt
##  centretime75.sqrt
##  centretime100.sqrt
#
#
##Systematic factors-
##  TrialDay 
##  ArenaID 
##  TankID
#
#
##Testing for systemic issues 
#KARR_avespeed_tot.sqrt.mod <- lmer(avespeed_tot.sqrt ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_avespeed_tot.sqrt.mod) #TrialDay, and TankID effects, no ArenaID effects
#plot(KARR_avespeed_tot.sqrt.mod) #no clustering issues
#
#KARR_avespeed_mob.mod <- lmer(avespeed_mob ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_avespeed_mob.mod) #TrialDay effects, no ArenaID or TankID effects
#plot(KARR_avespeed_mob.mod) #no clustering issues
#
#KARR_aveacceler.mod <- lmer(aveacceler ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_aveacceler.mod) #TrialDay, and TankID effects, no ArenaID effects (marginal)
#plot(KARR_aveacceler.mod) #no clustering issues
#
#KARR_propmoving.mod <- lmer(propmoving ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_propmoving.mod) #TrialDay, and TankID effects, no ArenaID effects (marginal)
#plot(KARR_propmoving.mod) #no clustering issues
#
#KARR_dist.sqrt.mod <- lmer(dist.sqrt ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_dist.sqrt.mod) #TrialDay, and TankID effects, no ArenaID effects (marginal)
#plot(KARR_dist.sqrt.mod) #no clustering issues
#
#KARR_frozenevents.mod <- lmer(frozenevents ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_frozenevents.mod) #TankID effects, no TrialDay + ArenaID effects (marginal)
#plot(KARR_frozenevents.mod) #no clustering issues
#
#KARR_timefrozen_tot.mod <- lmer(timefrozen_tot ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_timefrozen_tot.mod) #TankID, TrialDay + ArenaID effects 
#plot(KARR_timefrozen_tot.mod) #no clustering issues
#
#KARR_timefrozen_ave.ln.mod <- lmer(timefrozen_ave.ln ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_timefrozen_ave.ln.mod) #TankID effects, no TrialDay + ArenaID effects (marginal)
#plot(KARR_timefrozen_ave.ln.mod) #no clustering issues
#
#KARR_centretime50.sqrt.mod <- lmer(centretime50.sqrt ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_centretime50.sqrt.mod) #no effects
#plot(KARR_centretime50.sqrt.mod) #no clustering issues
#
#KARR_centretime75.sqrt.mod <- lmer(centretime75.sqrt ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_centretime75.sqrt.mod) #no effects
#plot(KARR_centretime75.sqrt.mod) #no clustering issues
#
#KARR_centretime100.sqrt.mod <- lmer(centretime100.sqrt ~ TrialDay + TankID.combo + ArenaID + (1|FishID), data=KARRact)
#Anova(KARR_centretime100.sqrt.mod) #no effects
#plot(KARR_centretime100.sqrt.mod) #no clustering issues
#
#
##Systematic factors for account for-
##  avespeed_tot.sqrt:   TrialDay, TankID.combo
##  avespeed_mob:        TrialDay
##  aveacceler:          TrialDay, TankID.combo
##  propmoving:          TrialDay, TankID.combo
##  dist.sqrt:           TrialDay, TankID.combo
##  frozenevents:        TrialDay, 
##  timefrozen_tot:      TrialDay, TankID.combo, ArenaID
##  timefrozen_ave.ln:   TankID.combo
##  centretime50.sqrt:   nil
##  centretime75.sqrt:   nil
##  centretime100.sqrt:  nil
#
#
##State factors to account for- 
#
#KARR_avespeed_tot.sqrt.mod2 <- lmer(avespeed_tot.sqrt ~ Sex + TL + ConditionFactor + InfectionScore + (1|TrialDay) + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
#Anova(KARR_avespeed_tot.sqrt.mod2) #no effects
#plot(KARR_avespeed_tot.sqrt.mod2) #no clustering issues
#
#KARR_avespeed_mob.mod2 <- lmer(avespeed_mob ~ Sex + TL + ConditionFactor + InfectionScore + (1|TrialDay) + (1|FishID), data=KARRact.processed)
#Anova(KARR_avespeed_mob.mod2) #no effects (marginal Sex difference)
#plot(KARR_avespeed_mob.mod2) #no clustering issues
#
#KARR_aveacceler.mod2 <- lmer(aveacceler ~ Sex + TL + ConditionFactor + InfectionScore + (1|TrialDay) + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
#Anova(KARR_aveacceler.mod2) #no effects
#plot(KARR_aveacceler.mod2) #no clustering issues
#
#KARR_propmoving.mod2 <- lmer(propmoving ~ Sex + TL + ConditionFactor + InfectionScore + (1|TrialDay) + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
#Anova(KARR_propmoving.mod2) #no effects
#plot(KARR_propmoving.mod2) #no clustering issues
#
#KARR_dist.sqrt.mod2 <- lmer(dist.sqrt ~ Sex + TL + ConditionFactor + InfectionScore + (1|TrialDay) + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
#Anova(KARR_dist.sqrt.mod2) #no effects
#plot(KARR_dist.sqrt.mod2) #no clustering issues
#
#KARR_frozenevents.mod2 <- lmer(frozenevents ~  Sex + TL + ConditionFactor + InfectionScore + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
#Anova(KARR_frozenevents.mod2) #no effects
#plot(KARR_frozenevents.mod2) #no clustering issues
#
#KARR_timefrozen_tot.mod2 <- lmer(timefrozen_tot ~ Sex + TL + ConditionFactor + InfectionScore + (1|TrialDay) + (1|TankID.combo) + (1|ArenaID) + (1|FishID), data=KARRact.processed)
#Anova(KARR_timefrozen_tot.mod2)  #no effects
#plot(KARR_timefrozen_tot.mod2) #no clustering issues
#
#KARR_timefrozen_ave.ln.mod2 <- lmer(timefrozen_ave.ln ~ Sex + TL + ConditionFactor + InfectionScore + (1|TankID.combo) + (1|FishID), data=KARRact.processed)
#Anova(KARR_timefrozen_ave.ln.mod2)  #no effects (marginal Condition difference)
#plot(KARR_timefrozen_ave.ln.mod2) #no clustering issues
#
#KARR_centretime50.sqrt.mod2 <- lmer(centretime50.sqrt ~ Sex + TL + ConditionFactor + InfectionScore + (1|FishID), data=KARRact.processed)
#Anova(KARR_centretime50.sqrt.mod2) #ConditionFactor effect
#summary(KARR_centretime50.sqrt.mod2) #positive effect
#plot(KARR_centretime50.sqrt.mod2) #no clustering issues
#
#KARR_centretime75.sqrt.mod2 <- lmer(centretime75.sqrt ~ Sex + TL + ConditionFactor + InfectionScore + (1|FishID), data=KARRact.processed)
#Anova(KARR_centretime75.sqrt.mod2) #ConditionFactor effect
#summary(KARR_centretime75.sqrt.mod2) #positive effect
#plot(KARR_centretime75.sqrt.mod2) #no clustering issues
#
#KARR_centretime100.sqrt.mod2 <- lmer(centretime100.sqrt ~ Sex + TL + ConditionFactor + InfectionScore + (1|FishID), data=KARRact.processed)
#Anova(KARR_centretime100.sqrt.mod2) #ConditionFactor + Infection Score effect
#summary(KARR_centretime100.sqrt.mod2) #positive effects
#plot(KARR_centretime100.sqrt.mod2) #no clustering issues
#
#
##Summary-
#
##  Variable             Systematic effects                Fixed effects
#
##  avespeed_tot.sqrt:   TrialDay, TankID.combo            nil
##  avespeed_mob:        TrialDay                          nil
##  aveacceler:          TrialDay, TankID.combo            nil
##  propmoving:          TrialDay, TankID.combo            nil
##  dist.sqrt:           TrialDay, TankID.combo            nil
##  frozenevents:        TrialDay,                         nil
##  timefrozen_tot:      TrialDay, TankID.combo, ArenaID   nil
##  timefrozen_ave.ln:   TankID.combo                      nil
##  centretime50.sqrt:   nil                               ConditionFactor
##  centretime75.sqrt:   nil                               ConditionFactor
##  centretime100.sqrt:  nil                               ConditionFactor, InfectionScore
#
#