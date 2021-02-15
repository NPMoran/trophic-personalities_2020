# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#Karrebaek.1. Preliminary data processing and analysis ----


Sys.setenv(LANG = "en")


#Loading required packages- 
library(dplyr); library(ggplot2); library(ggpubr);
library(lme4); library(lmerTest); library(rptR); library(car)


#Loading required datasets-
KARRact <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARR_ACTdat.csv")
labels(KARRact)
KARRact <- subset (KARRact, FishID != 'NOFISH')


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
#Variation across areas as expected, i.e. higher ppmm in central arenas


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
#  Mob min. speed- 5mm/s (originally set to 1mm/s)
Settings_test <- NULL
Settings_test$minmobilityspeedset <- c(0, 1, 3, 6, 9, 12, 15, 18, 20) 
Settings_test$mobrate_act5_4a <- c(1, 0.32, 0.05, 0.01, 0.00, 0.00, 0.00, 0.00, 0.00) #Inactive fish (ACT5_4A)
Settings_test$mobrate_act4_3g <- c(1, 0.93, 0.84, 0.76, 0.71, 0.68, 0.65, 0.62, 0.61) #Very active (ACT4_3G)
Settings_test$mobratediff <- (Settings_test$mobrate_act4_3g - Settings_test$mobrate_act5_4a)
par(mfrow=c(1,3))    
plot(Settings_test$minmobilityspeedset, Settings_test$mobrate_act5_4a) #Curve is steady exponential decline
plot(Settings_test$minmobilityspeedset, Settings_test$mobrate_act4_3g) #Flattens by about 5mm 
plot(Settings_test$minmobilityspeedset, Settings_test$mobratediff) 
abline(v = 5, col="purple") #Set to 5mm/s capture difference between behaviours 

#  Frozen event. Max. Dist- 25mm (originally set to 5mm)  (not currently being used)
Settings_test$frozenmaxdist <- c(0, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30) 
Settings_test$timefrozen_act5_4a <- c(0, 0, 352, 1171, 1195, 1198, 1199, 1199, 1199, 1199, 1199, 1199) #Inactive fish (ACT5_4A)
Settings_test$timefrozen_act4_3g <- c(0, 0, 0, 27, 47, 77, 83, 105, 116, 138, 145, 154) #Very active (ACT4_3G)
Settings_test$timefrozendiff <- (Settings_test$timefrozen_act5_4a - Settings_test$timefrozen_act4_3g)
plot(Settings_test$frozenmaxdist, Settings_test$timefrozen_act5_4a) #Maxes out at approx 10mm
plot(Settings_test$frozenmaxdist, Settings_test$timefrozen_act4_3g) #Some Flattening between 12-21mm
plot(Settings_test$frozenmaxdist, Settings_test$timefrozendiff) 
abline(v = 25, col="purple") #Set to 25mm/s capture stable difference between behaviours 
#Set to 25mm capture difference between behaviours 
#  Frozen Event Min. Time- 3s  (not currently being used)
#  Transitions time Int.- 7s  (not currently being used)



### K.1.3 Preliminary data exploration ----

# Variables of interest
# in: avespeed_tot        
#     avespeed_mob
#     propmoving          
#     dist
#     timefrozen_tot
#     centretime50        
#     centretime75
#     centretime100       
#     centrescore


#Calculating centre use variables:
# - centretime50: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (50-75mm, 75-100mm, 100+mm)
KARRact$centretime50 <- (KARRact$centre_50_75+KARRact$centre_75_100+KARRact$centre_100_)/30

# - centretime75: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (75-100mm, 100+mm)
KARRact$centretime75 <- (KARRact$centre_75_100+KARRact$centre_100_)/30

# - centretime100: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (75-100mm, 100+mm)
KARRact$centretime100 <- (KARRact$centre_100_)/30

# - centrescore: calculated from the proportion of time spend in 25mm widt zones from the edge of the arena
KARRact$centrescore <- ((1*(KARRact$centre_0_25))+(2*KARRact$centre_25_50)+(3*KARRact$centre_50_75)+(4*KARRact$centre_75_100)+(5*KARRact$centre_100_))/
  (KARRact$centre_0_25+KARRact$centre_25_50+KARRact$centre_50_75+KARRact$centre_75_100+KARRact$centre_100_)



#visualizing variation across in treatment groups across the Trial days
KARRact$TrialDay[KARRact$TrialDay == 4] <- "Day 0"
KARRact$TrialDay[KARRact$TrialDay == 5] <- "Day 2"
KARRact$TrialDay[KARRact$TrialDay == 6] <- "Day 10"
KARRact$TrialDay <- ordered(KARRact$TrialDay, levels = c("Day 0","Day 2","Day 10"))

KARRact$Treatment[KARRact$Treatment == 'cont'] <- "Control"
KARRact$Treatment[KARRact$Treatment == 'clip'] <- "PIT+clip"
KARRact$Treatment[KARRact$Treatment == 'pit'] <- "PITtagged"
KARRact$Treatment <- ordered(KARRact$Treatment, levels = c("Control","PITtagged","PIT+clip"))

ggplot(KARRact, aes(x = TrialDay, y = dist, fill=factor(Treatment))) + geom_boxplot() + scale_fill_manual(values=c("white","seashell2","red")) + simpletheme 
ggplot(KARRact, aes(x = TrialDay, y = propmoving, fill=factor(Treatment))) + geom_boxplot() + scale_fill_manual(values=c("white","seashell2","red")) + simpletheme 
ggplot(KARRact, aes(x = TrialDay, y = avespeed_mob, fill=factor(Treatment))) + geom_boxplot() + scale_fill_manual(values=c("white","seashell2","red")) + simpletheme 
ggplot(KARRact, aes(x = TrialDay, y = centrescore, fill=factor(Treatment))) + geom_boxplot() + scale_fill_manual(values=c("white","seashell2","red")) + simpletheme 
ggplot(KARRact, aes(x = TrialDay, y = avespeed_tot, fill=factor(Treatment))) + geom_boxplot() + scale_fill_manual(values=c("white","seashell2","red")) + simpletheme 
ggplot(KARRact, aes(x = TrialDay, y = timefrozen_tot, fill=factor(Treatment))) + geom_boxplot() + scale_fill_manual(values=c("white","seashell2","red")) + simpletheme 
ggplot(KARRact, aes(x = TrialDay, y = centretime50, fill=factor(Treatment))) + geom_boxplot() + scale_fill_manual(values=c("white","seashell2","red")) + simpletheme 
ggplot(KARRact, aes(x = TrialDay, y = centretime75, fill=factor(Treatment))) + geom_boxplot() + scale_fill_manual(values=c("white","seashell2","red")) + simpletheme 
ggplot(KARRact, aes(x = TrialDay, y = centretime100, fill=factor(Treatment))) + geom_boxplot() + scale_fill_manual(values=c("white","seashell2","red")) + simpletheme 
#Notable reduction in activity, and potential increase in center time in day 10



### K.1.4 Processing databases for analysis ----
#- Adjusting TankID factor
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

#- Adding state variables using data as measured on Day 0 trials
#  day O measurements are considered more accurate, as later measurements were taken just to confirm identification, so day 0 used throughout
KARRfish <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARRfish_10112020.csv")
KARRfish <- select(KARRfish, -c(RANDBETWEEN.0.1000000., TankID, Treatment, TrialID, ArenaID, PITID, Sex))

#  removing SL, TL + weights measured at trials, 
KARRact <- select(KARRact, -c(TL, SL, Weight))

#  removing variables not used in analysis
KARRact <- select(KARRact, -c(ImageJ_width_pixel, ImageJ_height_pixel, ppmm_x_calc, ppmm_y_calc, width_pixel, 
                              height_pixel, width_mm, height_mm, arenaarea_m2, visibilityrate, aveacceler,
                              frozenevents, timefrozen_tot.raw, timefrozen_ave.raw, timefrozen_ave, offset,
                              centre_0_25, centre_25_50, centre_50_75, centre_75_100, centre_100_))

#  merging databases
KARRact <- merge(KARRfish, KARRact, by = "FishID", all.x = TRUE)


#- Checking fish state variables
#    Sex
n_distinct(subset(KARRact,  Sex == 'F')$FishID) #n = 16
n_distinct(subset(KARRact, Sex == 'M')$FishID) #n = 32

#    TL
summary(KARRact.processed$TL) #range 10.1 - 18.6 cm
#cor.test(KARRact.processed$TL, KARRact.processed$SL)
#TL and SL strongly correlated, so TL used here
#cor.test(KARRact.processed$TL, KARRact.processed$Weight)
#TL and SL also strongly correlated, so only TL used here

#    Condition factor
KARRact$ConditionFactor <- 100* (KARRact$Weight / ((KARRact$TL)^3))

#    InfectionScore
summary(KARRact$InfectionScore) 
#Score of 1-3: 
#1 = 0 - 10% of fins with ectoparasite/fungal infection
#2 = 10 - 50% of fins with ectoparasite/fungal infection
#3 = 50 - 100% of fins with ectoparasite/fungal infection


#Reorganising dataframe
labels(KARRact)
KARRact <- KARRact %>% relocate(UniqueID, .after = ArenaID)
KARRact <- KARRact %>% relocate(ConditionFactor, .after = Weight)
KARRact <- KARRact %>% relocate(TankID.combo, .after = TankID)
KARRact <- KARRact %>% relocate(Notes, .after = centrescore)
KARRact <- KARRact %>% relocate(PITID, .after = FishID)
KARRact <- KARRact %>% relocate(Treatment, .after = PITID)
KARRact <- KARRact %>% relocate(Sex, .after = Treatment)


#Writing dataset
write.csv(KARRact, "~/trophicpersonalities_A/Data_Karrebaek/KARR_ACTdat_processed.csv")




