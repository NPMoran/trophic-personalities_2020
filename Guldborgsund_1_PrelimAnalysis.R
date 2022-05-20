#
# Title: How to Quantify Behavioural and Trophic Variation Among-Individuals: A case study using an invasive marine fish
#
# Author: Nicholas Moran, 
#         Centre for Ocean Life- DTU Aqua
#         National Institute for Aquatic Resources
#         Technical University of Denmark


#### 1. Preliminary data processing and analysis ####

Sys.setenv(LANG = "en")

#Loading required packages- 
library(dplyr); library(ggplot2); library(ggpubr)

#Creating a general theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"),axis.text.x = element_text(size = 10, colour = "black"), panel.background = element_rect(fill = "white"),axis.title.y  = element_text(size=12, vjust = 2),axis.title.x  = element_text(size=12, vjust = 0.1),panel.border = element_rect(colour = "black", fill=NA, size = 1))



## 1.1 ToxTrac calibration (ACT trials) ----

#Calibration pixel/mm ratios are calculated manually for each arena:

# - Pixel length/width calculated via ImageJ at the midpoint of each ACT arena
#    (Schneider, C. A.; Rasband, W. S. & Eliceiri, K. W. (2012), "NIH Image to ImageJ: 25 years of image analysis", Nature methods 9(7): 671-675)
#    ImageJ files in: "~/trophicpersonalities_A/1_PrelimAnalysis_Karrebaek/ImageJ_Calibration.csv 
# - Pixel/mm ratio calculated using known length/width of arenas
#    length = 492.5mm
#    width = 322.5mm
# - Pixel/mm ratios manually entered individually into each Toxtrac tracking file. 


#Visualizing calibration ratios across arenas-
ppmm_x <- ggplot(GULDact, aes(x = ArenaID, y = ppmm_x_calc)) + geom_boxplot(fill = 'deepskyblue') + simpletheme + ylim(0.45, 0.7)
ppmm_y <- ggplot(GULDact, aes(x = ArenaID, y = ppmm_y_calc)) + geom_boxplot(fill = 'deeppink') + simpletheme + ylim(0.45, 0.7)

ppmm_xy <- ggarrange(ppmm_x, ppmm_y,
                     ncol = 2, nrow = 1)
ppmm_xy
#Expected pattern, with higher ratio in central arenas and lower at edges. Effect more on horiz ration than vertical


#Visualizing calculated arena measurement from Toxtrac tracking files
#  for toxtrac, arena dimensions were accepted when within 10mm of actual measured length
width_mm <- ggplot(GULDact, aes(x = ArenaID, y = width_mm)) + 
  geom_boxplot(fill = 'deepskyblue') + 
  simpletheme + 
  geom_hline(yintercept = 322.5, linetype = 2, colour = "black", size = 0.75) +
  geom_hline(yintercept = 312.5, linetype = 2, colour = "black", size = 0.5) +
  geom_hline(yintercept = 332.5, linetype = 2, colour = "black", size = 0.5) +
  scale_y_continuous(limits = c(280,360), expand = c(0, 0), breaks=c(282.5, 302.5, 322.5, 342.5))

height_mm <- ggplot(GULDact, aes(x = ArenaID, y = height_mm)) + 
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



## G.1.2 ToxTrac Optimization (ACT trials) ----
#Used same settings as Karrebaek fish to maximize comparability across experiments

# Project Initials Settings: 
#  Start at (min/s)- 5:00, Finish at(min/s)- 25:00 
#  Fill Temp. Holes, Max Size 25 Frames
#  Fill Spacial Holes, Max Size 25 Frames
#  Fill Extremes

# Statistics Advanced Options:
#  Mob min. speed- 5mm/s (originally set to 1mm/s)
#  Frozen event. Max. Dist- 25mm (originally set to 5mm)
#  Frozen Event Min. Time- 3s
#  Transitions time Int.- 7s (not used)


# Tracking Settings: Use defaults

# Statistics Advanced Options:
#  Mob min. speed- 5mm/s (originally set to 1mm/s)
Settings_test <- NULL
Settings_test$minmobilityspeedset <- c(0, 1, 3, 6, 9, 12, 15, 18, 20) 
Settings_test$mobrate_act1_1a <- c(1, 0.93, 0.79, 0.67, 0.60, 0.56, 0.52, 0.49, 0.46) #Relatively active fish
Settings_test$mobrate_act1_1c <- c(1, 0.59, 0.15, 0.06, 0.04, 0.01, 0.00, 0.00, 0.00) #Very little movement
Settings_test$mobratediff <- (Settings_test$mobrate_act1_1a - Settings_test$mobrate_act1_1c)
par(mfrow=c(1,3))    
plot(Settings_test$minmobilityspeedset, Settings_test$mobrate_act1_1a) #Curve begins to flatten around 5mm
plot(Settings_test$minmobilityspeedset, Settings_test$mobrate_act1_1c) #Min 5mm/s needed to capture lack of movement
plot(Settings_test$minmobilityspeedset, Settings_test$mobratediff) 
abline(v = 5, col="purple") #Set to 5mm/s capture difference between behaviours 

#  Frozen event. Max. Dist- 25mm (originally set to 5mm)
Settings_test$frozenmaxdist <-      c(0,  1,  3,  6,  9,  12, 15, 18, 21, 24, 27, 30) 
Settings_test$timefrozen_act1_1a <- c(14, 14, 15, 19, 19, 25, 67, 119, 186, 248, 271, 286) #Relatively active fish
Settings_test$timefrozen_act1_1c <- c(0, 0, 0, 29, 396, 755, 952, 1052, 1088, 1121, 1146, 1168) #Very little movement
Settings_test$timefrozendiff <- (Settings_test$timefrozen_act1_1c - Settings_test$timefrozen_act1_1a)
plot(Settings_test$frozenmaxdist, Settings_test$timefrozen_act1_1a) #Curve begins to flatten around 20mm
plot(Settings_test$frozenmaxdist, Settings_test$timefrozen_act1_1c) #Min 20mm/s needed to capture lack of movement
plot(Settings_test$frozenmaxdist, Settings_test$timefrozendiff) #Set to 20mm capture difference between behaviours 
abline(v = 25, col="purple") #Set to 25mm/s capture stable difference between behaviours 
#  Frozen Event Min. Time- 3s 
#  Transitions time Int.- 7s (not used)



## G.1.3 Preliminary ACT data exploration ----

# Variables of interest
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

#Loading required dataset-
GULDact <- read.csv("~/trophicpersonalities_A/Data_Guldborgsund/GULD_ACTdat.csv", strip.white = TRUE)
labels(GULDact)
GULDact <- subset(GULDact, PITID != 'NOFISH') #excluding rows with no fish
nrow(GULDact) #124 trials
n_distinct(GULDact$PITID) #data for 47 fish

#Re-creating the unique id for each trial-
GULDact$UniqueID <- paste(GULDact$TrialType, GULDact$TrialDay, sep = "")
GULDact$UniqueID <- paste(GULDact$UniqueID, GULDact$TrialRound, sep = "_")
GULDact$UniqueID <- paste(GULDact$UniqueID, GULDact$ArenaID, sep = "")


#Calculating centre use variables:
# - centrescore: calculated from the proportion of time spend in 25mm widt zones from the edge of the arena
GULDact$centrescore <- ((1*(GULDact$centre_0_25))+(2*GULDact$centre_25_50)+(3*GULDact$centre_50_75)+(4*GULDact$centre_75_100)+(5*GULDact$centre_100_))/
  (GULDact$centre_0_25+GULDact$centre_25_50+GULDact$centre_50_75+GULDact$centre_75_100+GULDact$centre_100_)

# - centretime50: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (50-75mm, 75-100mm, 100+mm)
GULDact$centretime50 <- (GULDact$centre_50_75+GULDact$centre_75_100+GULDact$centre_100_)/30

# - centretime75: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (75-100mm, 100+mm)
GULDact$centretime75 <- (GULDact$centre_75_100+GULDact$centre_100_)/30

# - centretime100: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (75-100mm, 100+mm)
GULDact$centretime100 <- (GULDact$centre_100_)/30



#visualizing variation across the trial days
GULDact$TrialDay[GULDact$TrialDay == 1] <- "trial 1"
GULDact$TrialDay[GULDact$TrialDay == 2] <- "trial 2"
GULDact$TrialDay[GULDact$TrialDay == 3] <- "trial 3"
ggplot(GULDact, aes(x = TrialDay, y = avespeed_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = avespeed_mob)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = aveacceler)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = propmoving)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = dist)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = frozenevents)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = timefrozen_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = centretime50)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = centretime75)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = centretime100)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = centrescore)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#Some reduction in activity over trials

#visualizing variation across the trial arenas
ggplot(GULDact, aes(x = ArenaID, y = avespeed_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = avespeed_mob)) + geom_boxplot(fill = 'deepskyblue') + simpletheme
ggplot(GULDact, aes(x = ArenaID, y = aveacceler)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = propmoving)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = dist)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = frozenevents)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = timefrozen_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = centretime50)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = centretime75)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = centretime100)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = centrescore)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#no clear patterns across arenas

#visualizing variation across the trial Rounds
GULDact$TrialRound <- as.factor(GULDact$TrialRound)
ggplot(GULDact, aes(x = TrialRound, y = avespeed_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = avespeed_mob)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = aveacceler)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = propmoving)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = dist)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = frozenevents)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = timefrozen_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = centretime50)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = centretime75)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = centretime100)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = centrescore)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#no clear patterns across trial rounds

#visualizing variation across the holding tanks
ggplot(GULDact, aes(x = TankID, y = avespeed_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = avespeed_mob)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = aveacceler)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = propmoving)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = dist)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = frozenevents)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = timefrozen_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = centretime50)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = centretime75)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = centretime100)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = centrescore)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#no clear patterns across holding tanks



## G.1.4 Formatting ACT database for analysis ----

#Adding FishIDs and additional data to the dataframe
# - FishID, InfectionScore, TL data taken from data from fish tagging (collected 19/06/2020)
GULD_fish <- read.csv("~/trophicpersonalities_A/Data_Guldborgsund/GULDFish_19062020.csv")
GULD_fish <- select(GULD_fish, -c(TankID, Notes, Sex))

# - Sex taken from pre-trial assessments (collected 24/06/2020)
GULD_fish2 <- read.csv("~/trophicpersonalities_A/Data_Guldborgsund/GULDFish_25062020.csv")
GULD_fish2 <- select(GULD_fish2, -c(TankID, PITID, Mass_g, ReprodState, Notes))

GULD_fish <- merge(GULD_fish, GULD_fish2, by = 'FishID', all.x = TRUE)
GULDact <- merge(GULD_fish, GULDact, by = 'PITID', all.x = FALSE)
n_distinct(GULDact$FishID) #data for 47 fish
labels(GULDact)


#Excluding some trials due to: injuries found on individuals on post-trial examination-
#Injuries likely occured post-arrival in lab, so excluded to avoid possible behavioural effects 
GULDact <- subset(GULDact, UniqueID != "ACT1_1C") #PITID 1492
GULDact <- subset(GULDact, UniqueID != "ACT1_3F") #PITID 2155
GULDact <- subset(GULDact, UniqueID != "ACT1_4D") #PITID 2165
GULDact <- subset(GULDact, UniqueID != "ACT1_5C") #PITID 2191
GULDact <- subset(GULDact, UniqueID != "ACT2_5A") #PITID 2038
GULDact <- subset(GULDact, UniqueID != "ACT2_5B") #PITID 2155

nrow(GULDact) #118 included trials
n_distinct(GULDact$FishID) #43 fish included in analysis


#Checking fish state variables
# - Sex
GULDact$Sex[GULDact$Sex == "m(j)"] <- "m"
n_distinct(subset(GULDact, Sex == 'f')$FishID) #n = 6
n_distinct(subset(GULDact, Sex == 'm')$FishID) #n = 37

# - TL
summary(GULDact$TL) #range 9.5 - 17.3 cm
GULDact$Weight <- as.numeric(GULDact$Weight) 
#cor.test(GULDact$TL, as.numeric(GULDact$Weight))
#TL and Weight also strongly correlated, so only TL used here

# - Condition factor
GULDact$ConditionFactor <- 100* ((GULDact$Weight) / ((GULDact$TL)^3))

# - InfectionScore
summary(GULDact$InfectionScore) 
#Score of 1-3: 
#1 = 0 - 10% of fins with ectoparasite/fungal infection
#2 = 10 - 50% of fins with ectoparasite/fungal infection
#3 = 50 - 100% of fins with ectoparasite/fungal infection


#Reorganising dataframe
labels(GULDact)
# - removing variables not used in analysis
GULDact <- select(GULDact, -c(ImageJ_width_pixel, ImageJ_height_pixel, ppmm_x_calc, ppmm_y_calc, width_pixel, 
                              height_pixel, width_mm, height_mm, arenaarea_m2, visibilityrate, timefrozen_tot.raw, 
                              timefrozen_ave.raw, timefrozen_ave, offset, centre_0_25, centre_25_50, centre_50_75, 
                              centre_75_100, centre_100_))

GULDact <- GULDact %>% relocate(UniqueID, .after = ArenaID)
GULDact <- GULDact %>% relocate(Weight, .after = TL)
GULDact <- GULDact %>% relocate(ConditionFactor, .after = Weight)
GULDact <- GULDact %>% relocate(Notes, .after = centretime100)
GULDact <- GULDact %>% relocate(PITID, .after = FishID)


#Writing dataset
write.csv(GULDact, "~/trophicpersonalities_A/Data_Guldborgsund/GULD_ACTdat_processed.csv")





## G.1.5 Preliminary EXPL data exploration ----
# Variables of interest
# in: emergelat
#     endpointlat
#     endpointspeed
#     refugereturnlat


#Loading required dataset-
GULDexpl <- read.csv("~/trophicpersonalities_A/Data_Guldborgsund/GULD_EXPLdat.csv", strip.white = TRUE)
labels(GULDexpl)
GULDexpl <- subset (GULDexpl, PITID != 'NOFISH') #excluding rows with no fish
GULDexpl <- subset(GULDexpl, GULDexpl$PITID != "2044/1395") #excluding trial where fish escaped arena
nrow(GULDexpl) #118 trials
n_distinct(GULDexpl$PITID) #data for 46 fish


#Re-creating the unique id for each trial-
GULDexpl$UniqueID <- paste(GULDexpl$TrialType, GULDexpl$TrialDay, sep = "")
GULDexpl$UniqueID <- paste(GULDexpl$UniqueID, GULDexpl$TrialRound, sep = "_")
GULDexpl$UniqueID <- paste(GULDexpl$UniqueID, GULDexpl$ArenaID, sep = "")


#visualizing variation across the trial days
GULDexpl$TrialDay[GULDexpl$TrialDay == 1] <- "trial 1"
GULDexpl$TrialDay[GULDexpl$TrialDay == 2] <- "trial 2"
GULDexpl$TrialDay[GULDexpl$TrialDay == 3] <- "trial 3"
ggplot(GULDexpl, aes(x = TrialDay, y = emergelat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = TrialDay, y = endpointlat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = TrialDay, y = endpointspeed)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = TrialDay, y = refugereturnlat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#No clear patterns across trials

#visualizing variation across the trial arenas
ggplot(GULDexpl, aes(x = ArenaID, y = emergelat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = ArenaID, y = endpointlat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = ArenaID, y = endpointspeed)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = ArenaID, y = refugereturnlat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#No clear patterns across arenas

#visualizing variation across the trial rounds
GULDexpl$TrialRound <- as.factor(GULDexpl$TrialRound)
ggplot(GULDexpl, aes(x = TrialRound, y = emergelat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = TrialRound, y = endpointlat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = TrialRound, y = endpointspeed)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = TrialRound, y = refugereturnlat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#No clear patterns across rounds

#visualizing variation across the holding tanks
ggplot(GULDexpl, aes(x = TankID, y = emergelat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = TankID, y = endpointlat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = TankID, y = endpointspeed)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDexpl, aes(x = TankID, y = refugereturnlat)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#No clear patterns across tanks



## G.1.6 Formatting EXPL database for analysis ----
#Adding FishIDs to the dataframe
GULDexpl <- merge(GULD_fish, GULDexpl, by = 'PITID', all.x = FALSE)
n_distinct(GULDexpl$FishID) #data for 46 fish

#Excluding some trials due to: injuries found on individuals on post-trial examination-
#Injuries likely occurred post-arrival in lab, so excluded to avoid possible behavioural effects
GULDexpl <- subset(GULDexpl, UniqueID != "EXPL1_3A") #PITID 2155
GULDexpl <- subset(GULDexpl, UniqueID != "EXPL1_3C") #PITID 2165
GULDexpl <- subset(GULDexpl, UniqueID != "EXPL1_10B") #PITID 2191

nrow(GULDexpl) #113 trials
n_distinct(GULDexpl$PITID) #data for 43 fish


#Formatting state variables
# - running sex as m, f only
GULDexpl$Sex[GULDexpl$Sex == "m(j)"] <- "m"
n_distinct(subset(GULDexpl, Sex == 'f')$FishID) #n = 6
n_distinct(subset(GULDexpl, Sex == 'm')$FishID) #n = 37

# -calculating ConditionFactor using weight recorded during ACT trials
GULDweight <- NULL 
GULDweight$mergeID <- paste(GULDact$FishID, GULDact$TrialDay, sep = '') 
GULDweight$Weight <- GULDact$Weight
GULDweight <- as.data.frame(GULDweight)
GULDexpl$mergeID <- paste(GULDexpl$FishID, GULDexpl$TrialDay, sep = '')
GULDexpl <- merge(GULDexpl, GULDweight, by = 'mergeID', all.x = FALSE)
GULDexpl$mergeID <- NULL

GULDexpl$ConditionFactor <- 100* ((GULDexpl$Weight) / ((GULDexpl$TL)^3))


#Converting heavily bimodal variables (endpointlat, emergelat) into binomials
# -visualising distributions
#emergelat: (s) latency to emerge from the shelter
ggplot(GULDexpl) + aes(x = emergelat) + geom_histogram(color="black", fill="lightblue", binwidth = 170) + simpletheme 
ggqqplot(GULDexpl$emergelat) 

#endpointlat: (s) latency to explore to the endpoint from trial start time
ggplot(GULDexpl) + aes(x = endpointlat) + geom_histogram(color="black", fill="lightblue", binwidth = 170) + simpletheme 
ggqqplot(GULDexpl$endpointlat) 

# - conversions to binomial using median emergence time as conversion point (1 <= median, 0 > median)
median(GULDexpl$emergelat) #median = 55
GULDexpl1 <- subset(GULDexpl, emergelat <= 55)
GULDexpl2 <- subset(GULDexpl, emergelat >= 56)
GULDexpl1$emergelat.bin <- 1
GULDexpl2$emergelat.bin <- 0
GULDexpl <- rbind(GULDexpl1, GULDexpl2)

median(GULDexpl$endpointlat) #median = 178
GULDexpl1 <- subset(GULDexpl, endpointlat <= 178)
GULDexpl2 <- subset(GULDexpl, endpointlat >= 179)
GULDexpl1$endpointlat.bin <- 1
GULDexpl2$endpointlat.bin <- 0
GULDexpl <- rbind(GULDexpl1, GULDexpl2)


#Reorganising dataframe
labels(GULDexpl)
# - removing variables not used in analysis
GULDexpl <- select(GULDexpl, -c(refugereturntime, endpointtime, emergetime))

GULDexpl <- GULDexpl %>% relocate(UniqueID, .after = ArenaID)
GULDexpl <- GULDexpl %>% relocate(Weight, .after = TL)
GULDexpl <- GULDexpl %>% relocate(ConditionFactor, .after = Weight)
GULDexpl <- GULDexpl %>% relocate(Notes, .after = endpointlat.bin)
GULDexpl <- GULDexpl %>% relocate(PITID, .after = FishID)
GULDexpl <- GULDexpl %>% relocate(endpointlat.bin, .after = endpointlat)
GULDexpl <- GULDexpl %>% relocate(emergelat.bin, .after = emergelat)


#Writing dataset
write.csv(GULDexpl, "~/trophicpersonalities_A/Data_Guldborgsund/GULD_EXPLdat_processed.csv")



#### #### 


