# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#Guldborgsund 1. Preliminary data processing and analysis ----


Sys.setenv(LANG = "en")


#Loading required packages- 
library(dplyr); library(ggplot2); library(ggpubr);
library(lme4); library(lmerTest); library(rptR); library(car)


#Loading required datasets-
GULDact <- read.csv("~/trophicpersonalities_A/Data_Guldborgsund/GULD_ACTdat.csv", strip.white = TRUE)
labels(GULDact)
GULDact <- subset (GULDact, PITID != 'NOFISH')
nrow(GULDact) #124 trials


#Re-creating the unique id for each trial-
GULDact$UniqueID <- paste(GULDact$TrialType, GULDact$TrialDay, sep = "")
GULDact$UniqueID <- paste(GULDact$UniqueID, GULDact$TrialRound, sep = "_")
GULDact$UniqueID <- paste(GULDact$UniqueID, GULDact$ArenaID, sep = "")


#Creating a general theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"),
                       axis.text.x = element_text(size = 10, colour = "black"), 
                       panel.background = element_rect(fill = "white"),
                       axis.title.y  = element_text(size=12, vjust = 2),
                       axis.title.x  = element_text(size=12, vjust = 0.1),
                       panel.border = element_rect(colour = "black", fill=NA, size = 1))



### G.1.1 Calibration of ToxTrac (ACT trials) ----


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



### G.1.2 Optimization of ToxTrac (ACT trials) ----
#Using same settings as Karrebaek fish to maximize comparability across experiments

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
# Statistics Advanced Options:
#  Mob min. speed- 10mm/s (originally set to 1mm/s)


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



### G.1.3 Preliminary data exploration (ACT trials) ----

# Variables of interest
# in: avespeed_tot        
#     avespeed_mob
#     aveaccler
#     propmoving          
#     dist
#     timefrozen_tot
#     centretime50        
#     centretime75
#     centretime100       
#     centrescore


#Calculating centre use variables:
# - centretime50: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (50-75mm, 75-100mm, 100+mm)
GULDact$centretime50 <- (GULDact$centre_50_75+GULDact$centre_75_100+GULDact$centre_100_)/30

# - centretime75: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (75-100mm, 100+mm)
GULDact$centretime75 <- (GULDact$centre_75_100+GULDact$centre_100_)/30

# - centretime100: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (75-100mm, 100+mm)
GULDact$centretime100 <- (GULDact$centre_100_)/30

# - centrescore: calculated from the proportion of time spend in 25mm widt zones from the edge of the arena
GULDact$centrescore <- ((1*(GULDact$centre_0_25))+(2*GULDact$centre_25_50)+(3*GULDact$centre_50_75)+(4*GULDact$centre_75_100)+(5*GULDact$centre_100_))/
  (GULDact$centre_0_25+GULDact$centre_25_50+GULDact$centre_50_75+GULDact$centre_75_100+GULDact$centre_100_)



#visualizing variation across the Trial days
GULDact$TrialDay[GULDact$TrialDay == 1] <- "Trial 1"
GULDact$TrialDay[GULDact$TrialDay == 2] <- "Trial 2"
GULDact$TrialDay[GULDact$TrialDay == 3] <- "Trial 3"
GULDact$TrialDay <- ordered(GULDact$TrialDay, levels = c("Trial 1","Trial 2","Trial 3"))
ggplot(GULDact, aes(x = TrialDay, y = avespeed_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = avespeed_mob)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = aveacceler)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = propmoving)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = dist)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = timefrozen_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = centretime50)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = centretime75)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = centretime100)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialDay, y = centrescore)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#Some reduction in activity over trials

#visualizing variation across the Trial arenas
ggplot(GULDact, aes(x = ArenaID, y = avespeed_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = avespeed_mob)) + geom_boxplot(fill = 'deepskyblue') + simpletheme
ggplot(GULDact, aes(x = ArenaID, y = aveacceler)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = propmoving)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = dist)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = timefrozen_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = centretime50)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = centretime75)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = centretime100)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = ArenaID, y = centrescore)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#no obvious patterns across arenas

#visualizing variation across the Trial Rounds
GULDact$TrialRound <- as.factor(GULDact$TrialRound)

ggplot(GULDact, aes(x = TrialRound, y = avespeed_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = avespeed_mob)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = aveacceler)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = propmoving)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = dist)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = timefrozen_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = centretime50)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = centretime75)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = centretime100)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TrialRound, y = centrescore)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#no obvious patterns across trial rounds

#visualizing variation across the holding tanks
ggplot(GULDact, aes(x = TankID, y = avespeed_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = avespeed_mob)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = aveacceler)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = propmoving)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = dist)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = timefrozen_tot)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = centretime50)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = centretime75)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = centretime100)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
ggplot(GULDact, aes(x = TankID, y = centrescore)) + geom_boxplot(fill = 'deepskyblue') + simpletheme 
#no obvious patterns across trial rounds






