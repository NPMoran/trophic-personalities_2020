# Title: Behavioural and trophic variation within a well-established invasive round goby population 
# Author: Nicholas Moran
# Date: March 2023



#### 1. Preliminary data processing ####
Sys.setenv(LANG = "en")
#Loading required packages- 
library(dplyr); library(ggplot2); library(ggpubr)
#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


## 1.1. Physical data preparation ----

#Adding FishIDs and additional data to the dataframe
#  - FishID, InfectionScore, TL data taken from data from fish tagging (collected 19/06/2020)
GULD_fish <- read.csv("~/trophic-personalities_2020/dat_fish/GULDFish_19062020.csv")
GULD_fish <- select(GULD_fish, -c(TankID, Notes, Sex))

# - Sex and mass taken from pre-trial assessments (collected 24/06/2020)
GULD_fish2 <- read.csv("~/trophic-personalities_2020/dat_fish/GULDFish_25062020.csv")
GULD_fish2 <- select(GULD_fish2, -c(TankID, PITID, ReprodState, Notes))
GULD_physdat <- merge(GULD_fish, GULD_fish2, by = 'FishID', all.x = TRUE)
nrow(GULD_physdat)
#Initially tagged/prepared number of fish = 55


#Checking physical variables of tagged fish
# - Sex
GULD_physdat$Sex[GULD_physdat$Sex == "m(j)"] <- "m"
n_distinct(subset(GULD_physdat, Sex == 'f')) #n = 8
n_distinct(subset(GULD_physdat, Sex == 'm')) #n = 41
n_distinct(subset(GULD_physdat, Sex == 'j')) #n = 1, with 5 NAs

# - TL
summary(GULD_physdat$TL) #range 8.0 - 17.3 cm
GULD_physdat$Weight <- as.numeric(GULD_physdat$Weight) 
cor.test(GULD_physdat$TL, as.numeric(GULD_physdat$Weight))
#TL and Weight also strongly correlated, so only TL used here

# - Condition factor (using fulton's condition factor)
GULD_physdat$ConditionFactor <- 100* ((GULD_physdat$Weight) / ((GULD_physdat$TL)^3))
cor.test(GULD_physdat$TL, GULD_physdat$ConditionFactor, method = "pearson") #no sig correlation between fulton's and TL, but suggests there may be a positive association.

# - Condition factor v2 (using population specific coefficient)
#Looking for the exponent of the log-log frunction to calculate a condition factor
summary(lm(log(Weight) ~ log(TL), GULD_physdat)) 
#2.9158 to be used as the coefficient
GULD_physdat$CondManual <- (100*(GULD_physdat$Weight/(GULD_physdat$TL^2.9158)))
cor.test(GULD_physdat$TL, GULD_physdat$CondManual, method = "pearson") #no correlation between our factor and and TL

## - InfectionScore
#summary(GULD_physdat$InfectionScore) 
##Score of 1-3: 
##1 = 0 - 10% of fins with ectoparasite/fungal infection
##2 = 10 - 50% of fins with ectoparasite/fungal infection
##3 = 50 - 100% of fins with ectoparasite/fungal infection
#cor.test(GULD_physdat$InfectionScore, GULD_physdat$CondManual, method = "spearman") #marginally sig correlation between infection score and condition, so using only condition
GULD_physdat <- select(GULD_physdat, -c(InfectionScore, Weight))

write.csv(GULD_physdat, "~/trophic-personalities_2020/dat_fish/GULD_physdat_processed.csv", row.names = FALSE)


#Excluding fish from behavioural analysis -
# - 8 fish the dies prior to the start of behavioural trials
# (G01, G03, G15, G16, G33, G37, G43, G50)
GULD_physdat_behav <- subset(GULD_physdat, FishID != "G01") #PITID 2068
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G03") #PITID 2221
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G15") #PITID 1412
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G16") #PITID 2132
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G33") #PITID 2246
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G37") #PITID 1259
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G43") #PITID 2179
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G50") #PITID 2179
nrow(GULD_physdat_behav) #47 fish began behavioural trials

# - 4 fish excluded from all behavioural analysis after injuries found in week 1 post-trial inspections.
# (G08, G23, G32, G47)
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G08") #PITID 2191
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G23") #PITID 2165
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G32") #PITID 2155
GULD_physdat_behav <- subset(GULD_physdat_behav, FishID != "G47") #PITID 1492
nrow(GULD_physdat_behav) #43 fish with some behavioural data 

# - 1 additional fish, (G21, PITID 2038) to be excluded from week 2 trials due to a wound. 


#Comparing physical characteristics of fish in behav trials
# - Sex
n_distinct(subset(GULD_physdat_behav, Sex == 'f')) #n = 6
n_distinct(subset(GULD_physdat_behav, Sex == 'm')) #n = 37
# female to male ratio = 0.1621622 (at tagging the ratio was 0.195122, note this doesn't include 1x juv and 5 NAs tha dies before sexing)

# - TL
summary(GULD_physdat_behav$TL) #mean 13.68 [range 9.5 - 17.3] (at tagging mean 13.19 [8.0 - 17.3])

# - Condition factor v2 (using population specific coefficient)
summary(GULD_physdat$CondManual) #mean 1.601 [range 1.31 - 1.87] (at tagging mean 1.587 [1.265 - 1.888])




## 1.2. Activity data preparation ----

#Loading required dataset-
GULDact <- read.csv("~/trophic-personalities_2020/dat_behaviour/GULD_ACTdat.csv", strip.white = TRUE)
labels(GULDact)
GULDact <- subset(GULDact, PITID != 'NOFISH') #excluding rows with no fish
nrow(GULDact) #124 trials
n_distinct(GULDact$PITID) #data for 47 fish


#Re-creating the unique id for each trial-
GULDact$UniqueID <- paste(GULDact$TrialType, GULDact$TrialDay, sep = "")
GULDact$UniqueID <- paste(GULDact$UniqueID, GULDact$TrialRound, sep = "_")
GULDact$UniqueID <- paste(GULDact$UniqueID, GULDact$ArenaID, sep = "")


#Calculating edge use variables- 
# - centrescore: calculated from the proportion of time spend in 25mm width zones from the edge of the arena
GULDact$centrescore <- ((1*(GULDact$centre_0_25))+(2*GULDact$centre_25_50)+(3*GULDact$centre_50_75)+(4*GULDact$centre_75_100)+(5*GULDact$centre_100_))/
  (GULDact$centre_0_25+GULDact$centre_25_50+GULDact$centre_50_75+GULDact$centre_75_100+GULDact$centre_100_)
# - centrescore2: recalculated centre score so that it is roughly proportional to the average distance from the wall
# (for time spent in each 2.5cm edge zone, distance taken as minimum - maximum distance from edge in each zone)
GULDact$centrescore2 <- ((1.25*(GULDact$centre_0_25))+(3.75*GULDact$centre_25_50)+(6.25*GULDact$centre_50_75)+(8.75*GULDact$centre_75_100)+(13.0625*GULDact$centre_100_))/
  (GULDact$centre_0_25+GULDact$centre_25_50+GULDact$centre_50_75+GULDact$centre_75_100+GULDact$centre_100_)
# - centretime50: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (50-75mm, 75-100mm, 100+mm)
GULDact$centretime50 <- (GULDact$centre_50_75+GULDact$centre_75_100+GULDact$centre_100_)/30
# - centretime75: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (75-100mm, 100+mm)
GULDact$centretime75 <- (GULDact$centre_75_100+GULDact$centre_100_)/30
# - centretime100: time spent >50mm from the edge of the arena, calculated from the pixels visible in zones (75-100mm, 100+mm)
GULDact$centretime100 <- (GULDact$centre_100_)/30



## 1.3. Activity, ToxTrac calibration check ----

#Calibration pixel/mm ratios are calculated manually for each arena-
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
#Expected pattern, with higher ratio in central arenas and lower at edges. Effect more on horiz ratio than vertical


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
#Calibration successful within the acceptable range (i.e. tracking arena +- 1 cm from actual measured arena dimensions)


## 1.4. Activity, ToxTrac settings ----
#Project Initials Settings: 
#  - Start at (min/s)- 5:00, Finish at(min/s)- 25:00 
#  - Fill Temp. Holes, Max Size 25 Frames
#  - Fill Spacial Holes, Max Size 25 Frames
#  - Fill Extremes

#Statistics Advanced Options:
#  - Mob min. speed- 5mm/s (originally set to 1mm/s)
#  - Frozen event. Max. Dist- 25mm (originally set to 5mm)
#  - Frozen Event Min. Time- 3s
#  - Transitions time Int.- 7s (not used)

#Tracking Settings: Use defaults

#Statistics Advanced Options
# - checking key settings by making stepwise adjustments to settings to visualize how this influences response vars)
citation()
# - Mob min. speed- 5mm/s (originally set to 1mm/s)
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

# - Frozen event. Max. Dist- 25mm (originally set to 5mm)
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


## 1.5. Activity, database formatting ----
nrow(GULDact) #124 included trials
n_distinct(GULDact$PITID) #data for 47 fish

#Combining with the physical data removes 4 fish excluded from behavioural analysis
GULDact_processed <- merge(GULD_physdat_behav, GULDact, by = 'PITID', all.x = FALSE)

#Excluding one fish from week 2 found with a new injury (week 1 data still useable)
GULDact_processed <- subset(GULDact_processed, UniqueID != "ACT2_5A") #PITID 2038

#Summary data for fish in trials
nrow(GULDact_processed) #118 included trials
n_distinct(GULDact_processed$PITID) #data for 43 fish
nrow(subset(GULDact_processed, TrialDay == 1)) #Week 1 = 43 fish 
nrow(subset(GULDact_processed, TrialDay == 2)) #Week 2 = 41 fish
nrow(subset(GULDact_processed, TrialDay == 3)) #Week 3 = 34 fish

#Reorganising dataframe
labels(GULDact_processed)
# - removing variables not used in analysis
GULDact_processed <- select(GULDact_processed, -c(Weight_trial, ImageJ_width_pixel, ImageJ_height_pixel, ppmm_x_calc, ppmm_y_calc, width_pixel, 
                              height_pixel, width_mm, height_mm, arenaarea_m2, visibilityrate, timefrozen_tot.raw, 
                              timefrozen_ave.raw, timefrozen_ave, offset, centre_0_25, centre_25_50, centre_50_75, 
                              centre_75_100, centre_100_, Notes))

GULDact_processed <- GULDact_processed %>% relocate(UniqueID, .after = ArenaID)
GULDact_processed <- GULDact_processed %>% relocate(ConditionFactor, .after = TL)
GULDact_processed <- GULDact_processed %>% relocate(CondManual, .after = ConditionFactor)
GULDact_processed <- GULDact_processed %>% relocate(PITID, .after = FishID)

#Writing dataset
write.csv(GULDact_processed, "~/trophic-personalities_2020/dat_behaviour/GULD_ACTdat_processed.csv", row.names = FALSE)




## 1.5. Exploration data preparation ----
# Variables of interest
# in: emergelat
#     endpointlat
#     endpointspeed
#     refugereturnlat


#Loading required dataset-
GULDexpl <- read.csv("~/trophic-personalities_2020/dat_behaviour/GULD_EXPLdat.csv", strip.white = TRUE)
GULDexpl <- subset (GULDexpl, PITID != 'NOFISH') #excluding rows with no fish
GULDexpl <- subset(GULDexpl, GULDexpl$PITID != "2044/1395") #excluding trial where fish escaped arena
nrow(GULDexpl) #116 trials
n_distinct(GULDexpl$PITID) #data for 46 fish

#Re-creating the unique id for each trial-
GULDexpl$UniqueID <- paste(GULDexpl$TrialType, GULDexpl$TrialDay, sep = "")
GULDexpl$UniqueID <- paste(GULDexpl$UniqueID, GULDexpl$TrialRound, sep = "_")
GULDexpl$UniqueID <- paste(GULDexpl$UniqueID, GULDexpl$ArenaID, sep = "")

#Combining with the physical data removes 3 fish excluded from behavioural analysis
GULDexpl_processed <- merge(GULD_physdat_behav, GULDexpl, by = 'PITID', all.x = FALSE)

#Excluding one fish from week 2 found with a new injury (week 1 data still useable)
GULDexpl_processed <- subset(GULDexpl_processed, UniqueID != "EXPL2_4A") #G41, PITID = 2133

#Excluding one fish from week 2 found with a new injury (week 1 data still useable)
GULDexpl_processed <- subset(GULDexpl_processed, UniqueID != "EXPL3_5D") #G40, PITID = 2101

nrow(GULDexpl_processed) #111 trials
n_distinct(GULDexpl_processed$PITID) #data for 43 fish


## 1.6. Exploration, database formatting ----
#Converting heavily bimodal variables (endpointlat, emergelat) into binary data
# - visualising distributions
#emergelat: (s) latency to emerge from the shelter
ggplot(GULDexpl_processed) + aes(x = emergelat) + geom_histogram(color="black", fill="lightblue", binwidth = 170) + simpletheme 
ggqqplot(GULDexpl_processed$emergelat) 

#endpointlat: (s) latency to explore to the endpoint from trial start time
ggplot(GULDexpl_processed) + aes(x = endpointlat) + geom_histogram(color="black", fill="lightblue", binwidth = 170) + simpletheme 
ggqqplot(GULDexpl_processed$endpointlat) 

# - conversions to binomial using median emergence time as conversion point (1 <= median, 0 > median)
median(GULDexpl_processed$emergelat) #median = 49
GULDexpl1 <- subset(GULDexpl_processed, emergelat <= 49)
GULDexpl2 <- subset(GULDexpl_processed, emergelat >= 50)
GULDexpl1$emergelat.bin <- 1
GULDexpl2$emergelat.bin <- 0
GULDexpl_processed <- rbind(GULDexpl1, GULDexpl2)

median(GULDexpl_processed$endpointlat) #median = 160
GULDexpl1 <- subset(GULDexpl_processed, endpointlat <= 160)
GULDexpl2 <- subset(GULDexpl_processed, endpointlat >= 161)
GULDexpl1$endpointlat.bin <- 1
GULDexpl2$endpointlat.bin <- 0
GULDexpl_processed <- rbind(GULDexpl1, GULDexpl2)

#Reorganising dataframe
labels(GULDexpl_processed)
# - removing variables not used in analysis
GULDexpl_processed <- select(GULDexpl_processed, -c(refugereturntime, endpointtime, emergetime, Notes))

GULDexpl_processed <- GULDexpl_processed %>% relocate(UniqueID, .after = ArenaID)
GULDexpl_processed <- GULDexpl_processed %>% relocate(ConditionFactor, .after = TL)
GULDexpl_processed <- GULDexpl_processed %>% relocate(CondManual, .after = ConditionFactor)
GULDexpl_processed <- GULDexpl_processed %>% relocate(PITID, .after = FishID)
GULDexpl_processed <- GULDexpl_processed %>% relocate(endpointlat.bin, .after = endpointlat)
GULDexpl_processed <- GULDexpl_processed %>% relocate(emergelat.bin, .after = emergelat)

#Writing dataset
write.csv(GULDexpl_processed, "~/trophic-personalities_2020/dat_behaviour/GULD_EXPLdat_processed.csv", row.names = FALSE)


