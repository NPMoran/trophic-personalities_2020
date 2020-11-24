# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG = "en")
library(dplyr)


#3. Toxtrac Calibration and Optimization ----

### 3.1 ACT_Calibration for Toxtrac ----
#Requires the vertical and horizontal pixel to distance conversion to be known
#Calibration pixel/mm ratios are calculated manually for each arena


#Defining 8 arenas used in ACT trials
cali_arena <- NULL
cali_arena$arenaID <- c("A","B","C","D","E","F","G","H")

#Calculating pixel/cm ratio within arenas using ImageJ 
#(Schneider, C. A.; Rasband, W. S. & Eliceiri, K. W. (2012), "NIH Image to ImageJ: 25 years of image analysis", Nature methods 9(7): 671-675)
# - using Calibration image with 3x5 grid of 50mm x 50mm black and white squared placed in each arena.
# - measurements taken at each line on the grid, values optained from Analyze/Set Scale, entering known distances
# - values are in pixels/cm (note: both calibration images and videos are all 1280 x 720)
cali_arena$vertical1 <- c(6.0905, 6.0653, 6.1032, 6.0001, 6.1370, 6.1783, 6.1612, 6.0847)
cali_arena$vertical2 <- c(6.0943, 6.1431, 6.1833, 5.9601, 6.0574, 6.2173, 6.2023, 5.9634)
cali_arena$vertical3 <- c(6.0916, 6.1840, 6.1042, 6.0412, 6.0971, 6.1777, 6.1612, 6.1264)
cali_arena$vertical4 <- c(6.0555, 6.1450, 6.1450, 6.0400, 6.0975, 6.1773, 6.2021, 6.1233)
cali_arena$horiz1 <- c(6.1012, 6.1828, 6.1178, 5.8670, 5.8845, 6.1504, 6.0681, 5.8682)
cali_arena$horiz2 <- c(6.0916, 6.3158, 6.0502, 5.8004, 5.9592, 6.2176, 6.0670, 6.0004)
cali_arena$horiz3 <- c(5.9062, 6.1167, 6.1828, 6.0004, 5.8845, 6.2845, 6.2004, 6.0015)
cali_arena$horiz4 <- c(5.8440, 6.1843, 6.1163, 5.9337, 6.0171, 6.1508, 6.2670, 5.9333)
cali_arena$horiz5 <- c(5.9065, 6.1167, 6.0499, 5.8701, 5.9499, 6.0839, 6.2000, 5.8667)
cali_arena$horiz6 <- c(5.9687, 5.9173, 6.0513, 5.7337, 5.9502, 6.2176, 6.2667, 5.9337)

# - calculating the average for each arena, and converting to pixels/mm for input into ToxTrac 
# - taken as mean of horizontal and vertical pixel/mm scale entered into Toxtrac/Calibration/Camera matrix for each arena
cali_arena$vertical.ppmm <- 0.1* (cali_arena$vertical1 + cali_arena$vertical2 + cali_arena$vertical3 + cali_arena$vertical4)/4
cali_arena$horiz.ppmm <- 0.1* (cali_arena$horiz1 + cali_arena$horiz2 + cali_arena$horiz3 + cali_arena$horiz4 + cali_arena$horiz5 + cali_arena$horiz6)/6


#Calibration images for each arena were taken during 3rd Activty Trial, ACT3, so minor adjustments made for calibrations of ACT2, ACT1
#(Note: position of camera and boxes is consistent between all trials, this is adjausting for different levels of zoom)
cali_trial <- NULL

#cali_trial$vertical1 <- (vertical1, vertical2, vertical3, vertical4, horiz1, horiz2, horiz3, horiz4, horiz5, horiz6)
cali_trial$ACT1 <- c(5.3624, 5.2802, 5.2406, 5.2014, 5.0000, 5.0004, 4.9351, 4.8617, 4.9374, 4.9351)
cali_trial$ACT2 <- c(5.8400, 5.9605, 5.8401, 5.7601, 5.2734, 5.4066, 5.3371, 5.3400, 5.2772, 5.2705)
cali_trial$ACT3 <- c(5.9686, 6.0085, 5.8867, 5.8849, 5.5369, 5.4037, 5.4703, 5.6036, 5.5337, 5.5400)
cali_trial<- as.data.frame(cali_trial)
cali_trial$ACT3toACT2 <- (cali_trial$ACT2)/(cali_trial$ACT3)
cali_trial$ACT3toACT1 <- (cali_trial$ACT1)/(cali_trial$ACT3)
mean(cali_trial$ACT3toACT2) #adjustment ratio between ACT3 and ACT2 calibration coefficient = 0.97279
mean(cali_trial$ACT3toACT1) #adjustment ratio between ACT3 and ACT1 calibration coefficient = 0.8932541


#Calculating ratio to adjust calibration coefficients for each arena in each trial
cali_full<-NULL
cali_full$arenaID <- c("A","B","C","D","E","F","G","H")
cali_full$ACT3.x <- cali_arena$horiz.ppmm
cali_full$ACT3.y <- cali_arena$vertical.ppmm
cali_full$ACT2.x <- 0.97279*(cali_full$ACT3.x)
cali_full$ACT2.y <- 0.97279*(cali_full$ACT3.y)
cali_full$ACT1.x <- 0.8932541*(cali_full$ACT3.x)
cali_full$ACT1.y <- 0.8932541*(cali_full$ACT3.y)
cali_full <- as.data.frame(cali_full)


#Database of calibration coefficients
write.csv(cali_full, "~/trophicpersonalities_GULD/3_Behaviour_ToxTracCalibration/Guld_toxtracmanualcalibration.csv")


### 3.2 ACT ToxTrac Tracking Data ----
# all videos trimmed to 25:00, staring from the point that all individuals are loaded
# trimmed using Microsoft Photos app

# Project Initials Settings:
#  Start at (min/s)- 5:00, Finish at(min/s)- 25:00 
#  Fill Temp. Holes, Max Size 25 Frames
#  Fill Spacial Holes, Max Size 25 Frames
#  Fill Extremes

# Tracking Settings: Use defaults

# Statistics Advanced Options:
#  Mob min. speed- 10mm/s (originally set to 1mm/s)
Settings_test <- NULL
Settings_test$minmobilityspeedset <- c(0, 1, 3, 6, 9, 12, 15, 18, 20) # Frozen event dist set to 10mm
Settings_test$mobrate_act1_1a <- c(1, 0.94, 0.81, 0.69, 0.62, 0.58, 0.54, 0.51, 0.49) #Relatively active fish
Settings_test$mobrate_act1_1c <- c(1, 0.74, 0.28, 0.11, 0.05, 0.02, 0.00, 0.00, 0.00) #Very little movement
Settings_test$mobratediff <- (Settings_test$mobrate_act1_1a - Settings_test$mobrate_act1_1c)
plot(Settings_test$minmobilityspeedset, Settings_test$mobrate_act1_1a) #Curve begins to flatten around 9mm
plot(Settings_test$minmobilityspeedset, Settings_test$mobrate_act1_1c) #Min 9mm/s needed to capture lack of movement
plot(Settings_test$minmobilityspeedset, Settings_test$mobratediff) #Set to 10mm/s capture difference between behaviours 

#  Frozen event. Max. Dist- 20mm (originally set to 5mm)
Settings_test <- NULL
Settings_test$frozenmaxdist <- c(0, 1, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30) #Mob min. speed set to 10mm/s
Settings_test$timefrozen_act1_1a <- c(10, 10, 10, 11, 14, 18, 41, 72, 143, 213, 251, 262) #Relatively active fish
Settings_test$timefrozen_act1_1c <- c(0, 0, 0, 0, 64, 259, 496, 767, 942, 1032, 1104, 1163) #Very little movement
Settings_test$timefrozendiff <- (Settings_test$timefrozen_act1_1c - Settings_test$timefrozen_act1_1a)
plot(Settings_test$frozenmaxdist, Settings_test$timefrozen_act1_1a) #Curve begins to flatten around 20mm
plot(Settings_test$frozenmaxdist, Settings_test$timefrozen_act1_1c) #Min 20mm/s needed to capture lack of movement
plot(Settings_test$frozenmaxdist, Settings_test$timefrozendiff) #Set to 20mm capture difference between behaviours 

#  Frozen Event Min. Time- 3s
#  Transitions time Int.- 7s (not used)

