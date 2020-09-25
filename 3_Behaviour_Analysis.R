# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG = "en")



#3. Behavioural Analysis ----

##3.1 Calibration of Toxtrac ----
#Requires the vertical and horizontal pixel to distance conversion to be known
#Calibration pixel/mm ratios are calculated manually for each arena


#Defining 8 arenas used in ACT trials
cali_arena <- NULL
cali_arena$arenaID <- c("A","B","C","D","E","F","G","H")

#Finding pixel/cm ratio within arenas using ImageJ (Schneider, C. A.; Rasband, W. S. & Eliceiri, K. W. (2012), "NIH Image to ImageJ: 25 years of image analysis", Nature methods 9(7): 671-675)
#-using Calibration image with 3x5 grid of 50mm x 50mm black and white squared placed in each arena.
#-measurements taken at each line on the grid, values optained from Analyze/Set Scale, entering known distances
#-values are in pixels/cm (note: both calibration images and videos are all 1280 x 720)
cali_arena$vertical1 <- c(6.0905, 6.0653, 6.1032, 6.0001, 0, 0, 0, 0)
cali_arena$vertical2 <- c(6.0943, 6.1431, 6.1833, 5.9601, 0, 0, 0, 0)
cali_arena$vertical3 <- c(6.0916, 6.1840, 6.1042, 6.0412, 0, 0, 0, 0)
cali_arena$vertical4 <- c(6.0555, 6.1450, 6.1450, 6.0400, 0, 0, 0, 0)
cali_arena$horiz1 <- c(6.1012, 6.1828, 6.1178, 5.8670, 0, 0, 0, 0)
cali_arena$horiz2 <- c(6.0916, 6.3158, 6.0502, 5.8004, 0, 0, 0, 0)
cali_arena$horiz3 <- c(5.9062, 6.1167, 6.1828, 6.0004, 0, 0, 0, 0)
cali_arena$horiz4 <- c(5.8440, 6.1843, 6.1163, 5.9337, 0, 0, 0, 0)
cali_arena$horiz5 <- c(5.9065, 6.1167, 6.0499, 5.8701, 0, 0, 0, 0)
cali_arena$horiz6 <- c(5.9687, 5.9173, 6.0513, 5.7337, 0, 0, 0, 0)

#-calculating the average for each arena, and converting to pixels/mm for input into ToxTrac 
cali_arena$vertical.ppmm <- 0.1* (cali_arena$vertical1 + cali_arena$vertical2 + cali_arena$vertical3 + cali_arena$vertical4)/4
cali_arena$horiz.ppmm <- 0.1* (cali_arena$horiz1 + cali_arena$horiz2 + cali_arena$horiz3 + cali_arena$horiz4 + cali_arena$horiz5 + cali_arena$horiz6)/6
#sverage of horizontal and vertical pixel/mm scale entered into Toxtrac/Calibration/Camera matrix for each arena

#Calibration images taken during 3rd Activty Trial, ACT3, so minor adjustments made for calibrations of ACT2, ACT1
#(Note: position of camera and boxes is consistent between all trials, this is adjausting for different levels of zoom)
cali_trial <- NULL
cali_trial$trialID <- c("ACT1","ACT2","ACT3")
#
cali_trial$vertical1 <- ( 0, 0, 0)
cali_trial$vertical2 <- ( 0, 0, 0)
cali_trial$vertical3 <- ( 0, 0, 0)
cali_trial$vertical4 <- ( 0, 0, 0)
cali_trial$horiz1 <- ( 0, 0, 0)
cali_trial$horiz2 <- ( 0, 0, 0)
cali_trial$horiz3 <- ( 0, 0, 0)
cali_trial$horiz4 <- ( 0, 0, 0)
cali_trial$horiz5 <- ( 0, 0, 0)
cali_trial$horiz6 <- ( 0, 0, 0)


##### Fish tracking and Processing ----

