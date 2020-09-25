# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG = "en")


#4. Compiling individual behavioural and isotope datasets

### 4.1 GULD_Fish state data ----

dat_prelim <- read.csv('GULDFish_19062020.csv', strip.white = TRUE)
dat_ACT1 <- read.csv('GULDFish_25062020.csv', strip.white = TRUE)
data_GULDfish <- merge(dat_prelim, dat_ACT1, all.x = TRUE)


