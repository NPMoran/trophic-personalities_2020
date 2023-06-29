# Title: Behavioural and trophic variation within a well-established invasive round goby population 
# Author: Nicholas Moran
# Date: June 2023



#### 3. Stable isotope analysis ####
Sys.setenv(LANG = "en")
#Loading required packages- 
library(dplyr); library(lme4); library(lmerTest); library(rptR); library(data.table)
library(ggplot2); library(ggpubr); library(RColorBrewer)
library(MixSIAR)

#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


#Importing and sorting datasets
GULD_SIA1 <- read.csv("~/trophic-personalities_2020/dat_stableisotope/GULD_SIAbatch1dat.csv")
GULD_SIA2 <- read.csv("~/trophic-personalities_2020/dat_stableisotope/GULD_SIAbatch2dat.csv")
GULD_SIA3 <- read.csv("~/trophic-personalities_2020/dat_stableisotope/GULD_SIAbatch3dat.csv")
nrow(GULD_SIA1); nrow(GULD_SIA2); nrow(GULD_SIA3) 
labels(GULD_SIA1); labels(GULD_SIA2); labels(GULD_SIA3) 
GULD_SIAfull <- rbind(GULD_SIA1, GULD_SIA2, GULD_SIA3)
GULD_SIAfull <- subset(GULD_SIAfull, sortID != 'NA')
nrow(GULD_SIAfull); labels(GULD_SIAfull)




