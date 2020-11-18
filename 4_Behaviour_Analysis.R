# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG = "en")
library(dplyr)
library(ggpubr)

library(lme4)
library(rptR)
library(brms)
library(ggplot2)



##3.3 Preliminary ACT Variance Analysis ----
GULD_ACT <- read.csv("~/trophicpersonalities_GULD/4_Behaviour_Analysis/GULDACTdat_18112020.csv")
nrow(GULD_ACT) #136 rows


#Re-creating the unique id for each trial 
GULD_ACT$UniqueID <- paste(GULD_ACT$TrialType, GULD_ACT$TrialDay, sep = "")
GULD_ACT$UniqueID <- paste(GULD_ACT$UniqueID, GULD_ACT$TrialRound, sep = "_")
GULD_ACT$UniqueID <- paste(GULD_ACT$UniqueID, GULD_ACT$ArenaID, sep = "")


#Excluding trials with no fish
GULD_ACT <- subset(GULD_ACT, GULD_ACT$PITID != "NOFISH")
nrow(GULD_ACT) #124 trials
n_distinct(GULD_ACT$PITID) #for 48 fish


#Excluding some trials due to: injuries found on individuals on post-trial examination
#Injuries likely occured post-arrival in lab, so excluded to avoid possible effects of 
GULD_ACT.excl <- subset(GULD_ACT, UniqueID != "ACT1_1C")
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT1_3F")
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT1_4D")
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT1_5C")
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT2_5A")
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT2_5B")


#General theme for ggplots
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"),
                       axis.text.x = element_text(size = 10, colour = "black"), 
                       panel.background = element_rect(fill = "white"),
                       axis.title.y  = element_text(size=12, vjust = 2),
                       axis.title.x  = element_text(size=12, vjust = 0.1),
                       panel.border = element_rect(colour = "black", fill=NA, size = 1))


#Assessing distributions
#avespeed_tot: the average speed of the individual accross the full trial period
ggplot(GULD_ACT.excl) + aes(x = avespeed_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(GULD_ACT.excl$avespeed_tot)
shapiro.test(GULD_ACT.excl$avespeed_tot) #departure from normality non-significant, just

#avespeed_mob: the average speed of the individual excluding periods when it was immobile
ggplot(GULD_ACT.excl) + aes(x = avespeed_mob) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(GULD_ACT.excl$avespeed_mob)
shapiro.test(GULD_ACT.excl$avespeed_mob) #significant departure from normality, four outliers at the very low end

#aveacceler: 
ggplot(GULD_ACT.excl) + aes(x = aveacceler) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(GULD_ACT.excl$avespeed_mob)
shapiro.test(GULD_ACT.excl$avespeed_mob) #non-normal, four outliers at the very low end








#Preliminary data exploration
GULD_ACT_behmod.avespeed_tot <- lmer(avespeed_tot ~ (1|PITID), data=GULD_ACT)
GULD_ACT_behmod.avespeed_tot
confint(GULD_ACT_behmod.avespeed_tot)
plot(GULD_ACT_behmod.avespeed_tot)
rpt(avespeed_tot ~ (1 | PITID), grname = "PITID", data = GULD_ACT, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT_behmod.avespeed_mob <- lmer(avespeed_mob ~ (1|PITID), data=GULD_ACT)
GULD_ACT_behmod.avespeed_mob
confint(GULD_ACT_behmod.avespeed_mob)
rpt(avespeed_mob ~ (1 | PITID), grname = "PITID", data = GULD_ACT, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT_behmod.aveacceler <- lmer(aveacceler ~ (1|PITID), data=GULD_ACT)
GULD_ACT_behmod.aveacceler
confint(GULD_ACT_behmod.aveacceler)
rpt(aveacceler ~ (1 | PITID), grname = "PITID", data = GULD_ACT, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT_behmod.propmoving <- lmer(propmoving ~ ArenaID + (1|PITID), data=GULD_ACT)
GULD_ACT_behmod.propmoving
confint(GULD_ACT_behmod.propmoving)
rpt(propmoving ~ (1 | PITID), grname = "PITID", data = GULD_ACT, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT_behmod.dist <- lmer(dist ~ (1|PITID), data=GULD_ACT)
GULD_ACT_behmod.dist
confint(GULD_ACT_behmod.dist)
rpt(dist ~ (1 | PITID), grname = "PITID", data = GULD_ACT, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT_behmod.timefrozen_tot <- lmer(timefrozen_tot ~ (1|PITID), data=GULD_ACT)
GULD_ACT_behmod.timefrozen_tot
confint(GULD_ACT_behmod.timefrozen_tot)
rpt(timefrozen_tot ~ (1 | PITID), grname = "PITID", data = GULD_ACT, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT_behmod.timefrozen_ave <- lmer(timefrozen_ave ~ (1|PITID), data=GULD_ACT)
GULD_ACT_behmod.timefrozen_ave
confint(GULD_ACT_behmod.timefrozen_ave)
rpt(timefrozen_ave ~ (1 | PITID), grname = "PITID", data = GULD_ACT, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT_behmod.centretime <- lmer(centretime ~ (1|PITID), data=GULD_ACT)
GULD_ACT_behmod.centretime
confint(GULD_ACT_behmod.centretime)
rpt(centretime ~ (1 | PITID), grname = "PITID", data = GULD_ACT, datatype = "Gaussian", 
    nboot = 100, npermut = 0)



nrow(GULD_ACT.excl)
n_distinct(GULD_ACT.excl$PITID) #data for 44 fish

hist(GULD_ACT.excl$avespeed_tot)
hist(GULD_ACT.excl$avespeed_mob)
hist(GULD_ACT.excl$aveacceler)
hist(GULD_ACT.excl$propmoving)
hist(GULD_ACT.excl$dist)
hist(GULD_ACT.excl$timefrozen_tot)
hist(GULD_ACT.excl$timefrozen_ave)
hist(GULD_ACT.excl$centretime)


GULD_ACT.excl_behmod.avespeed_tot <- lmer(avespeed_tot ~ TrialDay + (1|PITID), data=GULD_ACT.excl)
GULD_ACT.excl_behmod.avespeed_tot
confint(GULD_ACT.excl_behmod.avespeed_tot)
rpt(avespeed_tot ~ (1 | PITID), grname = "PITID", data = GULD_ACT.excl, datatype = "Gaussian", 
    nboot = 100, npermut = 0)


GULD_ACT.excl_behmod.avespeed_mob <- lmer(avespeed_mob ~ (1|PITID), data=GULD_ACT.excl)
GULD_ACT.excl_behmod.avespeed_mob
confint(GULD_ACT.excl_behmod.avespeed_mob)
rpt(avespeed_mob ~ (1 | PITID), grname = "PITID", data = GULD_ACT.excl, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT.excl_behmod.aveacceler <- lmer(aveacceler ~ (1|PITID), data=GULD_ACT.excl)
GULD_ACT.excl_behmod.aveacceler
confint(GULD_ACT.excl_behmod.aveacceler)
rpt(aveacceler ~ (1 | PITID), grname = "PITID", data = GULD_ACT.excl, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT.excl_behmod.propmoving <- lmer(propmoving ~ (1|PITID), data=GULD_ACT.excl)
GULD_ACT.excl_behmod.propmoving
confint(GULD_ACT.excl_behmod.propmoving)
rpt(propmoving ~ (1 | PITID), grname = "PITID", data = GULD_ACT.excl, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT.excl_behmod.dist <- lmer(dist ~ (1|PITID), data=GULD_ACT.excl)
GULD_ACT.excl_behmod.dist
confint(GULD_ACT.excl_behmod.dist)
rpt(dist ~ (1 | PITID), grname = "PITID", data = GULD_ACT.excl, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT.excl_behmod.timefrozen_tot <- lmer(timefrozen_tot ~ (1|PITID), data=GULD_ACT.excl)
GULD_ACT.excl_behmod.timefrozen_tot
confint(GULD_ACT.excl_behmod.timefrozen_tot)
rpt(timefrozen_tot ~ (1 | PITID), grname = "PITID", data = GULD_ACT.excl, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT.excl_behmod.timefrozen_ave <- lmer(timefrozen_ave ~ (1|PITID), data=GULD_ACT.excl)
GULD_ACT.excl_behmod.timefrozen_ave
confint(GULD_ACT.excl_behmod.timefrozen_ave)
rpt(timefrozen_ave ~ (1 | PITID), grname = "PITID", data = GULD_ACT.excl, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

GULD_ACT.excl_behmod.centretime <- lmer(centretime ~ (1|PITID), data=GULD_ACT.excl)
GULD_ACT.excl_behmod.centretime
confint(GULD_ACT.excl_behmod.centretime)
rpt(centretime ~ (1 | PITID), grname = "PITID", data = GULD_ACT.excl, datatype = "Gaussian", 
    nboot = 100, npermut = 0)


##3.3 Preliminary EXPL Variance Analysis ----
GULD_EXPL <- read.csv("~/trophicpersonalities_GULD/3_Behaviour_Analysis/GULDEXPLdat_15102020.csv")
#Re-creating unique id for each trial 
GULD_EXPL$UniqueID <- paste(GULD_EXPL$TrialType, GULD_EXPL$TrialDay, sep = "")
GULD_EXPL$UniqueID <- paste(GULD_EXPL$UniqueID, GULD_EXPL$TrialRound, sep = "_")
GULD_EXPL$UniqueID <- paste(GULD_EXPL$UniqueID, GULD_EXPL$ArenaID, sep = "")

#Assessing distributions
hist(GULD_EXPL$emergelat)
hist(GULD_EXPL$endpointlat)
hist(GULD_EXPL$refugereturnlat)

nrow(GULD_EXPL) #125 rows
GULD_EXPL <- subset(GULD_EXPL, GULD_EXPL$PITID != "NOFISH")
GULD_EXPL <- subset(GULD_EXPL, GULD_EXPL$PITID != "2044/1395") #excluding trial where fish escaped arena
nrow(GULD_EXPL) #116 trials
n_distinct(GULD_EXPL$PITID) #for 46 fish


GULD_EXPL.excl_behmod.emergelat <- lmer(emergelat ~ (1|PITID), data=GULD_EXPL)
GULD_EXPL.excl_behmod.emergelat
confint(GULD_EXPL.excl_behmod.emergelat)
rpt(emergelat ~ (1 | PITID), grname = "PITID", data = GULD_EXPL, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

#Converting to binomial for rpt estimate
GULD_EXPLa <- subset(GULD_EXPL, emergelat != 2700)
GULD_EXPLb <- subset(GULD_EXPL, emergelat == 2700)
GULD_EXPLa$emergebin <- 1
GULD_EXPLb$emergebin <- 0
GULD_EXPL <- rbind(GULD_EXPLa, GULD_EXPLb)
nrow(GULD_EXPLb)
n_distinct(GULD_EXPLb$PITID)

rpt(emergebin ~ (1 | PITID), grname = "PITID", data = GULD_EXPL, datatype = "Binary", 
    nboot = 100, npermut = 0)





