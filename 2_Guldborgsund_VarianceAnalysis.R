# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



Sys.setenv(LANG = "en")
library(dplyr); library(ggpubr); library(ggplot2); library(lme4); library(rptR)



#2. Individual Behavioural Variance Analysis (GULD) ----

### 2.1 Activity Assay (ACT) ----
GULD_ACT <- read.csv("~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis/GULDACTdat_18112020.csv")
nrow(GULD_ACT) #136 rows


#Re-creating the unique id for each trial-
GULD_ACT$UniqueID <- paste(GULD_ACT$TrialType, GULD_ACT$TrialDay, sep = "")
GULD_ACT$UniqueID <- paste(GULD_ACT$UniqueID, GULD_ACT$TrialRound, sep = "_")
GULD_ACT$UniqueID <- paste(GULD_ACT$UniqueID, GULD_ACT$ArenaID, sep = "")


#Excluding trials with no fish-
GULD_ACT <- subset(GULD_ACT, GULD_ACT$PITID != "NOFISH")
nrow(GULD_ACT) #124 trials
n_distinct(GULD_ACT$PITID) #data for 47 fish


#Adding FishIDs to the dataframe
GULD_fish <- read.csv("~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis/GULDFish_19062020.csv")
GULD_fish <- select(GULD_fish, -c(TankID, Notes, Sex))
GULD_fish2 <- read.csv("~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis/GULDFish_25062020.csv")
GULD_fish2 <- select(GULD_fish2, -c(TankID, PITID, Mass_g, ReprodState, Notes))
GULD_fish <- merge(GULD_fish, GULD_fish2, by = 'FishID', all.x = TRUE)
GULD_ACT <- merge(GULD_fish, GULD_ACT, by = 'PITID', all.x = FALSE)
n_distinct(GULD_ACT$FishID) #data for 47 fish


#Excluding some trials due to: injuries found on individuals on post-trial examination-
#Injuries likely occured post-arrival in lab, so excluded to avoid possible behavioural effects 
GULD_ACT.excl <- subset(GULD_ACT, UniqueID != "ACT1_1C") #PITID 1492
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT1_3F") #PITID 2155
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT1_4D") #PITID 2165
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT1_5C") #PITID 2191
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT2_5A") #PITID 2038
GULD_ACT.excl <- subset(GULD_ACT.excl, UniqueID != "ACT2_5B") #PITID 2155

nrow(GULD_ACT.excl) #118 included trials
n_distinct(GULD_ACT.excl$FishID) #43 fish included in analysis


#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"),
                       axis.text.x = element_text(size = 10, colour = "black"), 
                       panel.background = element_rect(fill = "white"),
                       axis.title.y  = element_text(size=12, vjust = 2),
                       axis.title.x  = element_text(size=12, vjust = 0.1),
                       panel.border = element_rect(colour = "black", fill=NA, size = 1))


#Initial exploration of variable and assessing distributions-
#avespeed_tot: (mm/s) the average speed of the individual accross the full trial period
ggplot(GULD_ACT.excl) + aes(x = avespeed_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 4.5) + simpletheme 
ggqqplot(GULD_ACT.excl$avespeed_tot)
#approximately normal


#avespeed_mob: (mm/s) the average speed of the individual excluding periods when it was immobile
ggplot(GULD_ACT.excl) + aes(x = avespeed_mob) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(GULD_ACT.excl$avespeed_mob)
#marginal non-normality driven four potential outliers at the very low end


#aveacceler: (mm/s^2) average rate of acceleration accross the trial
ggplot(GULD_ACT.excl) + aes(x = aveacceler) + geom_histogram(color="black", fill="lightblue", binwidth = 14) + simpletheme 
ggqqplot(GULD_ACT.excl$aveacceler)
#minimal left skew


#propmoving: (proportional) proportion of time mobile
ggplot(GULD_ACT.excl) + aes(x = propmoving) + geom_histogram(color="black", fill="lightblue", binwidth = 0.055) + simpletheme 
ggqqplot(GULD_ACT.excl$propmoving)
#some left skew
ggplot(GULD_ACT.excl) + aes(x = exp(propmoving)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.055) + simpletheme 
ggqqplot(exp(GULD_ACT.excl$propmoving))
#right skewed, exp moving closer to normality


#dist: (mm) total distance travelled during trial
ggplot(GULD_ACT.excl) + aes(x = dist) + geom_histogram(color="black", fill="lightblue", binwidth = 4800) + simpletheme 
ggqqplot(GULD_ACT.excl$dist)
#minimal left skew


#timefrozen_tot: (s) total time spent frozen during trial
ggplot(GULD_ACT.excl) + aes(x = timefrozen_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 85) + simpletheme 
ggqqplot(GULD_ACT.excl$timefrozen_tot)
#right skewed so log transformation applied
ggplot(GULD_ACT.excl) + aes(x = log(timefrozen_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.32) + simpletheme 
ggqqplot(log(GULD_ACT.excl$timefrozen_tot))
#minimal left skew, close to normality


#timefrozen_ave: (s) total duration of frozen periods
ggplot(GULD_ACT.excl) + aes(x = timefrozen_ave) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
ggqqplot(GULD_ACT.excl$timefrozen_ave)
#severely right skewed zero so log transformation applied
ggplot(GULD_ACT.excl) + aes(x = log(timefrozen_ave)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.45) + simpletheme 
ggqqplot(log(GULD_ACT.excl$timefrozen_ave))
#still quite skewed, may exclude from further analysis


#centretime: (s) total time spent >10 from an edge of the arena
ggplot(GULD_ACT.excl) + aes(x = centretime) + geom_histogram(color="black", fill="lightblue", binwidth = 20) + simpletheme 
ggqqplot(GULD_ACT.excl$centretime)
#right skewed so log(n+1) transformation applied as there are 3 zeros
ggplot(GULD_ACT.excl) + aes(x = log(centretime + 1)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.45) + simpletheme 
ggqqplot(log(GULD_ACT.excl$centretime + 1))
#approximately gaussian with a few zeros


#Summary-
#avespeed_tot:   _distribution approximately gaussian
#avespeed_mob:   _distribution approximately gaussian_with four low values that moderately skew data
#aveacceler:     _distribution approximately gaussian
#propmoving:     _distribution marginally left skewed, exp transformation an improvement
#dist:           _distribution approximately gaussian
#timefrozen_tot: _distribution very right skewed, log transformation is approximately gaussian
#timefrozen_ave: _distribution very right skewed, transformations not useful, EXCLUDED
#centretime:     _distribution very right skewed, log(n+1) transformation is approximately gaussian


#Variables to be used for repeatability analysis-
#avespeed_tot       avespeed_mob        aveacceler         propmoving      
#propmoving.exp     dist                timefrozen_tot     timefrozen_tot.ln  
#centretime         centretime.lnplus1

#(note, running untransformed versions just for comparison with transformed)


#Transformations-
GULD_ACT.excl$timefrozen_tot.ln <- log(GULD_ACT.excl$timefrozen_tot) #will use timefrozen_tot.ln as much less skewed distribution
GULD_ACT.excl$propmoving.exp <- exp(GULD_ACT.excl$propmoving)
GULD_ACT.excl$centretime.lnplus1 <- log(GULD_ACT.excl$centretime + 1)


#Variance analysis-
GULD_avespeed_tot.mod <- lmer(avespeed_tot ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_avespeed_tot.mod)
plot(GULD_avespeed_tot.mod)
GULD_avespeed_tot.rpt <- rpt(avespeed_tot ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
    nboot = 100, npermut = 0)
GULD_avespeed_tot.rpt


GULD_avespeed_mob.mod <- lmer(avespeed_mob ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_avespeed_mob.mod)
plot(GULD_avespeed_mob.mod)
GULD_avespeed_mob.rpt <- rpt(avespeed_mob ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
GULD_avespeed_mob.rpt 


GULD_aveacceler.mod <- lmer(aveacceler ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_aveacceler.mod)
plot(GULD_aveacceler.mod)
GULD_aveacceler.rpt <- rpt(aveacceler ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
GULD_aveacceler.rpt 


GULD_propmoving.mod <- lmer(propmoving ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_propmoving.mod)
plot(GULD_propmoving.mod) #some very minor assymmetry
GULD_propmoving.rpt <- rpt(propmoving ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
GULD_propmoving.rpt
GULD_propmoving.exp.mod <- lmer(propmoving.exp ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_propmoving.exp.mod)
plot(GULD_propmoving.exp.mod) #good
GULD_propmoving.exp.rpt <- rpt(propmoving.exp ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
GULD_propmoving.exp.rpt


GULD_dist.mod <- lmer(dist ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_dist.mod)
plot(GULD_dist.mod)
GULD_dist.rpt <- rpt(dist ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                                   nboot = 100, npermut = 0)
GULD_dist.rpt


GULD_timefrozen_tot.mod <- lmer(timefrozen_tot ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_timefrozen_tot.mod)
plot(GULD_timefrozen_tot.mod) #significant assymmetry
GULD_timefrozen_tot.rpt <- rpt(timefrozen_tot ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_timefrozen_tot.rpt
GULD_timefrozen_tot.ln.mod <- lmer(timefrozen_tot.ln ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_timefrozen_tot.ln.mod)
plot(GULD_timefrozen_tot.ln.mod) #good
GULD_timefrozen_tot.ln.rpt <- rpt(timefrozen_tot.ln ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                     nboot = 100, npermut = 0)
GULD_timefrozen_tot.ln.rpt


GULD_centretime.mod <- lmer(centretime ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_centretime.mod)
plot(GULD_centretime.mod) #significant assymmetry
GULD_centretime.rpt <- rpt(centretime ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                                   nboot = 100, npermut = 0)
GULD_centretime.rpt
GULD_centretime.lnplus1.mod <- lmer(centretime.lnplus1 ~ (1|FishID), data=GULD_ACT.excl)
summary(GULD_centretime.lnplus1.mod)
plot(GULD_centretime.lnplus1.mod)
GULD_centretime.lnplus1.rpt <- rpt(centretime.lnplus1 ~ (1 | FishID), grname = "FishID", data = GULD_ACT.excl, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_centretime.lnplus1.rpt


#Summary-
#avespeed_tot:      _ 0.467 [0.266, 0.618] ***
#avespeed_mob       _ 0.181 [0, 0.374]     *   
#aveacceler         _ 0.473 [0.27, 0.63]   ***
#propmoving         _ 0.496 [0.251, 0.614] ***
#propmoving.exp     _ 0.531 [0.371, 0.683] ***
#dist               _ 0.468 [0.293, 0.596] ***
#timefrozen_tot     _ 0.336 [0.1, 0.504]   **
#timefrozen_tot.ln  _ 0.374 [0.134, 0.583] ***
#centretime.lnplus1 _ 0.318 [0.106, 0.505] ***
#centretime.lnplus1 _ 0.43  [0.163, 0.63]  ***



### 2.2 Exploration Assay (EXPL) ----
GULD_EXPL <- read.csv("~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis//GULDEXPLdat_19112020.csv")
nrow(GULD_EXPL) #125 rows


#Re-creating unique id for each trial 
GULD_EXPL$UniqueID <- paste(GULD_EXPL$TrialType, GULD_EXPL$TrialDay, sep = "")
GULD_EXPL$UniqueID <- paste(GULD_EXPL$UniqueID, GULD_EXPL$TrialRound, sep = "_")
GULD_EXPL$UniqueID <- paste(GULD_EXPL$UniqueID, GULD_EXPL$ArenaID, sep = "")


#Excluding trials with no fish-
GULD_EXPL <- subset(GULD_EXPL, GULD_EXPL$PITID != "NOFISH")
n_distinct(GULD_EXPL$PITID) #47 unique IDs


#Excluding failed trials where fish escaped
GULD_EXPL <- subset(GULD_EXPL, GULD_EXPL$PITID != "2044/1395") #excluding trial where fish escaped arena
nrow(GULD_EXPL) #116 trials
n_distinct(GULD_EXPL$PITID) #47 unique IDs


#Adding FishIDs to the dataframe
GULD_EXPL <- merge(GULD_fish, GULD_EXPL, by = 'PITID', all.x = FALSE)
n_distinct(GULD_EXPL$FishID) #data for 46 fish


#Excluding some trials due to: injuries found on individuals on post-trial examination-
#Injuries likely occured post-arrival in lab, so excluded to avoid possible behavioural effects
GULD_EXPL.excl <- subset(GULD_EXPL, UniqueID != "EXPL1_3A") #PITID 2155
GULD_EXPL.excl <- subset(GULD_EXPL.excl, UniqueID != "EXPL1_3C") #PITID 2165
GULD_EXPL.excl <- subset(GULD_EXPL.excl, UniqueID != "EXPL1_10B") #PITID 2191


nrow(GULD_EXPL.excl) #113 trials
n_distinct(GULD_EXPL.excl$PITID) #data for 43 fish


#Initial exploration of variable and assessing distributions-
#emergelat: (s) latency to emerge from the shelter
ggplot(GULD_EXPL.excl) + aes(x = emergelat) + geom_histogram(color="black", fill="lightblue", binwidth = 170) + simpletheme 
ggqqplot(GULD_EXPL.excl$emergelat) 

#endpointlat: (s) latency to explore to the endpoint from trial start time
ggplot(GULD_EXPL.excl) + aes(x = endpointlat) + geom_histogram(color="black", fill="lightblue", binwidth = 170) + simpletheme 
ggqqplot(GULD_EXPL.excl$endpointlat) 

#endpointspeed: (s) latency to explore to the endpoint from time of emergence
ggplot(GULD_EXPL.excl) + aes(x = endpointspeed) + geom_histogram(color="black", fill="lightblue", binwidth = 100) + simpletheme 
ggqqplot(GULD_EXPL.excl$endpointspeed) 
ggplot(GULD_EXPL.excl) + aes(x = log(endpointspeed)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULD_EXPL.excl$endpointspeed)) #still some right skewed but better

#refugereturnlat: (s) latency to return to refuge after first emergence
ggplot(GULD_EXPL.excl) + aes(x = refugereturnlat) + geom_histogram(color="black", fill="lightblue", binwidth = 40) + simpletheme 
ggqqplot(GULD_EXPL.excl$refugereturnlat) 
ggplot(GULD_EXPL.excl) + aes(x = log(refugereturnlat)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULD_EXPL.excl$refugereturnlat)) #near normal


#Summary-
#emergelat:         _bimodal
#endpointlat:       _bimodal
#endpointspeed      _heavily right skewed, log transformation is better, 46 NAs
#refugereturnlat    _heavily right skewed, log transformation is approximately normal, 30 NAs 


#Variables to be used for repeatability analysis-
#emergelat.bin          endpointlat.bin       
#endpointspeed          endpointspeed.ln
#refugereturnlat        refugereturnlat.ln


#Coversions to binomial using median emergence time as conversion point (1 <= median, 0 > median)
median(GULD_EXPL.excl$emergelat) #median = 55
GULD_EXPL.excl1 <- subset(GULD_EXPL.excl, emergelat <= 55)
GULD_EXPL.excl2 <- subset(GULD_EXPL.excl, emergelat >= 56)
GULD_EXPL.excl1$emergelat.bin <- 1
GULD_EXPL.excl2$emergelat.bin <- 0
GULD_EXPL.excl <- rbind(GULD_EXPL.excl1, GULD_EXPL.excl2)


median(GULD_EXPL.excl$endpointlat) #median = 178
GULD_EXPL.excl1 <- subset(GULD_EXPL.excl, endpointlat <= 178)
GULD_EXPL.excl2 <- subset(GULD_EXPL.excl, endpointlat >= 179)
GULD_EXPL.excl1$endpointlat.bin <- 1
GULD_EXPL.excl2$endpointlat.bin <- 0
GULD_EXPL.excl <- rbind(GULD_EXPL.excl1, GULD_EXPL.excl2)


#Transformations-
GULD_EXPL.excl$endpointspeed.ln <- log(GULD_EXPL.excl$endpointspeed)
GULD_EXPL.excl$refugereturnlat.ln <- log(GULD_EXPL.excl$refugereturnlat)


#Variance analysis-
GULD_emergelat.bin.mod <- glmer(emergelat.bin ~ (1|FishID), family = binomial, data=GULD_EXPL.excl)
summary(GULD_emergelat.bin.mod)
GULD_emergelat.bin.rpt <- rpt(emergelat.bin ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Binary", 
                                 nboot = 100, npermut = 0)
GULD_emergelat.bin.rpt


GULD_endpointlat.bin.mod <- glmer(endpointlat.bin~ (1|FishID), family = binomial, data=GULD_EXPL.excl)
summary(GULD_endpointlat.bin.mod)
GULD_endpointlat.bin.rpt <- rpt(endpointlat.bin ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Binary", 
                                nboot = 100, npermut = 0)
GULD_endpointlat.bin.rpt


GULD_endpointspeed.mod <- lmer(endpointspeed ~ (1|FishID), data=GULD_EXPL.excl)
summary(GULD_endpointspeed.mod)
plot(GULD_endpointspeed.mod) #significant assymmetry
GULD_endpointspeed.rpt <- rpt(endpointspeed ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_endpointspeed.rpt
GULD_endpointspeed.ln.mod <- lmer(endpointspeed.ln ~ (1|FishID), data=GULD_EXPL.excl)
summary(GULD_endpointspeed.ln.mod)
plot(GULD_endpointspeed.ln.mod) #good
GULD_endpointspeed.ln.rpt <- rpt(endpointspeed.ln ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Gaussian", 
                                   nboot = 100, npermut = 0)
GULD_endpointspeed.ln.rpt


GULD_refugereturnlat.mod <- lmer(refugereturnlat ~ (1|FishID), data=GULD_EXPL.excl)
summary(GULD_refugereturnlat.mod)
plot(GULD_refugereturnlat.mod) #significant assymmetry
GULD_refugereturnlat.rpt <- rpt(refugereturnlat ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Gaussian", 
                                   nboot = 100, npermut = 0)
GULD_refugereturnlat.rpt
GULD_refugereturnlat.ln.mod <- lmer(refugereturnlat.ln ~ (1|FishID), data=GULD_EXPL.excl)
summary(GULD_refugereturnlat.ln.mod)
plot(GULD_refugereturnlat.ln.mod) #good
GULD_refugereturnlat.ln.rpt <- rpt(refugereturnlat.ln ~ (1 | FishID), grname = "FishID", data = GULD_EXPL.excl, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_refugereturnlat.ln.rpt


#Summary-
#emergelat.bin        _ 0.678 [0.355, 0.838] ***
#endpointlat.bin      _ 0.626 [0.248, 0.804] *** 
#endpointspeed        _ 0.098 [0, 0.34]      ns 
#endpointspeed.ln     _ 0.222 [0, 0.489]     . 
#refugereturnlat.ln   _ 0.243 [0, 0.45]      ns 
#refugereturnlat.ln   _ 0.158 [0, 0.411]     ns   


write.csv(GULD_EXPL.excl, '~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis/GULD_EXPL.processing.csv')
write.csv(GULD_ACT.excl, '~/trophicpersonalities_A/2_Guldborgsund_VarianceAnalysis/GULD_ACT.processing.csv')


