# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



Sys.setenv(LANG = "en")
library(dplyr); library(ggplot2); library(ggpubr); library(lme4); library(lmerTest); library(rptR)
library(car)


#5. Preliminary Variance and Factor Analysis (KARR) ----

### 5.1 Evaluation of Behavioural Variables (ACT) ----
KARR_ACT <- read.csv('~/trophicpersonalities_A/5_Karrebaek_PreliminaryAnalysis/KARR_ACT112020.csv')
nrow(KARR_ACT) #96 rows
labels(KARR_ACT)


#Re-creating the unique id for each trial-
KARR_ACT$UniqueID <- paste(KARR_ACT$TrialType, KARR_ACT$TrialDay, sep = "")
KARR_ACT$UniqueID <- paste(KARR_ACT$UniqueID, KARR_ACT$TrialRound, sep = "_")
KARR_ACT$UniqueID <- paste(KARR_ACT$UniqueID, KARR_ACT$ArenaID, sep = "")


#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"),
                       axis.text.x = element_text(size = 10, colour = "black"), 
                       panel.background = element_rect(fill = "white"),
                       axis.title.y  = element_text(size=12, vjust = 2),
                       axis.title.x  = element_text(size=12, vjust = 0.1),
                       panel.border = element_rect(colour = "black", fill=NA, size = 1))


#Initial exploration of variable and assessing distributions-
#avespeed_tot: (mm/s) the average speed of the individual accross the full trial period
ggplot(KARR_ACT) + aes(x = avespeed_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 4.5) + simpletheme 
ggqqplot(KARR_ACT$avespeed_tot)
#right skew due to 5 - 10 inactive fish, consider excluding injured fish
ggplot(KARR_ACT) + aes(x = sqrt(avespeed_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(sqrt(KARR_ACT$avespeed_tot))
#root transformation is much improved


#avespeed_mob: (mm/s) the average speed of the individual excluding periods when it was immobile
ggplot(KARR_ACT) + aes(x = avespeed_mob) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(KARR_ACT$avespeed_mob)
#minimal right skew due to 5 - 10 inactive fish, not an issue


#aveacceler: (mm/s^2) average rate of acceleration accross the trial
ggplot(KARR_ACT) + aes(x = aveacceler) + geom_histogram(color="black", fill="lightblue", binwidth = 14) + simpletheme 
ggqqplot(KARR_ACT$aveacceler)
#minimal right skew due to 5 - 10 inactive fish, not an issue
#aveacceler: (mm/s^2) average rate of acceleration accross the trial
ggplot(KARR_ACT) + aes(x = sqrt(aveacceler)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.7) + simpletheme 
ggqqplot(sqrt(KARR_ACT$aveacceler))
#root transformation is much improved
         

#propmoving: (proportional) proportion of time mobile
ggplot(KARR_ACT) + aes(x = propmoving) + geom_histogram(color="black", fill="lightblue", binwidth = 0.055) + simpletheme 
ggqqplot(KARR_ACT$propmoving)
#some right skew
ggplot(KARR_ACT) + aes(x = exp(propmoving)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.055) + simpletheme 
ggqqplot(exp(KARR_ACT$propmoving))
#applying the transformation used on GULD data worsens right skew, untransformed is moving to normality
ggplot(KARR_ACT) + aes(x = sqrt(propmoving)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.055) + simpletheme 
ggqqplot(sqrt(KARR_ACT$propmoving))
#root transformation is not a clear improvement


#dist: (mm) total distance travelled during trial
ggplot(KARR_ACT) + aes(x = dist) + geom_histogram(color="black", fill="lightblue", binwidth = 4800) + simpletheme 
ggqqplot(KARR_ACT$dist)
#right skew
#dist: (mm) total distance travelled during trial
ggplot(KARR_ACT) + aes(x = sqrt(dist)) + geom_histogram(color="black", fill="lightblue", binwidth = 12) + simpletheme 
ggqqplot(sqrt(KARR_ACT$dist))
#root transformation is much improved


#timefrozen_tot: (s) total time spent frozen during trial
ggplot(KARR_ACT) + aes(x = timefrozen_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 85) + simpletheme 
ggqqplot(KARR_ACT$timefrozen_tot)
#left skewed
ggplot(KARR_ACT) + aes(x = log(timefrozen_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.32) + simpletheme 
ggqqplot(log(KARR_ACT$timefrozen_tot))
#applying the transformation used on GULD data worsens left skew, untransformed is moving to normality


#timefrozen_ave: (s) total duration of frozen periods
ggplot(KARR_ACT) + aes(x = timefrozen_ave) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
ggqqplot(KARR_ACT$timefrozen_ave)
#severely right skewed zero so log transformation applied (consider running as a poisson distribution)
ggplot(KARR_ACT) + aes(x = log(timefrozen_ave)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.45) + simpletheme 
ggqqplot(log(KARR_ACT$timefrozen_ave))
#log transformation is improved but still quite skewed,


#centretime: (s) total time spent >10 from an edge of the arena
ggplot(KARR_ACT) + aes(x = centretime) + geom_histogram(color="black", fill="lightblue", binwidth = 20) + simpletheme 
ggqqplot(KARR_ACT$centretime)
#right skewed so log(n+1) transformation applied as there are 3 zeros
ggplot(KARR_ACT) + aes(x = log(centretime + 1)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.45) + simpletheme 
ggqqplot(log(KARR_ACT$centretime + 1))
#approximately gaussian with some minor zero inflation


#avespeed_tot:   _sqrt transformation approximately gaussian
#avespeed_mob:   _distribution approximately gaussian, with some low values that minimally skew data
#aveacceler:     _sqrt transformation approximately gaussian
#propmoving:     _untransformed or sqrt transformation closest to gaussian
#dist:           _sqrt transformation approximately gaussian
#timefrozen_tot: _left skewed potentially bimodal, original scale is fine, but could run as binomial
#timefrozen_ave: _distribution very right skewed, log transformation is minimal improvement, run as poisson?
#centretime:     _distribution very right skewed, log(n+1) transformation is closest to gaussian


#Variables to be used for repeatability analysis-
#avespeed_tot       avespeed_tot.sqrt   avespeed_mob        
#aveacceler         aveacceler.sqrt     propmoving      
#dist               dist.sqrt           timefrozen_tot 
#centretime.lnplus1

#Transformations-
KARR_ACT$avespeed_tot.sqrt <- sqrt(KARR_ACT$avespeed_tot)
KARR_ACT$aveacceler.sqrt <- sqrt(KARR_ACT$aveacceler)
KARR_ACT$dist.sqrt <- sqrt(KARR_ACT$dist)
KARR_ACT$centretime.lnplus1 <- log(KARR_ACT$centretime + 1)


### 5.2. Variance and Repeatability Analysis ----
KARR_avespeed_tot.mod <- lmer(avespeed_tot ~ (1|FishID), data=KARR_ACT)
summary(KARR_avespeed_tot.mod)
plot(KARR_avespeed_tot.mod) #some minor clustering issues 
KARR_avespeed_tot.rpt <- rpt(avespeed_tot ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
KARR_avespeed_tot.rpt
KARR_avespeed_tot.sqrt.mod <- lmer(avespeed_tot.sqrt ~ (1|FishID), data=KARR_ACT)
summary(KARR_avespeed_tot.sqrt.mod)
plot(KARR_avespeed_tot.sqrt.mod) #a little better
KARR_avespeed_tot.sqrt.rpt <- rpt(avespeed_tot.sqrt ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
KARR_avespeed_tot.sqrt.rpt


KARR_avespeed_mob.mod <- lmer(avespeed_mob ~ (1|FishID), data=KARR_ACT)
summary(KARR_avespeed_mob.mod)
plot(KARR_avespeed_mob.mod)
KARR_avespeed_mob.rpt <- rpt(avespeed_mob ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
KARR_avespeed_mob.rpt


KARR_aveacceler.mod <- lmer(aveacceler ~ (1|FishID), data=KARR_ACT)
summary(KARR_aveacceler.mod)
plot(KARR_aveacceler.mod) #some minor clustering issues 
KARR_aveacceler.rpt <- rpt(aveacceler ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                             nboot = 100, npermut = 0)
KARR_aveacceler.rpt
KARR_aveacceler.sqrt.mod <- lmer(aveacceler.sqrt ~ (1|FishID), data=KARR_ACT)
summary(KARR_aveacceler.sqrt.mod)
plot(KARR_aveacceler.sqrt.mod) #good 
KARR_aveacceler.sqrt.rpt <- rpt(aveacceler.sqrt ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                           nboot = 100, npermut = 0)
KARR_aveacceler.sqrt.rpt


KARR_propmoving.mod <- lmer(propmoving ~ (1|FishID), data=KARR_ACT)
summary(KARR_propmoving.mod)
plot(KARR_propmoving.mod) #minor clustering issue
KARR_propmoving.rpt <- rpt(propmoving ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                                nboot = 100, npermut = 0)
KARR_propmoving.rpt


KARR_dist.mod <- lmer(dist ~ (1|FishID), data=KARR_ACT)
summary(KARR_dist.mod)
plot(KARR_dist.mod) #minor clustering and assymmetry
KARR_dist.rpt <- rpt(dist ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                           nboot = 100, npermut = 0)
KARR_dist.rpt
KARR_dist.sqrt.mod <- lmer(dist.sqrt ~ (1|FishID), data=KARR_ACT)
summary(KARR_dist.sqrt.mod)
plot(KARR_dist.sqrt.mod) #good
KARR_dist.sqrt.rpt <- rpt(dist.sqrt ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                           nboot = 100, npermut = 0)
KARR_dist.sqrt.rpt


KARR_timefrozen_tot.mod <- lmer(timefrozen_tot ~ (1|FishID), data=KARR_ACT)
summary(KARR_timefrozen_tot.mod)
plot(KARR_timefrozen_tot.mod) #minor clustering issue
KARR_timefrozen_tot.rpt <- rpt(timefrozen_tot ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                           nboot = 100, npermut = 0)
KARR_timefrozen_tot.rpt


KARR_centretime.mod <- lmer(centretime ~ (1|FishID), data=KARR_ACT)
summary(KARR_centretime.mod)
plot(KARR_centretime.mod) #significant assymmetry
KARR_centretime.rpt <- rpt(centretime ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                                   nboot = 100, npermut = 0)
KARR_centretime.rpt
KARR_centretime.lnplus1.mod <- lmer(centretime.lnplus1 ~ (1|FishID), data=KARR_ACT)
summary(KARR_centretime.lnplus1.mod)
plot(KARR_centretime.lnplus1.mod) #good
KARR_centretime.lnplus1.rpt <- rpt(centretime.lnplus1 ~ (1 | FishID), grname = "FishID", data = KARR_ACT, datatype = "Gaussian", 
                               nboot = 100, npermut = 0)
KARR_centretime.lnplus1.rpt


#Summary
#avespeed_tot       _ 0.624 [0.448, 0.763] ***
#avespeed_tot.sqrt  _ 0.651 [0.454, 0.787] ***
#avespeed_mob       _ 0.382 [0.084, 0.622] **
#aveacceler         _ 0.663 [0.458, 0.785] ***
#aveacceler.sqrt    _ 0.681 [0.502, 0.795] ***
#propmoving         _ 0.697 [0.534, 0.802] ***
#dist               _ 0.63  [0.504, 0.773] ***
#dist.sqrt          _ 0.656 [0.486, 0.79]  ***
#timefrozen_tot     _ 0.711 [0.526, 0.841] ***
#centretime         _ 0.259 [0, 0.447]     *
#centretime.lnplus1 _ 0.641 [0.401, 0.782] ***

write.csv(KARR_ACT, '~/trophicpersonalities_A/5_Karrebaek_PreliminaryAnalysis/KARR_ACT.processing.csv')


### 5.3. Systematic factors influencing behavioural variation ----
#  Behavioral variables used in analysis:
#    avespeed_tot.sqrt
#    avespeed_mob
#    aveacceler.sqrt
#    propmoving
#    dist.sqrt
#    timefrozen_tot
#    centretime.lnplus1

#Experimental control predictors 
#    TrialDay
#    TrialRound
#    ArenaID
#    TankID
#Currently running Trial day as categorical, 4 = pre-treatment, 5 = post treatment
KARR_ACT.processed$TrialDay <- as.factor(KARR_ACT.processed$TrialDay)

KARR_ACT.processed <- read.csv('~/trophicpersonalities_A/5_Karrebaek_PreliminaryAnalysis/KARR_ACT.processing.csv')



KARR_ACT_avespeed_tot.sqrt.mod.sysfact <- lmer(avespeed_tot.sqrt ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                      data=KARR_ACT.processed)
plot(KARR_ACT_avespeed_tot.sqrt.mod.sysfact)
Anova(KARR_ACT_avespeed_tot.sqrt.mod.sysfact) 
summary(KARR_ACT_avespeed_tot.sqrt.mod.sysfact) 
#TrialDay:   ***, significant decrease in post-treatment trials
#TrialRound: ns
#ArenaID:    ns
#TankID:     *, D_4 significantly higher, D_3 marginally higher


KARR_ACT_avespeed_mob.mod.sysfact <- lmer(avespeed_mob ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                               data=KARR_ACT.processed)
plot(KARR_ACT_avespeed_mob.mod.sysfact)
Anova(KARR_ACT_avespeed_mob.mod.sysfact) 
summary(KARR_ACT_avespeed_mob.mod.sysfact) 
#TrialDay:   *, marginal decrease in post-treatment trials 
#TrialRound: ns
#ArenaID:    ns
#TankID:     ns


KARR_ACT_aveacceler.sqrt.mod.sysfact <- lmer(aveacceler.sqrt ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                          data=KARR_ACT.processed)
plot(KARR_ACT_aveacceler.sqrt.mod.sysfact)
Anova(KARR_ACT_aveacceler.sqrt.mod.sysfact) 
summary(KARR_ACT_aveacceler.sqrt.mod.sysfact) 
#TrialDay:   **, significant decrease in post-treatment trials
#TrialRound: ns
#ArenaID:    ns
#TankID:     ns


KARR_ACT_propmoving.mod.sysfact <- lmer(propmoving ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                             data=KARR_ACT.processed)
plot(KARR_ACT_propmoving.mod.sysfact)
Anova(KARR_ACT_propmoving.mod.sysfact) 
summary(KARR_ACT_propmoving.mod.sysfact) 
#TrialDay:   ***, significant decrease in post-treatment trials
#TrialRound: ns
#ArenaID:    *, Arena B,C,D,G significantly lower
#TankID:     ., D_3, D_4 marginally higher


KARR_ACT_dist.sqrt.sysfact <- lmer(dist.sqrt ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                        data=KARR_ACT.processed)
plot(KARR_ACT_dist.sqrt.sysfact)
Anova(KARR_ACT_dist.sqrt.sysfact) 
summary(KARR_ACT_dist.sqrt.sysfact) 
#TrialDay:   ***, significant decrease in post-treatment trials
#TrialRound: ns
#ArenaID:    ns
#TankID:     *, D_4 significantly higher, D_3 marginally higher


KARR_ACT_timefrozen_tot.sysfact <- lmer(timefrozen_tot ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                   data=KARR_ACT.processed)
plot(KARR_ACT_timefrozen_tot.sysfact)
Anova(KARR_ACT_timefrozen_tot.sysfact) 
summary(KARR_ACT_timefrozen_tot.sysfact) 
#TrialDay:   ***, significant increase in post-treatment trials
#TrialRound: ns
#ArenaID:    *, Arenas B,C,D,G significantly higher, F marginally higher 
#TankID:     ., D_4, D_3 significantly lower


KARR_ACT_centretime.lnplus1.sysfact <- lmer(centretime.lnplus1 ~ TrialDay + TrialRound + ArenaID + TankID + (1|FishID), 
                                        data=KARR_ACT.processed)
plot(KARR_ACT_centretime.lnplus1.sysfact)
Anova(KARR_ACT_centretime.lnplus1.sysfact) 
summary(KARR_ACT_centretime.lnplus1.sysfact) 
#TrialDay:   ns
#TrialRound: ns
#ArenaID:    ns
#TankID:     ns


#  Additional factors included for each behavioural variable:
#    avespeed_tot       - TrialDay + TankID
#    avespeed_mob       - TrialDay
#    aveacceler         - TrialDay + ArenaID + Sex
#    propmoving.exp     - TrialDay + ArenaID + Sex + ConditionFactor
#    dist               - TrialDay + ArenaID + Sex
#    timefrozen_tot.ln  - TrialDay + ArenaID + ConditionFactor + InfectionScore
#    centretime.lnplus1 - ArenaID 
#    emergelat.bin.B    - nil
#    endpointlat.bin.B  - nil
#    endpointspeed.ln   - nil
#    refugereturnlat.ln - TrialDay + TrialRound



