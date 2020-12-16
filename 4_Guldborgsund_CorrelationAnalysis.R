# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



Sys.setenv(LANG = "en")
library(dplyr); library(ggplot2); library(vegan)



#4. Correlations Between Behavioural Variables ----

GULD_ACT.processed <- read.csv('~/trophicpersonalities_A/3_Guldbordsund_FactorAnalysis/GULD_ACT.processed.csv')
GULD_EXPL.processed <- read.csv('~/trophicpersonalities_A/3_Guldbordsund_FactorAnalysis/GULD_EXPL.processed.csv')

#  Behavioral variables of interest:
#    avespeed_tot
#    avespeed_mob
#    aveacceler
#    propmoving.exp
#    dist
#    timefrozen_tot.ln
#    centretime.lnplus1
#    emergelat.bin
#    endpointlat.bin
#    endpointspeed.ln
#    refugereturnlat.ln


### 4.1. Building dataframe with all beahvioural variables ----
GULD_ACT.processed$mergeID <- paste(GULD_ACT.processed$FishID, GULD_ACT.processed$TrialDay, sep ='_')
GULD_EXPL.processed$mergeID <- paste(GULD_EXPL.processed$FishID, GULD_EXPL.processed$TrialDay, sep ='_')
GULD_behav_merged <- merge(GULD_ACT.processed, GULD_EXPL.processed, by = 'mergeID', all.x = TRUE)
GULD_behav_merged <- select(GULD_behav_merged, -c(Weight.y, emergelat,
                                                  Notes.y, UniqueID.y, endpointtime, endpointlat, endpointspeed, refugereturntime, 
                                                  refugereturnlat, Date.y, TimeLoaded.y, TimeInitiated, TrialType.y, TrialDay.y, 
                                                  TrialRound.y, ArenaID.y, TankID.y,emergetime, X.y, PITID.y, FishID.y,
                                                  TL.y, InfectionScore.y, Sex.y, timefrozen_tot, timefrozen_ave, centretime, Notes.x,
                                                  propmoving, TrialType.x, Date.x, mergeID, X.x, TimeLoaded.x, ArenaID.x, ConditionFactor.y))

labels(GULD_behav_merged)
GULD_behav_merged <- rename(GULD_behav_merged, PITID = PITID.x)
GULD_behav_merged <- rename(GULD_behav_merged, FishID = FishID.x)
GULD_behav_merged <- rename(GULD_behav_merged, TL = TL.x)
GULD_behav_merged <- rename(GULD_behav_merged, InfectionScore = InfectionScore.x)
GULD_behav_merged <- rename(GULD_behav_merged, Sex = Sex.x)
GULD_behav_merged <- rename(GULD_behav_merged, TrialDay = TrialDay.x)
GULD_behav_merged <- rename(GULD_behav_merged, TrialRound = TrialRound.x)
GULD_behav_merged <- rename(GULD_behav_merged, TankID = TankID.x)
GULD_behav_merged <- rename(GULD_behav_merged, Weight = Weight.x)
GULD_behav_merged <- rename(GULD_behav_merged, ConditionFactor = ConditionFactor.x)
GULD_behav_merged <- rename(GULD_behav_merged, UniqueID = UniqueID.x)
labels(GULD_behav_merged)


### 4.2. Spearman Correlations ----

#  Visualising correlations
pairs(GULD_behav_merged[,12:22])
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$avespeed_mob, method = 'spearman')            # 0.6785468   ***
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$aveacceler, method = 'spearman')              # 0.9519464   ***
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$propmoving.exp, method = 'spearman')          # 0.9283687   ***
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$dist, method = 'spearman')                    # 0.9946242   ***
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')       # -0.8766261  ***
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$centretime.lnplus1, method = 'spearman')      # 0.2611043   **
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$emergelat.bin, method = 'spearman')           # 0.3906882   ***
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$endpointlat.bin, method = 'spearman')         # 0.347821    ***
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$endpointspeed.ln, method = 'spearman')        # -0.04041535 
cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')      # 0.2067612   .
      
cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$aveacceler, method = 'spearman')              # 0.5202159   ***
cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$propmoving.exp, method = 'spearman')          # 0.4108809   ***
cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$dist, method = 'spearman')                    # 0.6454957   ***
cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')       # -0.4084209  ***
cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$centretime.lnplus1, method = 'spearman')      # 0.2512216   **
cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$emergelat.bin, method = 'spearman')           # 0.1443376   
cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$endpointlat.bin, method = 'spearman')         # 0.1459654   
cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$endpointspeed.ln, method = 'spearman')        # -0.007647766 
cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')      # 0.05115232   
      
cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$propmoving.exp, method = 'spearman')            # 0.9594258   ***
cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$dist, method = 'spearman')                      # 0.9574244   ***
cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')         # -0.8711334  ***
cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$centretime.lnplus1, method = 'spearman')        # 0.1969622   *
cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$emergelat.bin, method = 'spearman')             # 0.4617717   ***   
cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$endpointlat.bin, method = 'spearman')           # 0.3999127   ***
cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$endpointspeed.ln, method = 'spearman')          # -0.1042732  
cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')        # 0.2225698   *
  
cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$dist, method = 'spearman')                  # 0.9382001   ***
cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')     # -0.9206188  ***
cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$centretime.lnplus1, method = 'spearman')    # 0.198686    *
cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$emergelat.bin, method = 'spearman')         # 0.4747946   ***   
cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$endpointlat.bin, method = 'spearman')       # 0.4091373   ***
cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$endpointspeed.ln, method = 'spearman')      # -0.04768372  
cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')    # 0.2829491   **
  
cor.test(GULD_behav_merged$dist, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')               # -0.8783937  ***
cor.test(GULD_behav_merged$dist, GULD_behav_merged$centretime.lnplus1, method = 'spearman')              # 0.2494248   **
cor.test(GULD_behav_merged$dist, GULD_behav_merged$emergelat.bin, method = 'spearman')                   # 0.3803783   ***   
cor.test(GULD_behav_merged$dist, GULD_behav_merged$endpointlat.bin, method = 'spearman')                 # 0.3353407   ***
cor.test(GULD_behav_merged$dist, GULD_behav_merged$endpointspeed.ln, method = 'spearman')                # -0.0445687   
cor.test(GULD_behav_merged$dist, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')              # 0.2352503   *
 
cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$centretime.lnplus1, method = 'spearman') # -0.2141717  *
cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$emergelat.bin, method = 'spearman')      # -0.3991005  ***   
cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$endpointlat.bin, method = 'spearman')    # -0.3472798  ***
cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$endpointspeed.ln, method = 'spearman')   # 0.00246608  
cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$refugereturnlat.ln, method = 'spearman') # -0.2230328  *

cor.test(GULD_behav_merged$centretime.lnplus1, GULD_behav_merged$emergelat.bin, method = 'spearman')     # 0.1722862   .   
cor.test(GULD_behav_merged$centretime.lnplus1, GULD_behav_merged$endpointlat.bin, method = 'spearman')   # 0.1028291   
cor.test(GULD_behav_merged$centretime.lnplus1, GULD_behav_merged$endpointspeed.ln, method = 'spearman')  # 0.01855069 
cor.test(GULD_behav_merged$centretime.lnplus1, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')# -0.1046046  

cor.test(GULD_behav_merged$emergelat.bin, GULD_behav_merged$endpointlat.bin, method = 'spearman')        # 0.822995    ***
cor.test(GULD_behav_merged$emergelat.bin, GULD_behav_merged$endpointspeed.ln, method = 'spearman')       # -0.384746   **
cor.test(GULD_behav_merged$emergelat.bin, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')     # 0.1599561  

cor.test(GULD_behav_merged$endpointlat.bin, GULD_behav_merged$endpointspeed.ln, method = 'spearman')     # -0.5917001  ***
cor.test(GULD_behav_merged$endpointlat.bin, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')   # 0.205503    .

cor.test(GULD_behav_merged$endpointspeed.ln, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')  # -0.01330125 .

#Summary:
# Appears to be strong correlations between all activity variables, 
# more active individual also are to be more likely to emerge and reach endpoint.


### 4.3. PCA Analysis ----
#This is using a reduced dataset,
# - as PCA cannot cope with NAs, only Trial round with both EXPL and ACT data included (5 rows excluded, as fish died/euthanised between ACT and EXPL trial) 
GULD_behav_merged.pca <- subset(GULD_behav_merged, emergelat.bin != 'NA')
# - as only a subset of fish that emerged have endpointspeed.ln refugereturnlat.ln data, these have not been included here
GULD_behav_merged.pca <- select(GULD_behav_merged.pca, -c(endpointspeed.ln, refugereturnlat.ln))


GULD_behav_merged.pca.rda <- rda(GULD_behav_merged.pca[,c(-1,-2,-3,-4-5,-6,-7,-8,-9,-10,-11)], scale=TRUE)
summary(GULD_behav_merged.pca.rda, display=NULL) 
screeplot(GULD_behav_merged.pca.rda)
abline(a = 1, b = 0) 
#PC1, PC2, and PC3 have eigenvalue greater than 1, suggesting three distinct axes of behavioural variation


#Ordination plot with systematic and state factors included
GULD_behav_merged.pca.ord1 <- ordiplot(GULD_behav_merged.pca.rda, type = "n")
data.envfit <- envfit(GULD_behav_merged.pca.rda, GULD_behav_merged.pca[,12:20])
plot(data.envfit, col="blue")
data.envfit

Sex <- model.matrix(~-1+Sex, GULD_behav_merged.pca)
data.envfit2 <- envfit(GULD_behav_merged.pca.rda, env=Sex)
data.envfit2
plot(data.envfit2, col="green")
InfectionScore <- model.matrix(~-1+InfectionScore, GULD_behav_merged.pca)
data.envfit3 <- envfit(GULD_behav_merged.pca.rda, env=InfectionScore)
data.envfit3
plot(data.envfit3, col="red")
ConditionFactor <- model.matrix(~-1+ConditionFactor, GULD_behav_merged.pca)
data.envfit4 <- envfit(GULD_behav_merged.pca.rda, env=ConditionFactor)
data.envfit4
plot(data.envfit4, col="pink")
TrialDay <- model.matrix(~-1+TrialDay, GULD_behav_merged.pca)
data.envfit5 <- envfit(GULD_behav_merged.pca.rda, env=TrialDay)
data.envfit5
plot(data.envfit5, col="darkgreen")


#Taking PC1 and PC2 as composite variables
GULD_behav_merged.pca.scores <- as.data.frame(scores(GULD_behav_merged.pca.rda, display="sites"))
GULD_behav_merged.pca.loadings <- scores(GULD_behav_merged.pca.rda, display="species")

#Adding those back into the main dataframe
GULD_behav_merged.pca.scores$mergeID <- (1:113)
GULD_behav_merged.pca$mergeID <- (1:113)
GULD_behav_merged.pca <- merge(GULD_behav_merged.pca, GULD_behav_merged.pca.scores, by = 'mergeID')
nrow(GULD_behav_merged.pca)
GULD_behav_merged.pca$mergeID <- paste(GULD_behav_merged.pca$FishID, GULD_behav_merged.pca$TrialDay, sep = '_')
GULD_behav_merged$mergeID <-  paste(GULD_behav_merged$FishID, GULD_behav_merged$TrialDay, sep = '_')
GULD_behav_merged.pca <- select(GULD_behav_merged.pca, c(mergeID, PC1, PC2))
GULD_behav_merged <- merge(GULD_behav_merged, GULD_behav_merged.pca, by = 'mergeID', all.x = TRUE)


#Analysing effects on composite variables
GULD_PC1.mod <- lmer(PC1 ~  TrialDay + Sex + InfectionScore + (1|FishID), data=GULD_behav_merged)
summary(GULD_PC1.mod)
plot(GULD_PC1.mod)
GULD_PC1.rpt.adj <- rpt(PC1 ~  TrialDay + Sex + InfectionScore + (1 | FishID), grname = "FishID", 
                        data = GULD_behav_merged, datatype = "Gaussian", 
                        nboot = 100, npermut = 0)
GULD_PC1.rpt.adj
#PC1:
#    -ve male Sex effect 
#    positive TrialDay effect
#    no InfectionScore effect
#    highly repeatable (0.59, [0.473, 0.758])

GULD_PC2.mod <- lmer(PC2 ~  TrialDay + Sex + InfectionScore + (1|FishID), data=GULD_behav_merged)
summary(GULD_PC2.mod)
plot(GULD_PC2.mod)
GULD_PC2.rpt.adj <- rpt(PC2 ~  TrialDay + Sex + InfectionScore + (1 | FishID), grname = "FishID", 
                        data = GULD_behav_merged, datatype = "Gaussian", 
                        nboot = 100, npermut = 0)
GULD_PC2.rpt.adj
#PC1:
#    no Sex effect
#    positive TrialDay effect
#    no InfectionScore effect
#    highly repeatable (0.536, [0.352, 0.704])

write.csv(GULD_behav_merged, '~/trophicpersonalities_A/4_Guldbordsund_CorrelationAnalysis/GULD_merged.csv')

