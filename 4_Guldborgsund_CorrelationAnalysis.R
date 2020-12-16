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

cor01_02 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$avespeed_mob, method = 'spearman')            # 0.6785468   ***
cor01_03 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$aveacceler, method = 'spearman')              # 0.9519464   ***
cor01_04 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$propmoving.exp, method = 'spearman')          # 0.9283687   ***
cor01_05 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$dist, method = 'spearman')                    # 0.9946242   ***
cor01_06 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')       # -0.8766261  ***
cor01_07 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$centretime.lnplus1, method = 'spearman')      # 0.2611043   **
cor01_08 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$emergelat.bin, method = 'spearman')           # 0.3906882   ***
cor01_09 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$endpointlat.bin, method = 'spearman')         # 0.347821    ***
cor01_10 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$endpointspeed.ln, method = 'spearman')        # -0.04041535 
cor01_11 <- cor.test(GULD_behav_merged$avespeed_tot, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')      # 0.2067612   .
      
cor02_03 <- cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$aveacceler, method = 'spearman')              # 0.5202159   ***
cor02_04 <- cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$propmoving.exp, method = 'spearman')          # 0.4108809   ***
cor02_05 <- cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$dist, method = 'spearman')                    # 0.6454957   ***
cor02_06 <- cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')       # -0.4084209  ***
cor02_07 <- cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$centretime.lnplus1, method = 'spearman')      # 0.2512216   **
cor02_08 <- cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$emergelat.bin, method = 'spearman')           # 0.1443376   
cor02_09 <- cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$endpointlat.bin, method = 'spearman')         # 0.1459654   
cor02_10 <- cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$endpointspeed.ln, method = 'spearman')        # -0.007647766 
cor02_11 <- cor.test(GULD_behav_merged$avespeed_mob, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')      # 0.05115232   
      
cor03_04 <- cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$propmoving.exp, method = 'spearman')            # 0.9594258   ***
cor03_05 <- cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$dist, method = 'spearman')                      # 0.9574244   ***
cor03_06 <- cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')         # -0.8711334  ***
cor03_07 <- cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$centretime.lnplus1, method = 'spearman')        # 0.1969622   *
cor03_08 <- cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$emergelat.bin, method = 'spearman')             # 0.4617717   ***   
cor03_09 <- cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$endpointlat.bin, method = 'spearman')           # 0.3999127   ***
cor03_10 <- cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$endpointspeed.ln, method = 'spearman')          # -0.1042732  
cor03_11 <- cor.test(GULD_behav_merged$aveacceler, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')        # 0.2225698   *
  
cor04_05 <- cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$dist, method = 'spearman')                  # 0.9382001   ***
cor04_06 <- cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')     # -0.9206188  ***
cor04_07 <- cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$centretime.lnplus1, method = 'spearman')    # 0.198686    *
cor04_08 <- cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$emergelat.bin, method = 'spearman')         # 0.4747946   ***   
cor04_09 <- cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$endpointlat.bin, method = 'spearman')       # 0.4091373   ***
cor04_10 <- cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$endpointspeed.ln, method = 'spearman')      # -0.04768372  
cor04_11 <- cor.test(GULD_behav_merged$propmoving.exp, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')    # 0.2829491   **
  
cor05_06 <- cor.test(GULD_behav_merged$dist, GULD_behav_merged$timefrozen_tot.ln, method = 'spearman')               # -0.8783937  ***
cor05_07 <- cor.test(GULD_behav_merged$dist, GULD_behav_merged$centretime.lnplus1, method = 'spearman')              # 0.2494248   **
cor05_08 <- cor.test(GULD_behav_merged$dist, GULD_behav_merged$emergelat.bin, method = 'spearman')                   # 0.3803783   ***   
cor05_09 <- cor.test(GULD_behav_merged$dist, GULD_behav_merged$endpointlat.bin, method = 'spearman')                 # 0.3353407   ***
cor05_10 <- cor.test(GULD_behav_merged$dist, GULD_behav_merged$endpointspeed.ln, method = 'spearman')                # -0.0445687   
cor05_11 <- cor.test(GULD_behav_merged$dist, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')              # 0.2352503   *
 
cor06_07 <- cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$centretime.lnplus1, method = 'spearman') # -0.2141717  *
cor06_08 <- cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$emergelat.bin, method = 'spearman')      # -0.3991005  ***   
cor06_09 <- cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$endpointlat.bin, method = 'spearman')    # -0.3472798  ***
cor06_10 <- cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$endpointspeed.ln, method = 'spearman')   # 0.00246608  
cor06_11 <- cor.test(GULD_behav_merged$timefrozen_tot.ln, GULD_behav_merged$refugereturnlat.ln, method = 'spearman') # -0.2230328  *

cor07_08 <- cor.test(GULD_behav_merged$centretime.lnplus1, GULD_behav_merged$emergelat.bin, method = 'spearman')     # 0.1722862   .   
cor07_09 <- cor.test(GULD_behav_merged$centretime.lnplus1, GULD_behav_merged$endpointlat.bin, method = 'spearman')   # 0.1028291   
cor07_10 <- cor.test(GULD_behav_merged$centretime.lnplus1, GULD_behav_merged$endpointspeed.ln, method = 'spearman')  # 0.01855069 
cor07_11 <- cor.test(GULD_behav_merged$centretime.lnplus1, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')# -0.1046046  

cor08_09 <- cor.test(GULD_behav_merged$emergelat.bin, GULD_behav_merged$endpointlat.bin, method = 'spearman')        # 0.822995    ***
cor08_10 <- cor.test(GULD_behav_merged$emergelat.bin, GULD_behav_merged$endpointspeed.ln, method = 'spearman')       # -0.384746   **
cor08_11 <- cor.test(GULD_behav_merged$emergelat.bin, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')     # 0.1599561  

cor09_10 <- cor.test(GULD_behav_merged$endpointlat.bin, GULD_behav_merged$endpointspeed.ln, method = 'spearman')     # -0.5917001  ***
cor09_11 <- cor.test(GULD_behav_merged$endpointlat.bin, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')   # 0.205503    .

cor10_11 <- cor.test(GULD_behav_merged$endpointspeed.ln, GULD_behav_merged$refugereturnlat.ln, method = 'spearman')  # -0.01330125 .

GULD_behav_corr <- NULL
GULD_behav_corr$comparison <- 
  c(cor01_02$ data.name,cor01_03$ data.name,cor01_04$ data.name,cor01_05$ data.name,cor01_06$ data.name,cor01_07$ data.name,cor01_08$ data.name,cor01_09$ data.name,cor01_10$ data.name,cor01_11$ data.name,cor02_03$ data.name,cor02_04$ data.name,cor02_05$ data.name,cor02_06$ data.name,cor02_07$ data.name,cor02_08$ data.name,cor02_09$ data.name,cor02_10$ data.name,cor02_11$ data.name,cor03_04$ data.name,cor03_05$ data.name,cor03_06$ data.name,cor03_07$ data.name,cor03_08$ data.name,cor03_09$ data.name,cor03_10$ data.name,cor03_11$ data.name,cor04_05$ data.name,cor04_06$ data.name,cor04_07$ data.name,cor04_08$ data.name,cor04_09$ data.name,cor04_10$ data.name,cor04_11$ data.name,cor05_06$ data.name,cor05_07$ data.name,cor05_08$ data.name,cor05_09$ data.name,cor05_10$ data.name,cor05_11$ data.name,cor06_07$ data.name,cor06_08$ data.name,cor06_09$ data.name,cor06_10$ data.name,cor06_11$ data.name,cor07_08$ data.name,cor07_09$ data.name,cor07_10$ data.name,cor07_11$ data.name,cor08_09$ data.name,cor08_10$ data.name,cor08_11$ data.name,cor09_10$ data.name,cor09_11$ data.name,cor10_11$ data.name)
GULD_behav_corr <- as.data.frame(GULD_behav_corr)
GULD_behav_corr$method <- 'Spearmans rank correlation rho' 
GULD_behav_corr$rho <- 
  c(cor01_02$ estimate,cor01_03$ estimate,cor01_04$ estimate,cor01_05$ estimate,cor01_06$ estimate,cor01_07$ estimate,cor01_08$ estimate,cor01_09$ estimate,cor01_10$ estimate,cor01_11$ estimate,cor02_03$ estimate,cor02_04$ estimate,cor02_05$ estimate,cor02_06$ estimate,cor02_07$ estimate,cor02_08$ estimate,cor02_09$ estimate,cor02_10$ estimate,cor02_11$ estimate,cor03_04$ estimate,cor03_05$ estimate,cor03_06$ estimate,cor03_07$ estimate,cor03_08$ estimate,cor03_09$ estimate,cor03_10$ estimate,cor03_11$ estimate,cor04_05$ estimate,cor04_06$ estimate,cor04_07$ estimate,cor04_08$ estimate,cor04_09$ estimate,cor04_10$ estimate,cor04_11$ estimate,cor05_06$ estimate,cor05_07$ estimate,cor05_08$ estimate,cor05_09$ estimate,cor05_10$ estimate,cor05_11$ estimate,cor06_07$ estimate,cor06_08$ estimate,cor06_09$ estimate,cor06_10$ estimate,cor06_11$ estimate,cor07_08$ estimate,cor07_09$ estimate,cor07_10$ estimate,cor07_11$ estimate,cor08_09$ estimate,cor08_10$ estimate,cor08_11$ estimate,cor09_10$ estimate,cor09_11$ estimate,cor10_11$ estimate)
GULD_behav_corr$p.value <- 
  c(cor01_02$ p.value , cor01_03$ p.value , cor01_04$ p.value , cor01_05$ p.value , cor01_06$ p.value , cor01_07$ p.value , cor01_08$ p.value , cor01_09$ p.value , cor01_10$ p.value , cor01_11$ p.value , cor02_03$ p.value , cor02_04$ p.value , cor02_05$ p.value , cor02_06$ p.value , cor02_07$ p.value , cor02_08$ p.value , cor02_09$ p.value , cor02_10$ p.value , cor02_11$ p.value , cor03_04$ p.value , cor03_05$ p.value , cor03_06$ p.value , cor03_07$ p.value , cor03_08$ p.value , cor03_09$ p.value , cor03_10$ p.value , cor03_11$ p.value , cor04_05$ p.value , cor04_06$ p.value , cor04_07$ p.value , cor04_08$ p.value , cor04_09$ p.value , cor04_10$ p.value , cor04_11$ p.value , cor05_06$ p.value , cor05_07$ p.value , cor05_08$ p.value , cor05_09$ p.value , cor05_10$ p.value , cor05_11$ p.value , cor06_07$ p.value , cor06_08$ p.value , cor06_09$ p.value , cor06_10$ p.value , cor06_11$ p.value , cor07_08$ p.value , cor07_09$ p.value , cor07_10$ p.value , cor07_11$ p.value , cor08_09$ p.value , cor08_10$ p.value , cor08_11$ p.value , cor09_10$ p.value , cor09_11$ p.value , cor10_11$ p.value)

write.csv(GULD_behav_corr, '~/trophicpersonalities_A/4_Guldbordsund_CorrelationAnalysis/GULD_behav_corr.csv')


#Summary:
# Strong correlations between all ACT variables, 
# avespeed_tot/aveacceler/dist/propmoving.exp all >0.9 rho corr. Likely measured the same way, so only need to use one
#
# More active individual also are to be more likely to emerge and reach endpoint in EXPL trial
# avespeed_tot/aveacceler, dist, propmoving.exp moderate correlation with both emergelat.bin and endpointlat.


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

