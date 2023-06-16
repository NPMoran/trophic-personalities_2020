

#centrescore: (NA) calculated from the proportion of time spent in each area
ggplot(GULDact.processed) + aes(x = centrescore) + geom_histogram(color="black", fill="lightblue", binwidth = 0.2) + simpletheme 
ggqqplot(GULDact.processed$centrescore) #potential minimal positive skew
ggplot(GULDact.processed) + aes(x = sqrt(centrescore)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.05) + simpletheme 
ggqqplot(sqrt(GULDact.processed$centrescore)) #sqrt transformation is better but maybe not neccessary

#centrescore: (NA) calculated from the proportion of time spent in each area


#centretime50: (s) time >5cm away from edge
ggplot(GULDact.processed) + aes(x = centretime50) + geom_histogram(color="black", fill="lightblue", binwidth = 50) + simpletheme 
ggqqplot(GULDact.processed$centretime50) #positive skew
ggplot(GULDact.processed) + aes(x = sqrt(centretime50)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
ggqqplot(sqrt(GULDact.processed$centretime50)) #sqrt transformation close to normal

#centretime75: (s) time >7.5cm away from edge
ggplot(GULDact.processed) + aes(x = centretime75) + geom_histogram(color="black", fill="lightblue", binwidth = 50) + simpletheme 
ggqqplot(GULDact.processed$centretime75) #strong positive skew
ggplot(GULDact.processed) + aes(x = sqrt(centretime75)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
ggqqplot(sqrt(GULDact.processed$centretime75)) #sqrt transformation close to normal

#centretime100: (s) time >10cm away from edge
ggplot(GULDact.processed) + aes(x = centretime100) + geom_histogram(color="black", fill="lightblue", binwidth = 10) + simpletheme 
ggqqplot(GULDact.processed$centretime100) #strong skew
ggplot(GULDact.processed) + aes(x = sqrt(centretime100)) + geom_histogram(color="black", fill="lightblue", binwidth = 1) + simpletheme 
ggqqplot(sqrt(GULDact.processed$centretime100)) #sqrt transformation close to normal


GULD_centrescore.mod <- lmer(centrescore ~ 
                               Sex + TL.C + CondManual.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centrescore.mod)                 #TL effect
summary(GULD_centrescore.mod)               #TrialDay resolve no variance, TankID extremely little
plot(GULD_centrescore.mod)                  #No clustering issues
r2_nakagawa(GULD_centrescore.mod)           #random structure error


GULD_centrescore2.mod <- lmer(centrescore2 ~ 
                                Sex + TL.C + CondManual.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centrescore2.mod)                 #TL effect
summary(GULD_centrescore2.mod)               #TrialDay resolve no variance, TankID extremely little
plot(GULD_centrescore2.mod)                  #No clustering issues
r2_nakagawa(GULD_centrescore2.mod)           #random structure error


GULD_centretime50.sqrt.mod <- lmer(sqrt(centretime50) ~ 
                                     Sex + TL.C + CondManual.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime50.sqrt.mod)                 #No effects
summary(GULD_centretime50.sqrt.mod)               #TrialRound resolves no variance, TankID extremely little
plot(GULD_centretime50.sqrt.mod)                  #No clustering issues
r2_nakagawa(GULD_centretime50.sqrt.mod)           #random structure error


GULD_centretime75.sqrt.mod <- lmer(sqrt(centretime75) ~ 
                                     Sex + TL.C + CondManual.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime75.sqrt.mod)                 #No effects
summary(GULD_centretime75.sqrt.mod)               #TrialRound resolves no variance, TrialDay and TankID extremely little
plot(GULD_centretime75.sqrt.mod)                  #No clustering issues
r2_nakagawa(GULD_centretime75.sqrt.mod)           #random structure error

GULD_centretime100.sqrt.mod <- lmer(sqrt(centretime100) ~ 
                                      Sex + TL.C + CondManual.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime100.sqrt.mod)                 #No effects
summary(GULD_centretime100.sqrt.mod)               #TrialRound resolves no variance, TrialDay and TankID extremely little
plot(GULD_centretime100.sqrt.mod)                  #TrialRound, TrialDay, TankID resolve no variance
r2_nakagawa(GULD_centretime100.sqrt.mod)           #random structure error


save(GULD_avespeed_tot.mod, file = "./Output_Guldborgsund/GULD_avespeed_tot.mod.RData")
save(GULD_avespeed_mob.mod, file = "./Output_Guldborgsund/GULD_avespeed_mob.mod.RData")
save(GULD_aveacceler.mod, file = "./Output_Guldborgsund/GULD_aveacceler.mod.RData")
save(GULD_propmoving.invlog.mod, file = "./Output_Guldborgsund/GULD_propmoving.invlog.mod.RData")
save(GULD_frozenevents.sqrt.mod, file = "./Output_Guldborgsund/GULD_frozenevents.sqrt.mod.RData")
save(GULD_timefrozen_tot.sqrt.mod, file = "./Output_Guldborgsund/GULD_timefrozen_tot.sqrt.mod.RData")
save(GULD_centrescore.mod, file = "./Output_Guldborgsund/GULD_centrescore.mod.RData")
save(GULD_centretime50.sqrt.mod, file = "./Output_Guldborgsund/GULD_centretime50.sqrt.mod.RData")
save(GULD_centretime75.sqrt.mod, file = "./Output_Guldborgsund/GULD_centretime75.sqrt.mod.RData")
save(GULD_centretime100.sqrt.mod, file = "./Output_Guldborgsund/GULD_centretime100.sqrt.mod.RData")




GULD_centrescore.mod.red <- lmer(centrescore ~ 
                                   Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centrescore.mod.red)                 #TL effect
summary(GULD_centrescore.mod.red)               #TrialDay resolve no variance, TankID extremely little
plot(GULD_centrescore.mod.red)                  #No clustering issues
r2_nakagawa(GULD_centrescore.mod.red)           #random structure error


GULD_centretime50.sqrt.mod.red <- lmer(sqrt(centretime50) ~ 
                                         Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialDay) + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime50.sqrt.mod.red)                 #No effects
summary(GULD_centretime50.sqrt.mod.red)               #TrialRound resolves no variance, TankID extremely little
plot(GULD_centretime50.sqrt.mod.red)                  #No clustering issues
r2_nakagawa(GULD_centretime50.sqrt.mod.red)           #random structure error


GULD_centretime75.sqrt.mod.red <- lmer(sqrt(centretime75) ~ 
                                         Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime75.sqrt.mod.red)                 #No effects
summary(GULD_centretime75.sqrt.mod.red)               #TrialRound resolves no variance, TrialDay and TankID extremely little
plot(GULD_centretime75.sqrt.mod.red)                  #No clustering issues
r2_nakagawa(GULD_centretime75.sqrt.mod.red)           #random structure error


GULD_centretime100.sqrt.mod.red <- lmer(sqrt(centretime100) ~ 
                                          Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|ArenaID) + (1|FishID), data=GULDact.processed)
Anova(GULD_centretime100.sqrt.mod.red)                 #No effects
summary(GULD_centretime100.sqrt.mod.red)               #TrialRound resolves no variance, TrialDay and TankID extremely little
plot(GULD_centretime100.sqrt.mod.red)                  #TrialRound, TrialDay, TankID resolve no variance
r2_nakagawa(GULD_centretime100.sqrt.mod.red)           #random structure error


save(GULD_avespeed_tot.mod.red, file = "./Output_Guldborgsund/GULD_avespeed_tot.mod.red.RData")
save(GULD_avespeed_mob.mod.red, file = "./Output_Guldborgsund/GULD_avespeed_mob.mod.red.RData")
save(GULD_aveacceler.mod.red, file = "./Output_Guldborgsund/GULD_aveacceler.mod.red.RData")
save(GULD_propmoving.invlog.mod.red, file = "./Output_Guldborgsund/GULD_propmoving.invlog.mod.red.RData")
save(GULD_dist.mod.red, file = "./Output_Guldborgsund/GULD_dist.mod.red.RData")
save(GULD_frozenevents.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_frozenevents.sqrt.mod.red.RData")
save(GULD_timefrozen_tot.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_timefrozen_tot.sqrt.mod.red.RData")
save(GULD_centrescore.mod.red, file = "./Output_Guldborgsund/GULD_centrescore.mod.red.RData")
save(GULD_centretime50.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_centretime50.sqrt.mod.red.RData")
save(GULD_centretime75.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_centretime75.sqrt.mod.red.RData")
save(GULD_centretime100.sqrt.mod.red, file = "./Output_Guldborgsund/GULD_centretime100.sqrt.mod.red.RData")



#Assessing distributions (excluding binomial variables)-
#endpointspeed: (s) latency to explore to the endpoint from time of emergence (note: 46 NAs, where fish did not reach the end)
ggplot(GULDexpl.processed) + aes(x = endpointspeed) + geom_histogram(color="black", fill="lightblue", binwidth = 100) + simpletheme 
ggqqplot(GULDexpl.processed$endpointspeed) 
ggplot(GULDexpl.processed) + aes(x = log(endpointspeed)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULDexpl.processed$endpointspeed)) #still some right skew but much better

#refugereturnlat: (s) latency to return to refuge after first emergence (note: 30 NAs, where fish did not return, or emerge at all)
ggplot(GULDexpl.processed) + aes(x = refugereturnlat) + geom_histogram(color="black", fill="lightblue", binwidth = 40) + simpletheme 
ggqqplot(GULDexpl.processed$refugereturnlat) 
ggplot(GULDexpl.processed) + aes(x = log(refugereturnlat)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULDexpl.processed$refugereturnlat)) #near normal



#Full Models- including all random and fixed effects

#  Variable         Transformation    Random effects                                 Fixed effects
#  emergelat.bin    NA (binomial)     TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  endpointlat.bin  NA (binomial)     TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  endpointspeed    log               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore
#  refugereturnlat  log               TankID, TrialDay, TrialRound. ArenaID, FishID  Sex + TL + ConditionFactor + InfectionScore


GULD_endpointspeed.ln.mod <- lmer(log(endpointspeed) ~ 
                                    Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDexpl.processed)
Anova(GULD_endpointspeed.ln.mod)               #No effects
summary(GULD_endpointspeed.ln.mod)             #TankID resolved zero variance, TrialDay resolves extremely low variance
plot(GULD_endpointspeed.ln.mod)                #No issues
r2_nakagawa(GULD_endpointspeed.ln.mod)         #fails


GULD_refugereturnlat.ln.mod <- lmer(log(refugereturnlat) ~ 
                                      Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDexpl.processed)
Anova(GULD_refugereturnlat.ln.mod)               #No effects
summary(GULD_refugereturnlat.ln.mod)             #ArenaID resolves extremely low variance
plot(GULD_refugereturnlat.ln.mod)                #No issues
r2_nakagawa(GULD_refugereturnlat.ln.mod)         #Conditional R2: 0.491, Marginal R2: 0.040


save(GULD_emergelat.bin.mod, file = "./Output_Guldborgsund/GULD_emergelat.bin.mod.RData")
save(GULD_endpointlat.bin.mod, file = "./Output_Guldborgsund/GULD_endpointlat.bin.mod.RData")
save(GULD_endpointspeed.ln.mod, file = "./Output_Guldborgsund/GULD_endpointspeed.ln.mod.RData")
save(GULD_refugereturnlat.ln.mod, file = "./Output_Guldborgsund/GULD_refugereturnlat.ln.mod.RData")



#Reduced Models- excluding random effects that collapse to zero (or extremely close to zero)

#  Variable         Transformation    Random effects                                 Fixed effects
#  emergelat.bin    NA (binomial)     TankID, FishID                                 Sex + TL + ConditionFactor + InfectionScore
#  endpointlat.bin  NA (binomial)     TrialRound, FishID                             Sex + TL + ConditionFactor + InfectionScore
#  endpointspeed    log               TrialRound. ArenaID, FishID                    Sex + TL + ConditionFactor + InfectionScore
#  refugereturnlat  log               TankID, TrialDay, TrialRound, FishID           Sex + TL + ConditionFactor + InfectionScore



GULD_emergelat.bin.mod.red <- glmer(emergelat.bin ~ 
                                      Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|FishID), family = binomial, data=GULDexpl.processed)
Anova(GULD_emergelat.bin.mod.red)                #No effects
summary(GULD_emergelat.bin.mod.red)              #Most variance associated with FishID
plot(GULD_emergelat.bin.mod.red)                 #No issues
r2_nakagawa(GULD_emergelat.bin.mod.red)          #Conditional R2: 0.759, Marginal R2: 0.111
#convergence failure

GULD_endpointlat.bin.mod.red <- glmer(endpointlat.bin~ 
                                        Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialRound) + (1|FishID), family = binomial, data=GULDexpl.processed)
Anova(GULD_endpointlat.bin.mod.red)               #No effects
summary(GULD_endpointlat.bin.mod.red)             #Most variance associated with FishID
plot(GULD_endpointlat.bin.mod.red)                #No issues
r2_nakagawa(GULD_endpointlat.bin.mod.red)         #Conditional R2: 0.784, Marginal R2: 0.102


GULD_endpointspeed.ln.mod.red <- lmer(log(endpointspeed) ~ 
                                        Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TrialRound) + (1|ArenaID) + (1|FishID), data=GULDexpl.processed)
Anova(GULD_endpointspeed.ln.mod.red)               #No effects
summary(GULD_endpointspeed.ln.mod.red)             #FishID is largest component (other than residual)
plot(GULD_endpointspeed.ln.mod.red)                #No issues
r2_nakagawa(GULD_endpointspeed.ln.mod.red)         #Conditional R2: 0.391, Marginal R2: 0.034


GULD_refugereturnlat.ln.mod.red <- lmer(log(refugereturnlat) ~ 
                                          Sex + TL.C + ConditionFactor.C + InfectionScore.C + (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|FishID), data=GULDexpl.processed)
Anova(GULD_refugereturnlat.ln.mod.red)               #No effects
summary(GULD_refugereturnlat.ln.mod.red)             #FishID is largest component (other than residual)
plot(GULD_refugereturnlat.ln.mod.red)                #No issues
r2_nakagawa(GULD_refugereturnlat.ln.mod.red)         #Conditional R2: 0.491, Marginal R2: 0.040


save(GULD_emergelat.bin.mod.red, file = "./Output_Guldborgsund/GULD_emergelat.bin.mod.red.RData")
save(GULD_endpointlat.bin.mod.red, file = "./Output_Guldborgsund/GULD_endpointlat.bin.mod.red.RData")
save(GULD_endpointspeed.ln.mod.red, file = "./Output_Guldborgsund/GULD_endpointspeed.ln.mod.red.RData")
save(GULD_refugereturnlat.ln.mod.red, file = "./Output_Guldborgsund/GULD_refugereturnlat.ln.mod.red.RData")



## G.2.4. EXPL repeatability analysis ----
#Calculating:             
# - Raw repeatability       
# - Adj repeatability (random effects included)


GULD_emergelat.bin.rpt1 <- rpt(emergelat.bin ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                               nboot = 100, npermut = 0)
GULD_emergelat.bin.rpt1
GULD_emergelat.bin.rpt2 <- rpt(emergelat.bin ~ (1|TankID) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                               nboot = 100, npermut = 0)
GULD_emergelat.bin.rpt2

GULD_endpointlat.bin.rpt1 <- rpt(endpointlat.bin ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                                 nboot = 100, npermut = 0)
GULD_endpointlat.bin.rpt1
GULD_endpointlat.bin.rpt2 <- rpt(endpointlat.bin ~ (1|TrialRound) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                                 nboot = 100, npermut = 0)
GULD_endpointlat.bin.rpt2

GULD_endpointspeed.ln.rpt1 <- rpt(log(endpointspeed) ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_endpointspeed.ln.rpt1 #overlaps zero
GULD_endpointspeed.ln.rpt2 <- rpt(log(endpointspeed) ~ (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_endpointspeed.ln.rpt2

GULD_refugereturnlat.ln.rpt1 <- rpt(log(refugereturnlat) ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Gaussian", 
                                    nboot = 100, npermut = 0)
GULD_refugereturnlat.ln.rpt1 #overlaps zero
GULD_refugereturnlat.ln.rpt2 <- rpt(log(refugereturnlat) ~ (1|TankID) + (1|TrialDay) + (1|TrialRound) + (1|FishID), grname = c("FishID"), data = GULDexpl.processed, datatype = "Gaussian", 
                                    nboot = 100, npermut = 0)
GULD_refugereturnlat.ln.rpt2 


save(GULD_emergelat.bin.rpt1, file = "./Output_Guldborgsund/GULD_emergelat.bin.rpt1.RData")
save(GULD_endpointlat.bin.rpt1, file = "./Output_Guldborgsund/GULD_endpointlat.bin.rpt1.RData")
save(GULD_endpointspeed.ln.rpt1, file = "./Output_Guldborgsund/GULD_endpointspeed.ln.rpt1.RData")
save(GULD_refugereturnlat.ln.rpt1, file = "./Output_Guldborgsund/GULD_refugereturnlat.ln.rpt1.RData")

save(GULD_emergelat.bin.rpt2, file = "./Output_Guldborgsund/GULD_emergelat.bin.rpt2.RData")
save(GULD_endpointlat.bin.rpt2, file = "./Output_Guldborgsund/GULD_endpointlat.bin.rpt2.RData")
save(GULD_endpointspeed.ln.rpt2, file = "./Output_Guldborgsund/GULD_endpointspeed.ln.rpt2.RData")
save(GULD_refugereturnlat.ln.rpt2, file = "./Output_Guldborgsund/GULD_refugereturnlat.ln.rpt2.RData")
