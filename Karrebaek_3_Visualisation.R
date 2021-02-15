# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark


library(ggplot2); library(dplyr)


#Karrebaek 3. Visualisation/Presentation ----

#Focus on variables:
#
# dist
# avespeed_mob
# propmoving
# centrescore 
#  
# 
KARRact.processed <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARR_ACTdat_processed.csv")
labels(KARRact.processed)



### K.3.1 Location Maps ----
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html


### K.3.2 Day 2 Behavioural Effects, Full Plots ----
Fig.behav.dist <-  ggplot(KARRact, aes(x = TrialDay, y = (dist), fill=factor(Treatment))) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Trial Day",
        y = "Distance moved (m)")
Fig.behav.dist

Fig.behav.propmoving <-  ggplot(KARRact, aes(x = TrialDay, y = propmoving, fill=factor(Treatment))) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Trial Day",
       y = "Proportion of time moving")
Fig.behav.propmoving

Fig.behav.avespeed_mob <-  ggplot(KARRact, aes(x = TrialDay, y = avespeed_mob, fill=factor(Treatment))) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Trial Day",
       y = "Average speed while mobile (mm/s)")
Fig.behav.avespeed_mob

Fig.behav.centrescore <-  ggplot(KARRact, aes(x = TrialDay, y = centrescore, fill=factor(Treatment))) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Trial Day",
       y = "Centre use score")
Fig.behav.centrescore

ggsave("Output_Karrebaek/Fig.behav.dist.jpg", width = 15, height = 8, units = "cm", Fig.behav.dist, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.propmoving.jpg", width = 15, height = 8, units = "cm", Fig.behav.propmoving, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.avespeed_mob.jpg", width = 15, height = 8, units = "cm", Fig.behav.avespeed_mob, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.centrescore.jpg", width = 15, height = 8, units = "cm", Fig.behav.centrescore, dpi = 600)


### K.3.3 Day 2 Behavioural Effects, visualizing effect estimates ----

load("./Output_Karrebaek/KARR_dist.sqrt.red.Rdata")
load("./Output_Karrebaek/KARR_propmoving.sqrt.red.Rdata")
load("./Output_Karrebaek/KARR_avespeed_mob.mod.red.Rdata")
load("./Output_Karrebaek/KARR_centrescore.sqrt.red.Rdata")

#Calculating effect estimates and confidence intervals
summary(KARR_dist.sqrt.red)
confint(KARR_dist.sqrt.red)

summary(KARR_propmoving.sqrt.red)
confint(KARR_propmoving.sqrt.red)

summary(KARR_avespeed_mob.mod.red)
confint(KARR_avespeed_mob.mod.red)

summary(KARR_centrescore.sqrt.red)
confint(KARR_centrescore.sqrt.red)


# Creating databases for changes in behaviour
KARRact.processed0 <- subset(KARRact.processed, TrialDay == 'Day 0')
KARRact.processed2 <- subset(KARRact.processed, TrialDay == 'Day 2')
KARRact.processed10 <- subset(KARRact.processed, TrialDay == 'Day 10')
labels(KARRact.processed0)

KARRact.processed2 <- select(KARRact.processed2, -c(TankID.combo, ConditionFactor, TrialDay, TrialRound, ArenaID, TankID, PITID, Treatment, 
                                Sex, Notes, Date, TimeLoaded, TrialType, TL,  SL, Weight, InfectionScore, X, TL.C, ConditionFactor.C, InfectionScore.C))
KARRact.processed2<- rename(KARRact.processed2, UniqueID_Day2 = UniqueID)

KARRact.processed10 <- select(KARRact.processed10, -c(TankID.combo, ConditionFactor, TrialDay, TrialRound, ArenaID, TankID, PITID, Treatment, 
                                  Sex, Notes, Date, TimeLoaded, TrialType, TL,  SL, Weight, InfectionScore, X, TL.C, ConditionFactor.C, InfectionScore.C))
KARRact.processed10<- rename(KARRact.processed10, UniqueID_Day10 = UniqueID)


#Using the change in post-trial behaviour from pre-trial behaviour
KARRact.processed.RESP <- merge(KARRact.processed0, KARRact.processed2, by = 'FishID', all.x = TRUE)
labels(KARRact.processed.RESP)
KARRact.processed.RESP$dist.2 <- KARRact.processed.RESP$dist.y-KARRact.processed.RESP$dist.x
KARRact.processed.RESP$propmoving.2 <- KARRact.processed.RESP$propmoving.y-KARRact.processed.RESP$propmoving.x
KARRact.processed.RESP$avespeed_mob.2 <- KARRact.processed.RESP$avespeed_mob.y-KARRact.processed.RESP$avespeed_mob.x
KARRact.processed.RESP$centrescore.2 <- KARRact.processed.RESP$centrescore.y-KARRact.processed.RESP$centrescore.x


KARRact.processed.RESP <- merge(KARRact.processed.RESP, KARRact.processed10, by = 'FishID', all.x = TRUE)
labels(KARRact.processed.RESP)
KARRact.processed.RESP$dist.10 <- KARRact.processed.RESP$dist-KARRact.processed.RESP$dist.x
KARRact.processed.RESP$propmoving.10 <- KARRact.processed.RESP$propmoving-KARRact.processed.RESP$propmoving.x
KARRact.processed.RESP$avespeed_mob.10 <- KARRact.processed.RESP$avespeed_mob-KARRact.processed.RESP$avespeed_mob.x
KARRact.processed.RESP$centrescore.10 <- KARRact.processed.RESP$centrescore-KARRact.processed.RESP$centrescore.x


KARRact.processed.RESP <- select(KARRact.processed.RESP, -c(X, avespeed_tot, avespeed_mob, propmoving,  dist, centretime50, centretime75, 
                                        centretime100, centretime100.y, avespeed_tot.y, avespeed_mob.y, propmoving.y, 
                                        dist.y, timefrozen_tot.y, centretime50.y, centretime75.y, centrescore.y, avespeed_tot.x,
                                        timefrozen_tot.x, centretime50.x, centretime75.x, 
                                        centretime100.x, timefrozen_tot, centrescore))
KARRact.processed.RESP<- rename(KARRact.processed.RESP, UniqueID_Day0 = UniqueID)
labels(KARRact.processed.RESP)


#reordering some variables
KARRact.processed.RESP <- KARRact.processed.RESP %>% relocate(UniqueID_Day2, .after = UniqueID_Day0)
KARRact.processed.RESP <- KARRact.processed.RESP %>% relocate(UniqueID_Day10, .after = UniqueID_Day2)
KARRact.processed.RESP <- KARRact.processed.RESP %>% relocate(ConditionFactor, .after = Weight)
KARRact.processed.RESP <- KARRact.processed.RESP %>% relocate(TankID.combo, .after = TankID)
KARRact.processed.RESP$Treatment <- ordered(KARRact.processed.RESP$Treatment, levels = c("Control","PITtagged","PIT+clip"))

##Visualising responses across treatment groups, Day 2
Fig.behav.dist.resp2 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = dist.2, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "     Δ distance explored 
       (Day 2 - Day 0)")
Fig.behav.dist.resp2

Fig.behav.propmoving.resp2 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = propmoving.2, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "     Δ proportion moving 
       (Day 2 - Day 0)")
Fig.behav.propmoving.resp2

Fig.behav.avespeed_mob.resp2 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = avespeed_mob.2, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "      Δ mobility speed 
       (Day 2 - Day 0)")
Fig.behav.avespeed_mob.resp2

Fig.behav.centrescore.resp2 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = centrescore.2, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ centre score 
       (Day 2 - Day 0)")
Fig.behav.centrescore.resp2

##Visualising responses across treatment groups, Day 10
Fig.behav.dist.resp10 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = dist.10, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "     Δ distance explored 
       (Day 10 - Day 0)")
Fig.behav.dist.resp10

Fig.behav.propmoving.resp10 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = propmoving.10, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "     Δ proportion moving 
       (Day 2 - Day 0)")
Fig.behav.propmoving.resp10

Fig.behav.avespeed_mob.resp10 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = avespeed_mob.10, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "      Δ mobility speed 
       (Day 2 - Day 0)")
Fig.behav.avespeed_mob.resp10

Fig.behav.centrescore.resp10 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = centrescore.10, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ centre score 
       (Day 2 - Day 0)")
Fig.behav.centrescore.resp10


ggsave("Output_Karrebaek/Fig.behav.dist.resp2.jpg", width = 14, height = 8, units = "cm", Fig.behav.dist.resp2, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.propmoving.resp2.jpg", width = 14, height = 8, units = "cm", Fig.behav.propmoving.resp2, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.avespeed_mob.resp2.jpg", width = 14, height = 8, units = "cm", Fig.behav.avespeed_mob.resp2, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.centrescore.resp2.jpg", width = 14, height = 8, units = "cm", Fig.behav.centrescore.resp2, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.dist.resp10.jpg", width = 14, height = 8, units = "cm", Fig.behav.dist.resp10, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.propmoving.resp10.jpg", width = 14, height = 8, units = "cm", Fig.behav.propmoving.resp10, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.avespeed_mob.resp10.jpg", width = 14, height = 8, units = "cm", Fig.behav.avespeed_mob.resp10, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.centrescore.resp10.jpg", width = 14, height = 8, units = "cm", Fig.behav.centrescore.resp10, dpi = 600)




### K.3.4 Visualising growth effects ----

load("./Output_Karrebaek/KARR_treat.dTL.RData")
load("./Output_Karrebaek/KARR_treat.dSL.RData")
load("./Output_Karrebaek/KARR_treat.dWeight.RData")
load("./Output_Karrebaek/KARR_treat.dConditionFactor.RData")

#Calculating effect estimates and confidence intervals
Anova(KARR_treat.dTL)
summary(KARR_treat.dTL)
confint(KARR_treat.dTL)

summary(KARR_propmoving.sqrt.red)
confint(KARR_propmoving.sqrt.red)

summary(KARR_avespeed_mob.mod.red)
confint(KARR_avespeed_mob.mod.red)

summary(KARR_centrescore.sqrt.red)
confint(KARR_centrescore.sqrt.red)


KARRgrowth <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARRgrowth.csv")
KARRgrowth$Treatment <- ordered(KARRgrowth$Treatment, levels = c("Control","PITtagged","PIT+clip"))

Fig.treat.dTL <- ggplot(KARRgrowth, aes(x = Treatment, y = dTL, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ total length (cm)
       (Day 70 - Day 0)")
Fig.treat.dTL

Fig.treat.dSL <- ggplot(KARRgrowth, aes(x = Treatment, y = dSL, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ standard length (cm)
       (Day 70 - Day 0)")
Fig.treat.dSL

Fig.treat.dWeight <- ggplot(KARRgrowth, aes(x = Treatment, y = dWeight, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ weight (g)
       (Day 70 - Day 0)")
Fig.treat.dWeight

Fig.treat.dConditionFactor <- ggplot(KARRgrowth, aes(x = Treatment, y = dConditionFactor, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ condition factor
       (Day 70 - Day 0)")
Fig.treat.dConditionFactor


ggsave("Output_Karrebaek/Fig.treat.dTL.jpg", width = 14, height = 8, units = "cm", Fig.treat.dTL, dpi = 600)
ggsave("Output_Karrebaek/Fig.treat.dSL.jpg", width = 14, height = 8, units = "cm", Fig.treat.dSL, dpi = 600)
ggsave("Output_Karrebaek/Fig.treat.dWeight.jpg", width = 14, height = 8, units = "cm", Fig.treat.dWeight, dpi = 600)
ggsave("Output_Karrebaek/Fig.treat.dConditionFactor.jpg", width = 14, height = 8, units = "cm", Fig.treat.dConditionFactor, dpi = 600)




### K.3.5 Visualising initial differences in behaviour ----
Fig.behav.dist.init <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = dist.x, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "Day 0 distance explored (mm)")
Fig.behav.dist.init

KARRact.processed.RESP$Treatment <- as.numeric(KARRact.processed.RESP$Treatment)
KARRact.processed.RESP$Treatment <- as.factor(KARRact.processed.RESP$Treatment)

Mod.behav.dist.init <- lmer(sqrt(dist.x) ~ 
                                  Sex + TL.C + ConditionFactor.C + InfectionScore.C + Treatment + (1|TankID.combo) + (1|ArenaID) + (1|TrialRound), data=KARRact.processed.RESP)
Anova(Mod.behav.dist.init)
summary(Mod.behav.dist.init)
plot(Mod.behav.dist.init)

Mod.behav.dist.init.red <- lmer(sqrt(dist.x) ~ 
                                  Treatment + (1|TankID.combo) + (1|TrialRound), data=KARRact.processed.RESP)
Anova(Mod.behav.dist.init.red)
summary(Mod.behav.dist.init.red)
plot(Mod.behav.dist.init.red)


