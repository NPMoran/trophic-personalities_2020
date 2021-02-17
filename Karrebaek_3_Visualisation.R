# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#### Karrebaek 3. Visualisation/Presentation ####

Sys.setenv(LANG = "en")

#Loading required packages- 
library(ggplot2); library(dplyr); library(car); library(performance)


## K.3.1 Full Treatment*TrialDay Plots ----
KARRact.processed <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARR_ACTdat_processed.csv")
KARRact.processed$TrialDay <- ordered(KARRact.processed$TrialDay, levels = c("day 0","day 2","day 10"))
KARRact.processed$Treatment <- ordered(KARRact.processed$Treatment, levels = c("control","PITtagged","PIT+clip"))

Fig.behav.dist <-  ggplot(KARRact.processed, aes(x = TrialDay, y = (dist), fill=factor(Treatment))) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank(), legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Trial day",
       y = 'Distance (mm)')
Fig.behav.dist

Fig.behav.propmoving <-  ggplot(KARRact.processed, aes(x = TrialDay, y = propmoving, fill=factor(Treatment))) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank(), legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Trial day",
       y = "Proportion of time moving")
Fig.behav.propmoving

Fig.behav.avespeed_mob <-  ggplot(KARRact.processed, aes(x = TrialDay, y = avespeed_mob, fill=factor(Treatment))) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank(), legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Trial day",
       y = expression(paste('Mobility speed ', (mm.s^-1))))
Fig.behav.avespeed_mob

Fig.behav.centrescore <-  ggplot(KARRact.processed, aes(x = TrialDay, y = centrescore, fill=factor(Treatment))) + 
  geom_boxplot() + simpletheme + theme(legend.title = element_blank(), legend.key = element_rect(fill = "transparent", colour = "transparent")) +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Trial day",
       y = "Centre use score")
Fig.behav.centrescore

ggsave("Output_Karrebaek/Fig.behav.dist.jpg", width = 15, height = 8, units = "cm", Fig.behav.dist, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.propmoving.jpg", width = 15, height = 8, units = "cm", Fig.behav.propmoving, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.avespeed_mob.jpg", width = 15, height = 8, units = "cm", Fig.behav.avespeed_mob, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.centrescore.jpg", width = 15, height = 8, units = "cm", Fig.behav.centrescore, dpi = 600)


## K.3.2 Visualizing treatment effect estimates ----

load("./Output_Karrebaek/KARR_dist.sqrt.red.Rdata")
load("./Output_Karrebaek/KARR_propmoving.sqrt.red.Rdata")
load("./Output_Karrebaek/KARR_avespeed_mob.mod.red.Rdata")
load("./Output_Karrebaek/KARR_centrescore.sqrt.red.Rdata")

#Calculating effect estimates and confidence intervals
Anova(KARR_dist.sqrt.red)
summary(KARR_dist.sqrt.red)
confint(KARR_dist.sqrt.red)
r2_nakagawa(KARR_dist.sqrt.red)

Anova(KARR_propmoving.sqrt.red)
summary(KARR_propmoving.sqrt.red)
confint(KARR_propmoving.sqrt.red)
r2_nakagawa(KARR_propmoving.sqrt.red)

Anova(KARR_avespeed_mob.mod.red)
summary(KARR_avespeed_mob.mod.red)
confint(KARR_avespeed_mob.mod.red)
r2_nakagawa(KARR_avespeed_mob.mod.red)

Anova(KARR_centrescore.sqrt.red)
summary(KARR_centrescore.sqrt.red)
confint(KARR_centrescore.sqrt.red)
r2_nakagawa(KARR_centrescore.sqrt.red)


# Creating databases for changes in behaviour
KARRact.processed0 <- subset(KARRact.processed, TrialDay == 'day 0')
KARRact.processed2 <- subset(KARRact.processed, TrialDay == 'day 2')
KARRact.processed10 <- subset(KARRact.processed, TrialDay == 'day 10')
labels(KARRact.processed0)

KARRact.processed2 <- select(KARRact.processed2, -c(TankID.combo, ConditionFactor, TrialDay, TrialRound, ArenaID, TankID, PITID, Treatment, 
                                Sex, Notes, Date, TimeLoaded, TrialType, TL,  SL, Weight, InfectionScore, X))
KARRact.processed2<- rename(KARRact.processed2, UniqueID_Day2 = UniqueID)

KARRact.processed10 <- select(KARRact.processed10, -c(TankID.combo, ConditionFactor, TrialDay, TrialRound, ArenaID, TankID, PITID, Treatment, 
                                  Sex, Notes, Date, TimeLoaded, TrialType, TL,  SL, Weight, InfectionScore, X))
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


KARRact.processed.RESP <- select(KARRact.processed.RESP, -c(X, avespeed_mob, propmoving,  dist, avespeed_mob.y, propmoving.y, 
                                        dist.y, centrescore.y, centrescore))
KARRact.processed.RESP<- rename(KARRact.processed.RESP, UniqueID_Day0 = UniqueID)
labels(KARRact.processed.RESP)


#reordering some variables
KARRact.processed.RESP <- KARRact.processed.RESP %>% relocate(UniqueID_Day2, .after = UniqueID_Day0)
KARRact.processed.RESP <- KARRact.processed.RESP %>% relocate(UniqueID_Day10, .after = UniqueID_Day2)
KARRact.processed.RESP <- KARRact.processed.RESP %>% relocate(ConditionFactor, .after = Weight)
KARRact.processed.RESP <- KARRact.processed.RESP %>% relocate(TankID.combo, .after = TankID)
KARRact.processed.RESP$Treatment <- ordered(KARRact.processed.RESP$Treatment, levels = c("control","PITtagged","PIT+clip"))

##Visualising responses across treatment groups, Day 2
Fig.behav.dist.resp2 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = dist.2, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "      Δ Distance 
       (day 2 - day 0)")
Fig.behav.dist.resp2

Fig.behav.propmoving.resp2 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = propmoving.2, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "     Δ Proportion of time moving 
       (day 2 - day 0)")
Fig.behav.propmoving.resp2

Fig.behav.avespeed_mob.resp2 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = avespeed_mob.2, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "      Δ Mobility speed 
       (day 2 - day 0)")
Fig.behav.avespeed_mob.resp2

Fig.behav.centrescore.resp2 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = centrescore.2, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ Centre use score 
       (day 2 - day 0)")
Fig.behav.centrescore.resp2

##Visualising responses across treatment groups, Day 10
Fig.behav.dist.resp10 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = dist.10, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "      Δ Distance 
       (day 10 - day 0)")
Fig.behav.dist.resp10

Fig.behav.propmoving.resp10 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = propmoving.10, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "     Δ Proportion of time moving 
       (day 10 - day 0)")
Fig.behav.propmoving.resp10

Fig.behav.avespeed_mob.resp10 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = avespeed_mob.10, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "      Δ Mobility speed 
       (day 10 - day 0)")
Fig.behav.avespeed_mob.resp10

Fig.behav.centrescore.resp10 <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = centrescore.10, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ Centre use score 
       (day 10 - day 0)")
Fig.behav.centrescore.resp10


ggsave("Output_Karrebaek/Fig.behav.dist.resp2.jpg", width = 10, height = 8, units = "cm", Fig.behav.dist.resp2, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.propmoving.resp2.jpg", width = 10, height = 8, units = "cm", Fig.behav.propmoving.resp2, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.avespeed_mob.resp2.jpg", width = 10, height = 8, units = "cm", Fig.behav.avespeed_mob.resp2, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.centrescore.resp2.jpg", width = 10, height = 8, units = "cm", Fig.behav.centrescore.resp2, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.dist.resp10.jpg", width = 10, height = 8, units = "cm", Fig.behav.dist.resp10, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.propmoving.resp10.jpg", width = 10, height = 8, units = "cm", Fig.behav.propmoving.resp10, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.avespeed_mob.resp10.jpg", width = 10, height = 8, units = "cm", Fig.behav.avespeed_mob.resp10, dpi = 600)
ggsave("Output_Karrebaek/Fig.behav.centrescore.resp10.jpg", width = 10, height = 8, units = "cm", Fig.behav.centrescore.resp10, dpi = 600)



## K.3.3 Visualising growth effects ----

load("./Output_Karrebaek/KARR_treat.dTL.RData")
load("./Output_Karrebaek/KARR_treat.dSL.RData")
load("./Output_Karrebaek/KARR_treat.dWeight.RData")
load("./Output_Karrebaek/KARR_treat.dConditionFactor.RData")

#Calculating effect estimates and confidence intervals
Anova(KARR_treat.dTL)
summary(KARR_treat.dTL)
confint(KARR_treat.dTL)
r2_nakagawa(KARR_treat.dTL)

Anova(KARR_treat.dSL)
summary(KARR_treat.dSL)
confint(KARR_treat.dSL)
r2_nakagawa(KARR_treat.dSL)

Anova(KARR_treat.dWeight)
summary(KARR_treat.dWeight)
confint(KARR_treat.dWeight)
r2_nakagawa(KARR_treat.dWeight)

Anova(KARR_treat.dConditionFactor)
summary(KARR_treat.dConditionFactor)
confint(KARR_treat.dConditionFactor)
r2_nakagawa(KARR_treat.dConditionFactor)


#Visualising growth variables
KARRgrowth <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARRgrowth.csv")
KARRgrowth$Treatment <- ordered(KARRgrowth$Treatment, levels = c("control","PITtagged","PIT+clip"))


Fig.treat.dTL <- ggplot(KARRgrowth, aes(x = Treatment, y = dTL, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ Total length (cm)
       (day 70 - day 0)")
Fig.treat.dTL

Fig.treat.dSL <- ggplot(KARRgrowth, aes(x = Treatment, y = dSL, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ Standard length (cm)
       (day 70 - day 0)")
Fig.treat.dSL

Fig.treat.dWeight <- ggplot(KARRgrowth, aes(x = Treatment, y = dWeight, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ Weight (g)
       (day 70 - day 0)")
Fig.treat.dWeight

Fig.treat.dConditionFactor <- ggplot(KARRgrowth, aes(x = Treatment, y = dConditionFactor, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "        Δ Condition factor
       (day 70 - day 0)")
Fig.treat.dConditionFactor


ggsave("Output_Karrebaek/Fig.treat.dTL.jpg", width = 10, height = 8, units = "cm", Fig.treat.dTL, dpi = 600)
ggsave("Output_Karrebaek/Fig.treat.dSL.jpg", width = 10, height = 8, units = "cm", Fig.treat.dSL, dpi = 600)
ggsave("Output_Karrebaek/Fig.treat.dWeight.jpg", width = 10, height = 8, units = "cm", Fig.treat.dWeight, dpi = 600)
ggsave("Output_Karrebaek/Fig.treat.dConditionFactor.jpg", width = 10, height = 8, units = "cm", Fig.treat.dConditionFactor, dpi = 600)




## K.3.4 Visualising initial differences in behaviour ----
Fig.behav.dist.init <- ggplot(KARRact.processed.RESP, aes(x = Treatment, y = dist.x, fill = Treatment)) + 
  geom_boxplot() + simpletheme + theme(legend.position = "none") +
  scale_fill_manual(values=c("white","seashell2","red")) + 
  labs(x = "Treatment",
       y = "Day 0 distance explored (mm)")
Fig.behav.dist.init

KARRact.processed.RESP$Treatment <- as.numeric(KARRact.processed.RESP$Treatment)
KARRact.processed.RESP$Treatment <- as.factor(KARRact.processed.RESP$Treatment)

KARRact.processed.RESP$TL.C <- scale(KARRact.processed.RESP$TL)
KARRact.processed.RESP$ConditionFactor.C <- scale(KARRact.processed.RESP$ConditionFactor)
KARRact.processed.RESP$InfectionScore.C <- scale(KARRact.processed.RESP$InfectionScore)

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



#### #### 


