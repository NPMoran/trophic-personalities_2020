# Title: Behavioural and trophic variation within a well-established invasive round goby population 
# Author: Nicholas Moran
# Date: March 2023



#### 2. Behavioural analysis ####
Sys.setenv(LANG = "en")
#Loading required packages- 
library(dplyr); library(ggplot2); library(ggpubr)
library(lme4); library(lmerTest); library(car); library(rptR); library(performance)
library(vegan); library(data.table)
#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


# 2.1. Activity, distributions and variance analysis ----
GULDact.processed <- read.csv("~/trophic-personalities_2020/dat_behaviour/GULD_ACTdat_processed.csv")
nrow(GULDact.processed) #118 rows
n_distinct(GULDact.processed$FishID) #43 fish included in analysis

# - Z-transformation/scaling of continuous fixed effects
GULDact.processed$TL.C <- scale(GULDact.processed$TL)  
GULDact.processed$CondManual.C <- scale(GULDact.processed$CondManual)  
GULDact.processed$TrialDay.C <- scale(GULDact.processed$TrialDay)  

# - Making TrialRound a unique variable for each round
GULDact.processed$TrialRound <- paste(GULDact.processed$TrialDay, GULDact.processed$TrialRound, sep = "_")


#    - Total distance moved accross the trial (mm, 'dist') ----
# - Assessing distributions
#ggplot(GULDact.processed) + aes(x = dist) + geom_histogram(color="black", fill="lightblue", binwidth = 4800) + simpletheme 
#ggqqplot(GULDact.processed$dist)
#
## - Full model
#GULD_dist.mod <- lmer(dist ~ 
#                             Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
#summary(GULD_dist.mod) #TrialRound, TankID extremely low to zero variance explained
#plot(GULD_dist.mod) #No issues
#
## - Reduced model
#GULD_dist.mod.red <- lmer(dist ~ Sex + TL.C + CondManual.C + TrialDay.C + (1|ArenaID) + (1|FishID), data=GULDact.processed)
#summary(GULD_dist.mod.red)  #Sex and replicate effect, marginal condition effect
#r2_nakagawa(GULD_dist.mod.red) 
#
## - Repeatabilities
#GULD_dist.mod.rpt1 <- rpt(dist  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
#                          nboot = 100, npermut = 0)
#GULD_dist.mod.rpt1
#GULD_dist.mod.rpt2 <- rpt(dist ~ 
#                            (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
#                          nboot = 100, npermut = 0)
#GULD_dist.mod.rpt2
#
#save(GULD_dist.mod, file = "./outputs_visualisations/GULD_dist.mod.RData")
#save(GULD_dist.mod.red, file = "./outputs_visualisations/GULD_dist.mod.red.RData")
#save(GULD_dist.mod.rpt1, file = "./outputs_visualisations/GULD_dist.mod.rpt1.RData")
#save(GULD_dist.mod.rpt2, file = "./outputs_visualisations/GULD_dist.mod.rpt2.RData")

load(file = "./outputs_visualisations/GULD_dist.mod.RData")
load(file = "./outputs_visualisations/GULD_dist.mod.red.RData")
load(file = "./outputs_visualisations/GULD_dist.mod.rpt1.RData")
load(file = "./outputs_visualisations/GULD_dist.mod.rpt2.RData")


#    - Average speed (mm/m, 'avespeed_mob') ----
#ggplot(GULDact.processed) + aes(x = avespeed_mob) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
#ggqqplot(GULDact.processed$avespeed_mob) #minor issue with 4-5 outliers at the very low end
#
#GULD_avespeed_mob.mod <- lmer(avespeed_mob ~ 
#                                Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
#summary(GULD_avespeed_mob.mod)  #TrialRound, TankID, ArenaID extremely low to zero variance explained
#plot(GULD_avespeed_mob.mod)  #Small number of low end speed leading to some asymmetry
#
#GULD_avespeed_mob.mod.red <- lmer(avespeed_mob ~ 
#                                Sex + TL.C + CondManual.C + TrialDay.C + (1|FishID),  data=GULDact.processed)
#summary(GULD_avespeed_mob.mod.red)  #Sex and replicate effect, 
#r2_nakagawa(GULD_avespeed_mob.mod.red) 
#
#GULD_avespeed_mob.mod.rpt1 <- rpt(avespeed_mob ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
#                                  nboot = 100, npermut = 0)
#GULD_avespeed_mob.mod.rpt1
#GULD_avespeed_mob.mod.rpt2 <- rpt(avespeed_mob ~ 
#                                    (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
#                                  nboot = 100, npermut = 0)
#GULD_avespeed_mob.mod.rpt2

#save(GULD_avespeed_mob.mod, file = "./outputs_visualisations/GULD_avespeed_mob.mod.RData")
#save(GULD_avespeed_mob.mod.red, file = "./outputs_visualisations/GULD_avespeed_mob.mod.red.RData")
#save(GULD_avespeed_mob.mod.rpt1, file = "./outputs_visualisations/GULD_avespeed_mob.mod.rpt1.RData")
#save(GULD_avespeed_mob.mod.rpt2, file = "./outputs_visualisations/GULD_avespeed_mob.mod.rpt2.RData")

load(file = "./outputs_visualisations/GULD_avespeed_mob.mod.RData")
load(file = "./outputs_visualisations/GULD_avespeed_mob.mod.red.RData")
load(file = "./outputs_visualisations/GULD_avespeed_mob.mod.rpt1.RData")
load(file = "./outputs_visualisations/GULD_avespeed_mob.mod.rpt2.RData")


#    - Time frozen (s, 'timefrozen_tot') ----
#ggplot(GULDact.processed) + aes(x = timefrozen_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 85) + simpletheme 
#ggqqplot(GULDact.processed$timefrozen_tot)
#ggplot(GULDact.processed) + aes(x = log(timefrozen_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.3) + simpletheme 
#ggqqplot(log(GULDact.processed$timefrozen_tot)) #still some skew bit much closer to normal
#
#GULD_timefrozen_tot.mod <- lmer(log(timefrozen_tot) ~ 
#                                       Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
#summary(GULD_timefrozen_tot.mod)  #TankID resolve no variance
#plot(GULD_timefrozen_tot.mod)  #No issues
#
#GULD_timefrozen_tot.mod.red <- lmer(log(timefrozen_tot) ~ 
#                                           Sex + TL.C + CondManual.C + TrialDay.C + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
#summary(GULD_timefrozen_tot.mod.red)  #Sex, condition, and replicate effects
#r2_nakagawa(GULD_timefrozen_tot.mod.red) 
#
#GULD_timefrozen_tot.mod.rpt1 <- rpt(log(timefrozen_tot) ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
#                                  nboot = 100, npermut = 0)
#GULD_timefrozen_tot.mod.rpt1
#GULD_timefrozen_tot.mod.rpt2 <- rpt(log(timefrozen_tot) ~ 
#                                    (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
#                                  nboot = 100, npermut = 0)
#GULD_timefrozen_tot.mod.rpt2

#save(GULD_timefrozen_tot.mod, file = "./outputs_visualisations/GULD_timefrozen_tot.mod.RData")
#save(GULD_timefrozen_tot.mod.red, file = "./outputs_visualisations/GULD_timefrozen_tot.mod.red.RData")
#save(GULD_timefrozen_tot.mod.rpt1, file = "./outputs_visualisations/GULD_timefrozen_tot.mod.rpt1.RData")
#save(GULD_timefrozen_tot.mod.rpt2, file = "./outputs_visualisations/GULD_timefrozen_tot.mod.rpt2.RData")

load(file = "./outputs_visualisations/GULD_timefrozen_tot.mod.RData")
load(file = "./outputs_visualisations/GULD_timefrozen_tot.mod.red.RData")
load(file = "./outputs_visualisations/GULD_timefrozen_tot.mod.rpt1.RData")
load(file = "./outputs_visualisations/GULD_timefrozen_tot.mod.rpt2.RData")


#    - Edge use, (mm, 'centrescore') ---- 
#ggplot(GULDact.processed) + aes(x = centrescore2) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
#ggqqplot(GULDact.processed$centrescore2) #potential minimal positive skew
#
#GULD_centrescore.mod <- lmer(centrescore2 ~ 
#                                Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
#summary(GULD_centrescore.mod)   #TankID and TrialRound resolve no variance
#plot(GULD_centrescore.mod)  #No issues
#
#GULD_centrescore.mod.red <- lmer(centrescore ~ 
#                                Sex + TL.C + CondManual.C + TrialDay.C + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
#summary(GULD_centrescore.mod.red)  #length effect
#r2_nakagawa(GULD_centrescore.mod.red) 
#
#GULD_centrescore.mod.rpt1 <- rpt(centrescore2  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
#                                 nboot = 100, npermut = 0)
#GULD_centrescore.mod.rpt1
#GULD_centrescore.mod.rpt2 <- rpt(centrescore2 ~ 
#                                   (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
#                                 nboot = 100, npermut = 0)
#GULD_centrescore.mod.rpt2
#
#save(GULD_centrescore.mod, file = "./outputs_visualisations/GULD_centrescore.mod.RData")
#save(GULD_centrescore.mod.red, file = "./outputs_visualisations/GULD_centrescore.mod.red.RData")
#save(GULD_centrescore.mod.rpt1, file = "./outputs_visualisations/GULD_centrescore.mod.rpt1.RData")
#save(GULD_centrescore.mod.rpt2, file = "./outputs_visualisations/GULD_centrescore.mod.rpt2.RData")

load(file = "./outputs_visualisations/GULD_centrescore.mod.RData")
load(file = "./outputs_visualisations/GULD_centrescore.mod.red.RData")
load(file = "./outputs_visualisations/GULD_centrescore.mod.rpt1.RData")
load(file = "./outputs_visualisations/GULD_centrescore.mod.rpt2.RData")


# 2.2. EXPL distributions and variance analysis ----
GULDexpl.processed <- read.csv("~/trophic-personalities_2020/dat_behaviour/GULD_EXPLdat_processed.csv")
nrow(GULDexpl.processed) #111 trials
n_distinct(GULDexpl.processed$FishID) #43 fish included in analysis


# - Z-transformation/scaling of continuous fixed effects
GULDexpl.processed$TL.C <- scale(GULDexpl.processed$TL)  
GULDexpl.processed$CondManual.C <- scale(GULDexpl.processed$CondManual)  
GULDexpl.processed$TrialDay.C <- scale(GULDexpl.processed$TrialDay)  

# - Making TrialRound a unique variable for each round
GULDexpl.processed$TrialRound <- paste(GULDexpl.processed$TrialDay, GULDexpl.processed$TrialRound, sep = "_")

#    - Latency to emerge from the refuge (binary, 'emergelat.bin') ----
#GULD_emergelat.bin.mod <- glmer(emergelat.bin ~ 
#                                  Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), family = binomial, data=GULDexpl.processed)
#summary(GULD_emergelat.bin.mod) #TrialRound, ArenaID and TrialDay resolve extremely low variance
#plot(GULD_emergelat.bin.mod)              
#
#GULD_emergelat.bin.mod.red <- glmer(emergelat.bin ~ 
#                                  Sex + TL.C + CondManual.C + TrialDay.C + (1|FishID), family = binomial, data=GULDexpl.processed)
#summary(GULD_emergelat.bin.mod.red) #no effects
#r2_nakagawa(GULD_emergelat.bin.mod.red)              
#
#GULD_emergelat.bin.mod.rpt1 <- rpt(emergelat.bin ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
#                               nboot = 100, npermut = 0)
#GULD_emergelat.bin.mod.rpt1
#GULD_emergelat.bin.mod.rpt2 <- rpt(emergelat.bin ~ 
#                                 (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
#                               nboot = 100, npermut = 0)
#GULD_emergelat.bin.mod.rpt2
#
#save(GULD_emergelat.bin.mod, file = "./outputs_visualisations/GULD_emergelat.bin.mod.RData")
#save(GULD_emergelat.bin.mod.red, file = "./outputs_visualisations/GULD_emergelat.bin.mod.red.RData")
#save(GULD_emergelat.bin.mod.rpt1, file = "./outputs_visualisations/GULD_emergelat.bin.mod.rpt1.RData")
#save(GULD_emergelat.bin.mod.rpt2, file = "./outputs_visualisations/GULD_emergelat.bin.mod.rpt2.RData")

load(file = "./outputs_visualisations/GULD_emergelat.bin.mod.RData")
load(file = "./outputs_visualisations/GULD_emergelat.bin.mod.red.RData")
load(file = "./outputs_visualisations/GULD_emergelat.bin.mod.rpt1.RData")
load(file = "./outputs_visualisations/GULD_emergelat.bin.mod.rpt2.RData")



#    - Latency to explore to the endpoint of the arena (binary, 'emergelat.bin') ----
#GULD_endpointlat.bin.mod <- glmer(endpointlat.bin ~ 
#                                  Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), family = binomial, data=GULDexpl.processed)
#summary(GULD_endpointlat.bin.mod) #TrialRound, ArenaID and TrialDay resolve extremely low variance
#plot(GULD_endpointlat.bin.mod)              
#
#GULD_endpointlat.bin.mod.red <- glmer(endpointlat.bin ~ 
#                                      Sex + TL.C + CondManual.C + TrialDay.C + (1|FishID), family = binomial, data=GULDexpl.processed)
#summary(GULD_endpointlat.bin.mod.red) #no effects
#r2_nakagawa(GULD_endpointlat.bin.mod.red)              
#
#GULD_endpointlat.bin.mod.rpt1 <- rpt(endpointlat.bin ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
#                               nboot = 100, npermut = 0)
#GULD_endpointlat.bin.mod.rpt1
#GULD_endpointlat.bin.mod.rpt2 <- rpt(endpointlat.bin ~ 
#                                 (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
#                               nboot = 100, npermut = 0)
#GULD_endpointlat.bin.mod.rpt2

#save(GULD_endpointlat.bin.mod, file = "./outputs_visualisations/GULD_endpointlat.bin.mod.RData")
#save(GULD_endpointlat.bin.mod.red, file = "./outputs_visualisations/GULD_endpointlat.bin.mod.red.RData")
#save(GULD_endpointlat.bin.mod.rpt1, file = "./outputs_visualisations/GULD_endpointlat.bin.mod.rpt1.RData")
#save(GULD_endpointlat.bin.mod.rpt2, file = "./outputs_visualisations/GULD_endpointlat.bin.mod.rpt2.RData")

load(file = "./outputs_visualisations/GULD_endpointlat.bin.mod.RData")
load(file = "./outputs_visualisations/GULD_endpointlat.bin.mod.red.RData")
load(file = "./outputs_visualisations/GULD_endpointlat.bin.mod.rpt1.RData")
load(file = "./outputs_visualisations/GULD_endpointlat.bin.mod.rpt2.RData")



# 2.3. Extracting data for tables ----
#    - Table 1 ----

name <- as.data.frame(c("GULD_dist.mod.rpt",
                        "GULD_avespeed_mob.mod.rpt",
                        "GULD_timefrozen_tot.mod.rpt",
                        "GULD_centrescore.mod.rpt",
                        "GULD_emergelat.bin.mod.rpt",
                        "GULD_endpointlat.bin.mod.rpt"))

r1 <- rbind(GULD_dist.mod.rpt1$R, 
            GULD_avespeed_mob.mod.rpt1$R, 
            GULD_timefrozen_tot.mod.rpt1$R, 
            GULD_centrescore.mod.rpt1$R, 
            GULD_emergelat.bin.mod.rpt1$R[2, ], 
            GULD_endpointlat.bin.mod.rpt1$R[2, ])

cis <- rbind(GULD_dist.mod.rpt1$CI_emp,
             GULD_avespeed_mob.mod.rpt1$CI_emp,
             GULD_timefrozen_tot.mod.rpt1$CI_emp,
             GULD_centrescore.mod.rpt1$CI_emp,
             GULD_emergelat.bin.mod.rpt1$CI_emp$CI_link,
             GULD_endpointlat.bin.mod.rpt1$CI_em$CI_link)

p <- rbind(select(GULD_dist.mod.rpt1$P, -c(P_permut)), 
           select(GULD_avespeed_mob.mod.rpt1$P, -c(P_permut)), 
           select(GULD_timefrozen_tot.mod.rpt1$P, -c(P_permut)), 
           select(GULD_centrescore.mod.rpt1$P, -c(P_permut)), 
           select(GULD_emergelat.bin.mod.rpt1$P, -c(P_permut_org, P_permut_link)), 
           select(GULD_endpointlat.bin.mod.rpt1$P, -c(P_permut_org, P_permut_link)))
  
r2 <- rbind(GULD_dist.mod.rpt2$R, 
            GULD_avespeed_mob.mod.rpt2$R, 
            GULD_timefrozen_tot.mod.rpt2$R, 
            GULD_centrescore.mod.rpt2$R, 
            GULD_emergelat.bin.mod.rpt2$R[2, ], 
            GULD_endpointlat.bin.mod.rpt2$R[2, ])

cis2 <- rbind(GULD_dist.mod.rpt2$CI_emp,
             GULD_avespeed_mob.mod.rpt2$CI_emp,
             GULD_timefrozen_tot.mod.rpt2$CI_emp,
             GULD_centrescore.mod.rpt2$CI_emp,
             GULD_emergelat.bin.mod.rpt2$CI_emp$CI_link,
             GULD_endpointlat.bin.mod.rpt2$CI_em$CI_link)

p2 <- rbind(select(GULD_dist.mod.rpt2$P, -c(P_permut)), 
           select(GULD_avespeed_mob.mod.rpt2$P, -c(P_permut)), 
           select(GULD_timefrozen_tot.mod.rpt2$P, -c(P_permut)), 
           select(GULD_centrescore.mod.rpt2$P, -c(P_permut)), 
           select(GULD_emergelat.bin.mod.rpt2$P, -c(P_permut_org, P_permut_link)), 
           select(GULD_endpointlat.bin.mod.rpt2$P, -c(P_permut_org, P_permut_link)))

table1 <- cbind(name, r1, cis, p, r2, cis2, p2)
colnames(table1) <- c("var","r1","lci1","uci1","p1","r2","lci2","uci2","p2")

table1$text1 <- paste((round(table1$r1, digits = 2)), (round(table1$lci1, digits = 2)), sep = " [")
table1$text1 <- paste(table1$text1, (round(table1$uci1, digits = 2)), sep = ", ")

table1a <- subset(table1, p1 <= 0.001)
table1a$ptext <- "P < 0.001)"
table1b <- subset(table1, p1 >= 0.001)
table1b$ptext <- paste("P = ", (round(table1b$p1, digits = 4)), sep = "")
table1b$ptext <- paste(table1b$ptext, ")", sep = "")

table1 <-rbind(table1a, table1b)
table1$text1 <- paste(table1$text1, table1$ptext, sep = "] (")


table1$text2 <- paste((round(table1$r2, digits = 2)), (round(table1$lci2, digits = 2)), sep = " [")
table1$text2 <- paste(table1$text2, (round(table1$uci2, digits = 2)), sep = ", ")
table1$text2 <- paste(table1$text2, "P < 0.001)", sep = "] (")

write.csv(table1, "./outputs_visualisations/table1.csv")



#    - Table 2 ----
#GULD_dist.mod.red
fix2a <- as.data.frame(summary(GULD_dist.mod.red)$coefficients)
cin2a <- as.data.frame(confint(GULD_dist.mod.red))
cin2a_red <- cin2a[4:8, ]

table2a <- cbind(fix2a,cin2a_red)

colnames(table2a) <- c("Est","SE","df","t_z","P","CI25","CI95")
table2a$Est <- round(table2a$Est, digits = 2)
table2a$SE <- round(table2a$SE, digits = 2)
table2a$df <- round(table2a$df, digits = 2)
table2a$t_z <- round(table2a$t_z, digits = 2)
table2a$CI25 <- round(table2a$CI25, digits = 2)
table2a$CI95 <- round(table2a$CI95, digits = 2)
table2a.1 <- subset(table2a, P <= 0.001)
table2a.2 <- subset(table2a, P >= 0.001)
table2a.1$Ptext <- "< 0.001"
table2a.2$Ptext <- round(table2a.2$P, digits= 3)
table2a <- rbind(table2a.1, table2a.2)
table2a$text <- paste(table2a$Est, table2a$CI25, sep = " [")
table2a$text <- paste(table2a$text, table2a$CI95, sep = ", ")
table2a$text <- paste(table2a$text, "]", sep = "")
table2a <- table2a[, c(9, 2, 3, 4, 8)]
table2a$rownames <- rownames(table2a)
table2a <- table2a %>% arrange(factor(rownames, levels = c("(Intercept)", "Sexm", "TL.C", "CondManual.C", "TrialDay.C")))


#GULD_avespeed_mob.mod.red
fix2b <- as.data.frame(summary(GULD_avespeed_mob.mod.red)$coefficients)
cin2b <- as.data.frame(confint(GULD_avespeed_mob.mod.red))
cin2b_red <- cin2b[3:7, ]

table2b <- cbind(fix2b,cin2b_red)

colnames(table2b) <- c("Est","SE","df","t_z","P","CI25","CI95")
table2b$Est <- round(table2b$Est, digits = 2)
table2b$SE <- round(table2b$SE, digits = 2)
table2b$df <- round(table2b$df, digits = 2)
table2b$t_z <- round(table2b$t_z, digits = 2)
table2b$CI25 <- round(table2b$CI25, digits = 2)
table2b$CI95 <- round(table2b$CI95, digits = 2)
table2b.1 <- subset(table2b, P <= 0.001)
table2b.2 <- subset(table2b, P >= 0.001)
table2b.1$Ptext <- "< 0.001"
table2b.2$Ptext <- round(table2b.2$P, digits= 3)
table2b <- rbind(table2b.1, table2b.2)
table2b$text <- paste(table2b$Est, table2b$CI25, sep = " [")
table2b$text <- paste(table2b$text, table2b$CI95, sep = ", ")
table2b$text <- paste(table2b$text, "]", sep = "")
table2b <- table2b[, c(9, 2, 3, 4, 8)]
table2b$rownames <- rownames(table2b)
table2b <- table2b %>% arrange(factor(rownames, levels = c("(Intercept)", "Sexm", "TL.C", "CondManual.C", "TrialDay.C")))

#GULD_timefrozen_tot.mod.red
fix2c <- as.data.frame(summary(GULD_timefrozen_tot.mod.red)$coefficients)
cin2c <- as.data.frame(confint(GULD_timefrozen_tot.mod.red))
cin2c_red <- cin2c[5:9, ]

table2c <- cbind(fix2c,cin2c_red)

colnames(table2c) <- c("Est","SE","df","t_z","P","CI25","CI95")
table2c$Est <- round(table2c$Est, digits = 2)
table2c$SE <- round(table2c$SE, digits = 2)
table2c$df <- round(table2c$df, digits = 2)
table2c$t_z <- round(table2c$t_z, digits = 2)
table2c$CI25 <- round(table2c$CI25, digits = 2)
table2c$CI95 <- round(table2c$CI95, digits = 2)
table2c.1 <- subset(table2c, P <= 0.001)
table2c.2 <- subset(table2c, P >= 0.001)
table2c.1$Ptext <- "< 0.001"
table2c.2$Ptext <- round(table2c.2$P, digits= 3)
table2c <- rbind(table2c.1, table2c.2)
table2c$text <- paste(table2c$Est, table2c$CI25, sep = " [")
table2c$text <- paste(table2c$text, table2c$CI95, sep = ", ")
table2c$text <- paste(table2c$text, "]", sep = "")
table2c <- table2c[, c(9, 2, 3, 4, 8)]
table2c$rownames <- rownames(table2c)
table2c <- table2c %>% arrange(factor(rownames, levels = c("(Intercept)", "Sexm", "TL.C", "CondManual.C", "TrialDay.C")))

#GULD_centrescore.mod.red
fix2d <- as.data.frame(summary(GULD_centrescore.mod.red)$coefficients)
cin2d <- as.data.frame(confint(GULD_centrescore.mod.red))
cin2d_red <- cin2d[4:8, ]

table2d <- cbind(fix2d,cin2d_red)

colnames(table2d) <- c("Est","SE","df","t_z","P","CI25","CI95")
table2d$Est <- round(table2d$Est, digits = 2)
table2d$SE <- round(table2d$SE, digits = 2)
table2d$df <- round(table2d$df, digits = 2)
table2d$t_z <- round(table2d$t_z, digits = 2)
table2d$CI25 <- round(table2d$CI25, digits = 2)
table2d$CI95 <- round(table2d$CI95, digits = 2)
table2d.1 <- subset(table2d, P <= 0.001)
table2d.2 <- subset(table2d, P >= 0.001)
table2d.1$Ptext <- "< 0.001"
table2d.2$Ptext <- round(table2d.2$P, digits= 3)
table2d <- rbind(table2d.1, table2d.2)
table2d$text <- paste(table2d$Est, table2d$CI25, sep = " [")
table2d$text <- paste(table2d$text, table2d$CI95, sep = ", ")
table2d$text <- paste(table2d$text, "]", sep = "")
table2d <- table2d[, c(9, 2, 3, 4, 8)]
table2d$rownames <- rownames(table2d)
table2d <- table2d %>% arrange(factor(rownames, levels = c("(Intercept)", "Sexm", "TL.C", "CondManual.C", "TrialDay.C")))


#GULD_emergelat.bin.mod.red
fix2e <- as.data.frame(summary(GULD_emergelat.bin.mod.red)$coefficients)
cin2e <- as.data.frame(confint(GULD_emergelat.bin.mod.red, method="Wald"))
cin2e_red <- cin2e[2:6, ]
table2e <- cbind(fix2e,cin2e_red)
table2e$df <- "NA"

colnames(table2e) <- c("Est","SE","t_z","P","CI25","CI95","df")
table2e$Est <- round(table2e$Est, digits = 2)
table2e$SE <- round(table2e$SE, digits = 2)
table2e$t_z <- round(table2e$t_z, digits = 2)
table2e$CI25 <- round(table2e$CI25, digits = 2)
table2e$CI95 <- round(table2e$CI95, digits = 2)
table2e$Ptext <- round(table2e$P, digits= 3)
table2e$text <- paste(table2e$Est, table2e$CI25, sep = " [")
table2e$text <- paste(table2e$text, table2e$CI95, sep = ", ")
table2e$text <- paste(table2e$text, "]", sep = "")
table2e <- table2e[, c(9, 2, 7, 3, 8)]
table2e$rownames <- rownames(table2e)
table2e <- table2e %>% arrange(factor(rownames, levels = c("(Intercept)", "Sexm", "TL.C", "CondManual.C", "TrialDay.C")))


#GULD_endpointlat.bin.mod.red
fix2f <- as.data.frame(summary(GULD_endpointlat.bin.mod.red)$coefficients)
cin2f <- as.data.frame(confint(GULD_endpointlat.bin.mod.red, method="Wald"))
cin2f_red <- cin2f[2:6, ]
table2f <- cbind(fix2f,cin2f_red)
table2f$df <- "NA"

colnames(table2f) <- c("Est","SE","t_z","P","CI25","CI95","df")
table2f$Est <- round(table2f$Est, digits = 2)
table2f$SE <- round(table2f$SE, digits = 2)
table2f$t_z <- round(table2f$t_z, digits = 2)
table2f$CI25 <- round(table2f$CI25, digits = 2)
table2f$CI95 <- round(table2f$CI95, digits = 2)
table2f$Ptext <- round(table2f$P, digits= 3)
table2f$text <- paste(table2f$Est, table2f$CI25, sep = " [")
table2f$text <- paste(table2f$text, table2f$CI95, sep = ", ")
table2f$text <- paste(table2f$text, "]", sep = "")
table2f <- table2f[, c(9, 2, 7, 3, 8)]
table2f$rownames <- rownames(table2f)
table2f <- table2f %>% arrange(factor(rownames, levels = c("(Intercept)", "Sexm", "TL.C", "CondManual.C", "TrialDay.C")))

table2 <- rbind(table2a,
                table2b,
                table2c,
                table2d,
                table2e,
                table2f)

write.csv(table2, "./outputs_visualisations/table2.csv")


# 2.4. Correlations between behavioural variables ----
#Formatting database
GULDexpl.processed.working <- select(GULDexpl.processed, -c("PITID", "Date", "TL", "ConditionFactor", "CondManual", "Sex", "TrialType", "TankID",
                                                            "TL.C", "CondManual.C", "TrialDay.C"))
GULDexpl.processed.working <- rename(GULDexpl.processed.working, EXPLTrialRound = TrialRound)
GULDexpl.processed.working <- rename(GULDexpl.processed.working, EXPLArenaID = ArenaID)
GULDexpl.processed.working <- rename(GULDexpl.processed.working, EXPLUniqueID = UniqueID)

GULDact.processed.working  <- select(GULDact.processed, -c("Date", "TimeLoaded", "TrialType"))
GULDact.processed.working  <- rename(GULDact.processed.working, ACTTrialRound = TrialRound)
GULDact.processed.working  <- rename(GULDact.processed.working, ACTArenaID = ArenaID)
GULDact.processed.working  <- rename(GULDact.processed.working, ACTUniqueID = UniqueID)

GULDact.processed.working$mergeID <- paste(GULDact.processed.working$FishID, GULDact.processed.working$TrialDay, sep = "_") 
GULDexpl.processed.working$mergeID <- paste(GULDexpl.processed.working$FishID, GULDexpl.processed.working$TrialDay, sep = "_") 

GULDexpl.processed.working <- select(GULDexpl.processed.working, -c("TrialDay", "FishID"))
GULDbehav_combined <- merge(GULDact.processed.working, GULDexpl.processed.working, by = "mergeID", all.x = TRUE)

#removing NAs and unused variables
GULDbehav_combined.red <- subset(GULDbehav_combined, emergelat != "NA")
GULDbehav_combined.red <- select(GULDbehav_combined.red, c("FishID","dist","avespeed_mob","timefrozen_tot","centrescore2","emergelat","endpointlat"))
pairs(GULDbehav_combined.red[,-1],
      lower.panel = NULL)

#Running redundancy analysis to visualise relationships between variables
#https://rpubs.com/brouwern/veganpca

set.seed(253)
GULDbehav_combined.red$dist <- scale(GULDbehav_combined.red$dist)
GULDbehav_combined.red$avespeed_mob <- scale(GULDbehav_combined.red$avespeed_mob)
GULDbehav_combined.red$timefrozen_tot <- scale(GULDbehav_combined.red$timefrozen_tot)
GULDbehav_combined.red$centrescore2 <- scale(GULDbehav_combined.red$centrescore2)
GULDbehav_combined.red$emergelat <- scale(GULDbehav_combined.red$emergelat)
GULDbehav_combined.red$endpointlat <- scale(GULDbehav_combined.red$endpointlat)

my.rda <- rda(GULDbehav_combined.red[,-1])
summary(my.rda)
biplot(my.rda,
       display = c("sites", 
                   "species"),
       type = c("text",
                "points"))

#Correlation tests
corr1_1 <- cor.test(GULDbehav_combined$dist, GULDbehav_combined$avespeed_mob, method = "spearman")
corr1_2 <- cor.test(GULDbehav_combined$dist, GULDbehav_combined$timefrozen_tot, method = "spearman")
corr1_3 <- cor.test(GULDbehav_combined$dist, GULDbehav_combined$centrescore2, method = "spearman")
corr1_4 <- cor.test(GULDbehav_combined$dist, GULDbehav_combined$emergelat, method = "spearman")
corr1_5 <- cor.test(GULDbehav_combined$dist, GULDbehav_combined$endpointlat, method = "spearman")

corr2_2 <- cor.test(GULDbehav_combined$avespeed_mob, GULDbehav_combined$timefrozen_tot, method = "spearman")
corr2_3 <- cor.test(GULDbehav_combined$avespeed_mob, GULDbehav_combined$centrescore2, method = "spearman")
corr2_4 <- cor.test(GULDbehav_combined$avespeed_mob, GULDbehav_combined$emergelat, method = "spearman")
corr2_5 <- cor.test(GULDbehav_combined$avespeed_mob, GULDbehav_combined$endpointlat, method = "spearman")

corr3_3 <- cor.test(GULDbehav_combined$timefrozen_tot, GULDbehav_combined$centrescore2, method = "spearman")
corr3_4 <- cor.test(GULDbehav_combined$timefrozen_tot, GULDbehav_combined$emergelat, method = "spearman")
corr3_5 <- cor.test(GULDbehav_combined$timefrozen_tot, GULDbehav_combined$endpointlat, method = "spearman")

corr4_4 <- cor.test(GULDbehav_combined$centrescore2, GULDbehav_combined$emergelat, method = "spearman")
corr4_5 <- cor.test(GULDbehav_combined$centrescore2, GULDbehav_combined$endpointlat, method = "spearman")

corr5_5 <- cor.test(GULDbehav_combined$emergelat, GULDbehav_combined$endpointlat, method = "spearman")

corre <- as.data.frame(c(corr1_1$estimate,corr1_2$estimate,corr2_2$estimate,corr1_3$estimate,corr2_3$estimate,
                         corr3_3$estimate,corr1_4$estimate,corr2_4$estimate,corr3_4$estimate,corr4_4$estimate,
                         corr1_5$estimate,corr2_5$estimate,corr3_5$estimate,corr4_5$estimate,corr5_5$estimate))
colnames(corre) <- "a" 
corre$a <- round(corre$a, digits = 3) 

corrp <- as.data.frame(c(corr1_1$p.value,corr1_2$p.value,corr2_2$p.value,corr1_3$p.value,corr2_3$p.value,
                          corr3_3$p.value,corr1_4$p.value,corr2_4$p.value,corr3_4$p.value,corr4_4$p.value,
                          corr1_5$p.value,corr2_5$p.value,corr3_5$p.value,corr4_5$p.value,corr5_5$p.value))
colnames(corrp) <- "a"
corrp$rownames <- rownames(corrp)

corrp1 <- subset(corrp, a < 0.001)
corrp2 <- subset(corrp, a >= 0.001)
corrp1$a <- "(P < 0.001)"
corrp2$a <- round(corrp2$a, digits = 3)
corrp2$a <- paste("(P = ", corrp2$a, sep = "")
corrp2$a <- paste(corrp2$a, ")", sep = "")
corrp <- rbind(corrp1, corrp2)
corrp$rownames <- as.numeric(corrp$rownames) 
corrp <- corrp[order(corrp$rownames,decreasing=FALSE),]

corre$a <- paste(corre$a, corrp$a, sep = " ")

corr1 <- as.data.frame(c(corre[1,],"","","",""))
corr2 <- as.data.frame(c(corre[2:3,],"","",""))
corr3 <- as.data.frame(c(corre[4:6,],"",""))
corr4 <- as.data.frame(c(corre[7:10,],""))
corr5 <- as.data.frame(c(corre[11:15,]))

table3 <- (cbind(corr1,corr2,corr3,corr4,corr5))
colnames(table3) <- c(1,2,3,4,5)
write.csv(table3, "./outputs_visualisations/table3.csv")


# 2.5. Building data frame for SIA correlation analysis ----
# - Behavioural predictor variables 
#   1- Initial behavioural response (trial 1 response)
#   2- Average behavioural response (trial 1-3 average, limited to individuals completing all 3 trails, due to treatment effects)

#Extracting first trial
GULDbehav_phenotypes1 <- subset(GULDbehav_combined, TrialDay == 1)
GULDbehav_phenotypes1 <- GULDbehav_phenotypes1[,c(-1,-3,-8,-9,-10,-11,-13,-15,-16,-18,-25,-26,-27,-28,-29,-30,-31,-32,-37,-38)]
labels(GULDbehav_phenotypes1)

#Extracting list of fish with all three trial completed
GULDbehav_phenotypes2A <- subset(GULDbehav_combined, TrialDay == 3)
GULDbehav_phenotypes2A <- GULDbehav_phenotypes2A[,2]
GULDbehav_phenotypes2B <- subset(GULDbehav_combined, FishID %in% GULDbehav_phenotypes2A)
#34 fish and 102 trials.
GULDbehav_phenotypes2C <- setDT(GULDbehav_phenotypes2B)[ , list(dist_M = mean(dist),
                                                                timefrozen_tot_M = mean(timefrozen_tot),
                                                                log.timefrozen_tot_M = mean(log(timefrozen_tot)),
                                                                avespeed_mob_M = mean(avespeed_mob),
                                                                centrescore_M = mean(centrescore),
                                                                centrescore2_M = mean(centrescore2),
                                                                centretime50_M = mean(centretime50),
                                                                centretime75_M = mean(centretime75),
                                                                centretime100_M = mean(centretime100)), 
                                              by = .(FishID)]

GULDbehav_phenotypes2D <- subset(GULDbehav_combined, TrialDay == 3)
GULDbehav_phenotypes2D <- subset(GULDbehav_phenotypes2D, emergelat != 'NA')
GULDbehav_phenotypes2D <- GULDbehav_phenotypes2D[,2]
GULDbehav_phenotypes2E <- subset(GULDbehav_combined, FishID %in% GULDbehav_phenotypes2D)
#32 fish, 96 trials
GULDbehav_phenotypes2F <- setDT(GULDbehav_phenotypes2E)[ , list(emergelat_M = mean(emergelat),
                                                                emergelat.bin_M = mean(emergelat.bin),
                                                                endpointlat_M = mean(endpointlat),
                                                                endpointlat.bin_M = mean(endpointlat.bin)),
                                                         by = .(FishID)]
  
GULDbehav_phenotypes <- merge(GULDbehav_phenotypes1, GULDbehav_phenotypes2C, by = "FishID", all.x = TRUE)
GULDbehav_phenotypes <- merge(GULDbehav_phenotypes, GULDbehav_phenotypes2F, by = "FishID", all.x = TRUE)


write.csv(GULDbehav_phenotypes, '~/trophic-personalities_2020/dat_behaviour/GULDbehav_phenotypes.csv')


#### ... #### 


