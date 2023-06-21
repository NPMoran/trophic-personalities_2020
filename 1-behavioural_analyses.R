# Title: Behavioural and trophic variation within a well-established invasive round goby population 
# Author: Nicholas Moran
# Date: March 2023



#### 2. Behavioural analysis ####
Sys.setenv(LANG = "en")
#Loading required packages- 
library(dplyr); library(ggplot2); library(ggpubr)
library(lme4); library(lmerTest); library(car); library(rptR); library(performance)
#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, size = 1))


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


#    a. Total distance moved accross the trial (mm, 'dist') ----
# - Assessing distributions
ggplot(GULDact.processed) + aes(x = dist) + geom_histogram(color="black", fill="lightblue", binwidth = 4800) + simpletheme 
ggqqplot(GULDact.processed$dist)

# - Full model
GULD_dist.mod <- lmer(dist ~ 
                             Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
summary(GULD_dist.mod) #TrialRound, TankID extremely low to zero variance explained
plot(GULD_dist.mod) #No issues

# - Reduced model
GULD_dist.mod.red <- lmer(dist ~ Sex + TL.C + CondManual.C + TrialDay.C + (1|ArenaID) + (1|FishID), data=GULDact.processed)
summary(GULD_dist.mod.red)  #Sex and replicate effect, marginal condition effect
r2_nakagawa(GULD_dist.mod.red) 

# - Repeatabilities
GULD_dist.mod.rpt1 <- rpt(dist  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                          nboot = 100, npermut = 0)
GULD_dist.mod.rpt1
GULD_dist.mod.rpt2 <- rpt(dist ~ 
                            (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                          nboot = 100, npermut = 0)
GULD_dist.mod.rpt2

save(GULD_dist.mod, file = "./outputs_visualisations/GULD_dist.mod.RData")
save(GULD_dist.mod.red, file = "./outputs_visualisations/GULD_dist.mod.red.RData")
save(GULD_dist.mod.rpt1, file = "./outputs_visualisations/GULD_dist.mod.rpt1.RData")
save(GULD_dist.mod.rpt2, file = "./outputs_visualisations/GULD_dist.mod.rpt2.RData")



#    b. Average speed (mm/m, 'avespeed_mob') ----
ggplot(GULDact.processed) + aes(x = avespeed_mob) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(GULDact.processed$avespeed_mob) #minor issue with 4-5 outliers at the very low end

GULD_avespeed_mob.mod <- lmer(avespeed_mob ~ 
                                Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
summary(GULD_avespeed_mob.mod)  #TrialRound, TankID, ArenaID extremely low to zero variance explained
plot(GULD_avespeed_mob.mod)  #Small number of low end speed leading to some asymmetry

GULD_avespeed_mob.mod.red <- lmer(avespeed_mob ~ 
                                Sex + TL.C + CondManual.C + TrialDay.C + (1|FishID),  data=GULDact.processed)
summary(GULD_avespeed_mob.mod.red)  #Sex and replicate effect, 
r2_nakagawa(GULD_avespeed_mob.mod.red) 

GULD_avespeed_mob.mod.rpt1 <- rpt(avespeed_mob ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_avespeed_mob.mod.rpt1
GULD_avespeed_mob.mod.rpt2 <- rpt(avespeed_mob ~ 
                                    (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_avespeed_mob.mod.rpt2

save(GULD_avespeed_mob.mod, file = "./outputs_visualisations/GULD_avespeed_mob.mod.RData")
save(GULD_avespeed_mob.mod.red, file = "./outputs_visualisations/GULD_avespeed_mob.mod.red.RData")
save(GULD_avespeed_mob.mod.rpt1, file = "./outputs_visualisations/GULD_avespeed_mob.mod.rpt1.RData")
save(GULD_avespeed_mob.mod.rpt2, file = "./outputs_visualisations/GULD_avespeed_mob.mod.rpt2.RData")


#    c. Time frozen (s, 'timefrozen_tot') ----
ggplot(GULDact.processed) + aes(x = timefrozen_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 85) + simpletheme 
ggqqplot(GULDact.processed$timefrozen_tot)
ggplot(GULDact.processed) + aes(x = log(timefrozen_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.3) + simpletheme 
ggqqplot(log(GULDact.processed$timefrozen_tot)) #still some skew bit much closer to normal

GULD_timefrozen_tot.mod <- lmer(log(timefrozen_tot) ~ 
                                       Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
summary(GULD_timefrozen_tot.mod)  #TankID resolve no variance
plot(GULD_timefrozen_tot.mod)  #No issues

GULD_timefrozen_tot.mod.red <- lmer(log(timefrozen_tot) ~ 
                                           Sex + TL.C + CondManual.C + TrialDay.C + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
summary(GULD_timefrozen_tot.mod.red)  #Sex, condition, and replicate effects
r2_nakagawa(GULD_timefrozen_tot.mod.red) 

GULD_timefrozen_tot.mod.rpt1 <- rpt(log(timefrozen_tot) ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_timefrozen_tot.mod.rpt1
GULD_timefrozen_tot.mod.rpt2 <- rpt(log(timefrozen_tot) ~ 
                                    (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                  nboot = 100, npermut = 0)
GULD_timefrozen_tot.mod.rpt2

save(GULD_timefrozen_tot.mod, file = "./outputs_visualisations/GULD_timefrozen_tot.mod.RData")
save(GULD_timefrozen_tot.mod.red, file = "./outputs_visualisations/GULD_timefrozen_tot.mod.red.RData")
save(GULD_timefrozen_tot.mod.rpt1, file = "./outputs_visualisations/GULD_timefrozen_tot.mod.rpt1.RData")
save(GULD_timefrozen_tot.mod.rpt2, file = "./outputs_visualisations/GULD_timefrozen_tot.mod.rpt2.RData")



#    d. Edge use, (mm, 'centrescore') ---- 
ggplot(GULDact.processed) + aes(x = centrescore2) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(GULDact.processed$centrescore2) #potential minimal positive skew

GULD_centrescore.mod <- lmer(centrescore2 ~ 
                                Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
summary(GULD_centrescore.mod)   #TankID and TrialRound resolve no variance
plot(GULD_centrescore.mod)  #No issues

GULD_centrescore.mod.red <- lmer(centrescore ~ 
                                Sex + TL.C + CondManual.C + TrialDay.C + (1|ArenaID) + (1|FishID),  data=GULDact.processed)
summary(GULD_centrescore.mod.red)  #length effect
r2_nakagawa(GULD_centrescore.mod.red) 

GULD_centrescore.mod.rpt1 <- rpt(centrescore2  ~ (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_centrescore.mod.rpt1
GULD_centrescore.mod.rpt2 <- rpt(centrescore2 ~ 
                                   (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDact.processed, datatype = "Gaussian", 
                                 nboot = 100, npermut = 0)
GULD_centrescore.mod.rpt2

save(GULD_centrescore.mod, file = "./outputs_visualisations/GULD_centrescore.mod.RData")
save(GULD_centrescore.mod.red, file = "./outputs_visualisations/GULD_centrescore.mod.red.RData")
save(GULD_centrescore.mod.rpt1, file = "./outputs_visualisations/GULD_centrescore.mod.rpt1.RData")
save(GULD_centrescore.mod.rpt2, file = "./outputs_visualisations/GULD_centrescore.mod.rpt2.RData")



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

#    e. Latency to emerge from the refuge (binary, 'emergelat.bin') ----
GULD_emergelat.bin.mod <- glmer(emergelat.bin ~ 
                                  Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), family = binomial, data=GULDexpl.processed)
summary(GULD_emergelat.bin.mod) #TrialRound, ArenaID and TrialDay resolve extremely low variance
plot(GULD_emergelat.bin.mod)              

GULD_emergelat.bin.mod.red <- glmer(emergelat.bin ~ 
                                  Sex + TL.C + CondManual.C + TrialDay.C + (1|FishID), family = binomial, data=GULDexpl.processed)
summary(GULD_emergelat.bin.mod.red) #no effects
r2_nakagawa(GULD_emergelat.bin.mod.red)              

GULD_emergelat.bin.mod.rpt1 <- rpt(emergelat.bin ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                               nboot = 100, npermut = 0)
GULD_emergelat.bin.mod.rpt1
GULD_emergelat.bin.mod.rpt2 <- rpt(emergelat.bin ~ 
                                 (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                               nboot = 100, npermut = 0)
GULD_emergelat.bin.mod.rpt2

save(GULD_emergelat.bin.mod, file = "./outputs_visualisations/GULD_emergelat.bin.mod.RData")
save(GULD_emergelat.bin.mod.red, file = "./outputs_visualisations/GULD_emergelat.bin.mod.red.RData")
save(GULD_emergelat.bin.mod.rpt1, file = "./outputs_visualisations/GULD_emergelat.bin.mod.rpt1.RData")
save(GULD_emergelat.bin.mod.rpt2, file = "./outputs_visualisations/GULD_emergelat.bin.mod.rpt2.RData")


#    f. Latency to explore to the endpoint of the arena (binary, 'emergelat.bin') ----
GULD_endpointlat.bin.mod <- glmer(endpointlat.bin ~ 
                                  Sex + TL.C + CondManual.C + TrialDay.C + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), family = binomial, data=GULDexpl.processed)
summary(GULD_endpointlat.bin.mod) #TrialRound, ArenaID and TrialDay resolve extremely low variance
plot(GULD_endpointlat.bin.mod)              

GULD_endpointlat.bin.mod.red <- glmer(endpointlat.bin ~ 
                                      Sex + TL.C + CondManual.C + TrialDay.C + (1|FishID), family = binomial, data=GULDexpl.processed)
summary(GULD_endpointlat.bin.mod.red) #no effects
r2_nakagawa(GULD_endpointlat.bin.mod.red)              

GULD_endpointlat.bin.mod.rpt1 <- rpt(endpointlat.bin ~ (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                               nboot = 100, npermut = 0)
GULD_endpointlat.bin.mod.rpt1
GULD_endpointlat.bin.mod.rpt2 <- rpt(endpointlat.bin ~ 
                                 (1|TrialDay) + (1|TankID) + (1|TrialRound) + (1|ArenaID) + (1|FishID), grname = "FishID", data = GULDexpl.processed, datatype = "Binary", 
                               nboot = 100, npermut = 0)
GULD_endpointlat.bin.mod.rpt2


save(GULD_endpointlat.bin.mod, file = "./outputs_visualisations/GULD_endpointlat.bin.mod.RData")
save(GULD_endpointlat.bin.mod.red, file = "./outputs_visualisations/GULD_endpointlat.bin.mod.red.RData")
save(GULD_endpointlat.bin.mod.rpt1, file = "./outputs_visualisations/GULD_endpointlat.bin.mod.rpt1.RData")
save(GULD_endpointlat.bin.mod.rpt2, file = "./outputs_visualisations/GULD_endpointlat.bin.mod.rpt2.RData")




# 2.3. Extracting data for tables ----
#    a. Table 1 ----

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



#    a. Table 2 ----
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
labels(GULDact.processed.working)
labels(GULDexpl.processed.working)

GULDexpl.processed.working <- select(GULDexpl.processed, -c("PITID", "Date","TL", "ConditionFactor", "CondManual", "Sex", "TrialType", "TankID",
                                                            "TL.C", "CondManual.C", "TrialDay.C"))
GULDexpl.processed.working <- rename(GULDexpl.processed.working, EXPLTrialRound = TrialRound)
GULDexpl.processed.working <- rename(GULDexpl.processed.working, EXPLArenaID = ArenaID)
GULDexpl.processed.working <- rename(GULDexpl.processed.working, EXPLUniqueID = UniqueID)

GULDact.processed.working  <- select(GULDexpl.processed, -c("TL",  "Date", "TimeLoaded", "TrialType"))
GULDact.processed.working  <- rename(GULDact.processed, ACTTrialRound = TrialRound)
GULDact.processed.working  <- rename(GULDact.processed, ACTArenaID = ArenaID)
GULDact.processed.working  <- rename(GULDact.processed, ACTUniqueID = UniqueID)

GULDact.processed.working$mergeID <- paste(GULDact.processed.working$FishID, GULDact.processed.working$TrialDay, sep = "_") 
GULDexpl.processed.working$mergeID <- paste(GULDexpl.processed.working$FishID, GULDexpl.processed.working$TrialDay, sep = "_") 

GULDbehav_combined <- merge(GULDact.processed.working, GULDexpl.processed.working, by = "mergeID", all.x = TRUE)
cor.test(GULDbehav_combined$dist, GULDbehav_combined$emergelat, method = "spearman")
cor.test(GULDbehav_combined$dist, GULDbehav_combined$endpointlat, method = "spearman")

cor.test(GULDbehav_combined$centrescore2, GULDbehav_combined$emergelat, method = "spearman")
cor.test(GULDbehav_combined$centrescore2, GULDbehav_combined$endpointlat, method = "spearman")


## G.2.5. Building data frame for SIA correlation analysis ----
# - Behavioural predictor variations for SIA analysis
#   1- Initial behavioural response (trial 1 response)
#   2- Average behavioural response (trial 1-3 average, limited to individuals completing all 3 trails, due to treatment effects)
#   3- Model coefficients estimates for each individual from reduced models (conditional mode for each individuals, reflects an adjusted average behaviour) 

#Extracting the first trial and average behaviour accross trials
# - First Trial used as a single phenotypic measure most closely reflect their behaviour in the field
# - As there is a TrialDay effect in most variables, only individuals with scores for all three trials are used for averages. 
#Activity
behavACTT1 <- NULL
behavACTT1$FishID <- subset(GULDact.processed, TrialDay == 'trial 1')$FishID
behavACTT1$avespeed_tot.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$avespeed_tot
behavACTT1$avespeed_mob.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$avespeed_mob
behavACTT1$aveacceler.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$aveacceler
behavACTT1$propmoving.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$propmoving
behavACTT1$dist.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$dist
behavACTT1$frozenevents.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$frozenevents
behavACTT1$timefrozen_tot.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$timefrozen_tot
behavACTT1$centrescore.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$centrescore
behavACTT1$centretime50.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$centretime50
behavACTT1$centretime75.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$centretime75
behavACTT1$centretime100.ACTT1 <- subset(GULDact.processed, TrialDay == 'trial 1')$centretime100
behavACTT1 <- as.data.frame(behavACTT1)

behavACTT2 <- NULL
behavACTT2$FishID <- subset(GULDact.processed, TrialDay == 'trial 2')$FishID
behavACTT2$avespeed_tot.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$avespeed_tot
behavACTT2$avespeed_mob.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$avespeed_mob
behavACTT2$aveacceler.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$aveacceler
behavACTT2$propmoving.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$propmoving
behavACTT2$dist.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$dist
behavACTT2$frozenevents.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$frozenevents
behavACTT2$timefrozen_tot.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$timefrozen_tot
behavACTT2$centrescore.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$centrescore
behavACTT2$centretime50.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$centretime50
behavACTT2$centretime75.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$centretime75
behavACTT2$centretime100.ACTT2 <- subset(GULDact.processed, TrialDay == 'trial 2')$centretime100
behavACTT2 <- as.data.frame(behavACTT2)

behavACTT3 <- NULL
behavACTT3$FishID <- subset(GULDact.processed, TrialDay == 'trial 3')$FishID
behavACTT3$avespeed_tot.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$avespeed_tot
behavACTT3$avespeed_mob.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$avespeed_mob
behavACTT3$aveacceler.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$aveacceler
behavACTT3$propmoving.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$propmoving
behavACTT3$dist.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$dist
behavACTT3$frozenevents.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$frozenevents
behavACTT3$timefrozen_tot.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$timefrozen_tot
behavACTT3$centrescore.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$centrescore
behavACTT3$centretime50.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$centretime50
behavACTT3$centretime75.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$centretime75
behavACTT3$centretime100.ACTT3 <- subset(GULDact.processed, TrialDay == 'trial 3')$centretime100
behavACTT3 <- as.data.frame(behavACTT3)
nrow(behavACTT3)

behavMEANWorking <- merge(behavACTT1, behavACTT2, by = 'FishID', all.x = FALSE)
behavMEANWorking <- merge(behavMEANWorking, behavACTT3, by = 'FishID', all.x = FALSE)

behavMEAN <- NULL
behavMEAN$FishID <- behavMEANWorking$FishID
behavMEAN$avespeed_tot.MEAN <- (behavMEANWorking$avespeed_tot.ACTT1+behavMEANWorking$avespeed_tot.ACTT2+behavMEANWorking$avespeed_tot.ACTT3)/3
behavMEAN$avespeed_mob.MEAN <- (behavMEANWorking$avespeed_mob.ACTT1+behavMEANWorking$avespeed_mob.ACTT2+behavMEANWorking$avespeed_mob.ACTT3)/3
behavMEAN$aveacceler.MEAN <- (behavMEANWorking$aveacceler.ACTT1+behavMEANWorking$aveacceler.ACTT2+behavMEANWorking$aveacceler.ACTT3)/3
behavMEAN$propmoving.MEAN <- (behavMEANWorking$propmoving.ACTT1+behavMEANWorking$propmoving.ACTT2+behavMEANWorking$propmoving.ACTT3)/3
behavMEAN$dist.MEAN <- (behavMEANWorking$dist.ACTT1+behavMEANWorking$dist.ACTT2+behavMEANWorking$dist.ACTT3)/3
behavMEAN$frozenevents.MEAN <- (behavMEANWorking$frozenevents.ACTT1+behavMEANWorking$frozenevents.ACTT2+behavMEANWorking$frozenevents.ACTT3)/3
behavMEAN$timefrozen_tot.MEAN <- (behavMEANWorking$timefrozen_tot.ACTT1+behavMEANWorking$timefrozen_tot.ACTT2+behavMEANWorking$timefrozen_tot.ACTT3)/3
behavMEAN$centrescore.MEAN <- (behavMEANWorking$centrescore.ACTT1+behavMEANWorking$centrescore.ACTT2+behavMEANWorking$centrescore.ACTT3)/3
behavMEAN$centretime50.MEAN <- (behavMEANWorking$centretime50.ACTT1+behavMEANWorking$centretime50.ACTT2+behavMEANWorking$centretime50.ACTT3)/3
behavMEAN$centretime75.MEAN <- (behavMEANWorking$centretime75.ACTT1+behavMEANWorking$centretime75.ACTT2+behavMEANWorking$centretime75.ACTT3)/3
behavMEAN$centretime100.MEAN <- (behavMEANWorking$centretime100.ACTT1+behavMEANWorking$centretime100.ACTT2+behavMEANWorking$centretime100.ACTT3)/3
behavMEAN <- as.data.frame(behavMEAN)

#Exploratory data
behavEXPLT1 <- NULL
behavEXPLT1$FishID <- subset(GULDexpl.processed, TrialDay == 'trial 1')$FishID
behavEXPLT1$emergelat.bin.EXPLT1 <- subset(GULDexpl.processed, TrialDay == 'trial 1')$emergelat.bin
behavEXPLT1$endpointlat.bin.EXPLT1 <- subset(GULDexpl.processed, TrialDay == 'trial 1')$endpointlat.bin
behavEXPLT1$endpointspeed.EXPLT1 <- subset(GULDexpl.processed, TrialDay == 'trial 1')$endpointspeed
behavEXPLT1$refugereturnlat.EXPLT1 <- subset(GULDexpl.processed, TrialDay == 'trial 1')$refugereturnlat
behavEXPLT1 <- as.data.frame(behavEXPLT1)

behavEXPLT2 <- NULL
behavEXPLT2$FishID <- subset(GULDexpl.processed, TrialDay == 'trial 2')$FishID
behavEXPLT2$emergelat.bin.EXPLT2 <- subset(GULDexpl.processed, TrialDay == 'trial 2')$emergelat.bin
behavEXPLT2$endpointlat.bin.EXPLT2 <- subset(GULDexpl.processed, TrialDay == 'trial 2')$endpointlat.bin
behavEXPLT2$endpointspeed.EXPLT2 <- subset(GULDexpl.processed, TrialDay == 'trial 2')$endpointspeed
behavEXPLT2$refugereturnlat.EXPLT2 <- subset(GULDexpl.processed, TrialDay == 'trial 2')$refugereturnlat
behavEXPLT2 <- as.data.frame(behavEXPLT2)

behavEXPLT3 <- NULL
behavEXPLT3$FishID <- subset(GULDexpl.processed, TrialDay == 'trial 3')$FishID
behavEXPLT3$emergelat.bin.EXPLT3 <- subset(GULDexpl.processed, TrialDay == 'trial 3')$emergelat.bin
behavEXPLT3$endpointlat.bin.EXPLT3 <- subset(GULDexpl.processed, TrialDay == 'trial 3')$endpointlat.bin
behavEXPLT3$endpointspeed.EXPLT3 <- subset(GULDexpl.processed, TrialDay == 'trial 3')$endpointspeed
behavEXPLT3$refugereturnlat.EXPLT3 <- subset(GULDexpl.processed, TrialDay == 'trial 3')$refugereturnlat
behavEXPLT3 <- as.data.frame(behavEXPLT3)

behavMEANWorking <- merge(behavEXPLT1, behavEXPLT2, by = 'FishID', all.x = FALSE)
behavMEANWorking <- merge(behavMEANWorking, behavEXPLT3, by = 'FishID', all.x = FALSE)

behavMEAN2 <- NULL
behavMEAN2$FishID <- behavMEANWorking$FishID
behavMEAN2$emergelat.bin.MEAN <- (behavMEANWorking$emergelat.bin.EXPLT1+behavMEANWorking$emergelat.bin.EXPLT2+behavMEANWorking$emergelat.bin.EXPLT3)/3
behavMEAN2$endpointlat.bin.MEAN <- (behavMEANWorking$endpointlat.bin.EXPLT1+behavMEANWorking$endpointlat.bin.EXPLT2+behavMEANWorking$endpointlat.bin.EXPLT3)/3
behavMEAN2$endpointspeed.MEAN <- (behavMEANWorking$endpointspeed.EXPLT1+behavMEANWorking$endpointspeed.EXPLT2+behavMEANWorking$endpointspeed.EXPLT3)/3
behavMEAN2$refugereturnlat.MEAN <- (behavMEANWorking$refugereturnlat.EXPLT1+behavMEANWorking$refugereturnlat.EXPLT2+behavMEANWorking$refugereturnlat.EXPLT3)/3
behavMEAN2 <- as.data.frame(behavMEAN2)

#Combining Expl and Act data
behavMEAN <- merge(behavMEAN, behavMEAN2, by = 'FishID', all.x = TRUE)
behavTRIAL1 <- merge(behavACTT1, behavEXPLT1, by = 'FishID', all.x = TRUE)


#Extracting the coefficient for each individual for each behavioural variable. 
# - equivalent to a conditional mode for each level of the random effect (i.e. each individual fish)
#   i.e. the predicted most likely value for each individual FishID, accounting for other Random effects
# - equivalent to their BLUP + the overall model intercept

# Activity variables
behvar3a <- coef(GULD_avespeed_tot.mod.red)$FishID
behvar3b <- coef(GULD_avespeed_mob.mod.red)$FishID
behvar3c <- coef(GULD_aveacceler.mod.red)$FishID
behvar3d <- coef(GULD_propmoving.invlog.mod.red)$FishID
behvar3e <- coef(GULD_dist.mod.red)$FishID
behvar3f <- coef(GULD_frozenevents.sqrt.mod.red)$FishID
behvar3g <- coef(GULD_timefrozen_tot.sqrt.mod.red)$FishID
behvar3h <- coef(GULD_centrescore.mod.red)$FishID
behvar3i <- coef(GULD_centretime50.sqrt.mod.red)$FishID
behvar3j <- coef(GULD_centretime75.sqrt.mod.red)$FishID
behvar3k <- coef(GULD_centretime100.sqrt.mod.red)$FishID

colnames(behvar3a)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3b)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3c)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3d)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3e)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3f)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3g)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3h)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3i)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3j)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3k)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")

behavCOEFs <- NULL
behavCOEFs$FishID <- rownames(behvar3a)
behavCOEFs$avespeed_tot.COEF <- behvar3a$Intercept
behavCOEFs$avespeed_mob.COEF <- behvar3b$Intercept
behavCOEFs$aveacceler.COEF <- behvar3c$Intercept
behavCOEFs$propmoving.invlog.COEF <- behvar3d$Intercept
behavCOEFs$dist.COEF <- behvar3e$Intercept
behavCOEFs$frozenevents.sqrt.COEF <- behvar3f$Intercept
behavCOEFs$timefrozen_tot.sqrt.COEF <- behvar3g$Intercept
behavCOEFs$centrescore.COEF <- behvar3h$Intercept
behavCOEFs$centretime50.sqrt.COEF <- behvar3i$Intercept
behavCOEFs$centretime75.sqrt.COEF <- behvar3j$Intercept
behavCOEFs$centretime100.sqrt.COEF <- behvar3k$Intercept
behavCOEFs <- as.data.frame(behavCOEFs)

#Exploration variables:
behvar3l <- coef(GULD_emergelat.bin.mod.red)$FishID
behvar3m <- coef(GULD_endpointlat.bin.mod.red)$FishID
behvar3n <- coef(GULD_endpointspeed.ln.mod.red)$FishID
behvar3o <- coef(GULD_refugereturnlat.ln.mod.red)$FishID

colnames(behvar3l)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3m)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3n)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")
colnames(behvar3o)<- c("Intercept", "Sexm", "TL.C", "ConditionFactor.C", "InfectionScore.C")

behvar3l$FishID <- rownames(behvar3l)
behvar3m$FishID <- rownames(behvar3m)
behvar3n$FishID <- rownames(behvar3n)
behvar3o$FishID <- rownames(behvar3o)

behvar3l$emergelat.bin.COEF <- behvar3l$Intercept
behvar3m$endpointlat.bin.COEF <- behvar3m$Intercept
behvar3n$endpointspeed.ln.COEF <- behvar3n$Intercept
behvar3o$refugereturnlat.ln.COEF <- behvar3o$Intercept

behvar3l <- select(behvar3l, -c(Intercept, Sexm, TL.C, ConditionFactor.C, InfectionScore.C))
behvar3m <- select(behvar3m, -c(Intercept, Sexm, TL.C, ConditionFactor.C, InfectionScore.C))
behvar3n <- select(behvar3n, -c(Intercept, Sexm, TL.C, ConditionFactor.C, InfectionScore.C))
behvar3o <- select(behvar3o, -c(Intercept, Sexm, TL.C, ConditionFactor.C, InfectionScore.C))

behavCOEFs <- merge(behavCOEFs, behvar3l, by = 'FishID', all.x = TRUE)
behavCOEFs <- merge(behavCOEFs, behvar3m, by = 'FishID', all.x = TRUE)
behavCOEFs <- merge(behavCOEFs, behvar3n, by = 'FishID', all.x = TRUE)
behavCOEFs <- merge(behavCOEFs, behvar3o, by = 'FishID', all.x = TRUE)


#Combining all into a big fish phenotype database
# - adding in non-behavioural data as of first trial
GULDact.processed <- read.csv('~/trophicpersonalities_A/Data_Guldborgsund/GULD_ACTdat_processed.csv', strip.white = TRUE)
GULD_phenotypes <- subset(GULDact.processed, TrialDay == 'trial 1')
GULD_phenotypes <- select(GULD_phenotypes, -c(centretime100, Notes, avespeed_tot, avespeed_mob, aveacceler, propmoving, 
                                              dist, frozenevents, timefrozen_tot, centrescore, centretime50, centretime75,
                                              Date, TimeLoaded, TrialType, TrialDay, TrialRound, ArenaID, UniqueID, X, TankID))

GULD_phenotypes <- merge(GULD_phenotypes, behavACTT1, by = 'FishID', all.x = TRUE)
GULD_phenotypes <- merge(GULD_phenotypes, behavEXPLT1, by = 'FishID', all.x = TRUE)
GULD_phenotypes <- merge(GULD_phenotypes, behavMEAN, by = 'FishID', all.x = TRUE)
GULD_phenotypes <- merge(GULD_phenotypes, behavCOEFs, by = 'FishID', all.x = TRUE)

write.csv(GULD_phenotypes, '~/trophicpersonalities_A/Data_Guldborgsund/GULD_phenotypes.csv')



#### #### 


