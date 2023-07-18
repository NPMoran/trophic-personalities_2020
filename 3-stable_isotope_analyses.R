# Title: Behavioural and trophic variation within a well-established invasive round goby population 
# Author: Nicholas Moran
# Date: June 2023



#### 3. Stable isotope analysis ####
Sys.setenv(LANG = "en")
#Loading required packages- 
library(dplyr); library(lme4); library(lmerTest); library(rptR); library(data.table)
library(ggplot2); library(ggpubr); library(RColorBrewer)
library(MixSIAR); library(SIBER)

#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

#References: 
#https://cdnsciencepub.com/doi/full/10.1139/cjz-2014-0127


# 3.1. Importing and sorting datasets ----
GULD_SIA1 <- read.csv("~/trophic-personalities_2020/dat_stableisotope/GULD_SIAbatch1dat.csv")
GULD_SIA2 <- read.csv("~/trophic-personalities_2020/dat_stableisotope/GULD_SIAbatch2dat.csv")
GULD_SIA3 <- read.csv("~/trophic-personalities_2020/dat_stableisotope/GULD_SIAbatch3dat.csv")
nrow(GULD_SIA1); nrow(GULD_SIA2); nrow(GULD_SIA3) 
labels(GULD_SIA1); labels(GULD_SIA2); labels(GULD_SIA3) 
GULD_SIAfull <- rbind(GULD_SIA1, GULD_SIA2, GULD_SIA3)
GULD_SIAfull <- subset(GULD_SIAfull, sortID != 'NA')
nrow(GULD_SIAfull); labels(GULD_SIAfull)


#Creating separte data frames depending on sample type
GULD_SIAfins <- subset(GULD_SIAfull, sortID == 'fin')
nrow(GULD_SIAfins) #round goby fish fins
GULD_SIAfins <- subset(GULD_SIAfins, sampleID != "G21C") #excluding row with NAs (sample failed in analyzer)
GULD_SIAfins <- subset(GULD_SIAfins, sampleID != "G39A") #excluding samples suspects of being mixed up in batch 1
GULD_SIAfins <- subset(GULD_SIAfins, sampleID != "G40A") #excluding samples suspects of being mixed up in batch 1
GULD_SIAfins <- rename(GULD_SIAfins, FishID = sourceID)

GULD_SIAprey <- subset(GULD_SIAfull, sortID == 'prey')
nrow(GULD_SIAprey) #round goby potential prey items

GULD_SIAprod <- subset(GULD_SIAfull, sortID == 'prod')
nrow(GULD_SIAprod) #primary producers

#GULD_SIAdisc <- subset(GULD_SIAfull, sortID == 'discrim')
#nrow(GULD_SIAdisc) #additional samples to check discrimination factor (not currently in use)



# 3.2. Lipid correction ----
#Checking C:N ratios
# ratios above 4 require lipid correction (Post et al. 2007). All values here below 4.
GULD_SIAfins$CN_ratio <- (GULD_SIAfins$C_percentage/GULD_SIAfins$N_percentage)
summary(GULD_SIAfins$CN_ratio) #Mean- 3.337, Median- 3.370, Max 3.713

cor.test(GULD_SIAfins$CN_ratio, GULD_SIAfins$d13C, method = 'spearman')
plot(GULD_SIAfins$d13C, GULD_SIAfins$CN_ratio)

# - some samples CN ratio exceeds 4, and there is a signfiicant correlation between C and CN, 
# - so the correction has been applied to this dataset
GULD_SIAfins$d13C_post <- (GULD_SIAfins$d13C - 3.32 + (0.99*GULD_SIAfins$CN_ratio))
plot(GULD_SIAfins$d13C_post, GULD_SIAfins$CN_ratio)


#Correction for Prey
GULD_SIAprey$CN_ratio <- (GULD_SIAprey$C_percentage/GULD_SIAprey$N_percentage)
summary(GULD_SIAprey$CN_ratio) #Mean- 3.783, Median- 3.370, Max 7.065
GULD_SIAprey$d13C_post <- (GULD_SIAprey$d13C - 3.32 + (0.99*GULD_SIAprey$CN_ratio))

summary(GULD_SIAprey$d13C)
summary(GULD_SIAprey$d13C_post)


# 3.3. Visualising round goby isotope distributions ----
ggplot(GULD_SIAfins) + aes(x = d15N) + geom_histogram(color="black", fill="lightblue", binwidth = 0.3) + simpletheme 
ggqqplot(GULD_SIAfins$d15N) #approximately normal, some potential outliers at the high end

ggplot(GULD_SIAfins) + aes(x = d13C_post) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(GULD_SIAfins$d13C) #approximately normal, some potential outliers at the low end



# 3.2. Variance components round goby  
#GULD_SIAfins.N.mod <- lmer(d15N ~ (1|FishID), data=GULD_SIAfins)
#save(GULD_SIAfins.N.mod, file = "./outputs_visualisations/GULD_SIA1.N.mod.RData")
load(file = "./outputs_visualisations/GULD_SIA1.N.mod.RData")

summary(GULD_SIAfins.N.mod)
x1 <- as.data.frame(VarCorr(GULD_SIAfins.N.mod))
x2 <- as.data.frame(confint(GULD_SIAfins.N.mod,oldNames=FALSE))
x2 <- x2[-3,]
x1 <- x1[,c(-1,-2,-3)]
rownames(x1) <- c("fish_N", "res_N")
rownames(x2) <- c("fish_N", "res_N")
x <- cbind(x1,x2)
colnames(x) <- c("var", "sd","sd_95LCI","sd_95UCI")
x$var_95LCI <- (x$sd_95LCI)^2
x$var_95UCI <- (x$sd_95UCI)^2

x$text <- paste(round(x$var, digits = 2), round(x$var_95LCI, digits = 2), sep = " [")
x$text <- paste(x$text, round(x$var_95UCI, digits = 2), sep = ", ")
x$text <- paste(x$text, "]", sep = "")

#GULD_SIAfins.C.mod <- lmer(d13C_post ~ (1|FishID), data=GULD_SIAfins)
#save(GULD_SIAfins.C.mod, file = "./outputs_visualisations/GULD_SIA1.C.mod.RData")
load(file = "./outputs_visualisations/GULD_SIA1.C.mod.RData")

summary(GULD_SIAfins.C.mod)
y1 <- as.data.frame(VarCorr(GULD_SIAfins.C.mod))
y2 <- as.data.frame(confint(GULD_SIAfins.C.mod,oldNames=FALSE))
y2 <- y2[-3,]
y1 <- y1[,c(-1,-2,-3)]
rownames(y1) <- c("fish_C", "res_C")
rownames(y2) <- c("fish_C", "res_C")
y <- cbind(y1,y2)
colnames(y) <- c("var", "sd","sd_95LCI","sd_95UCI")
y$var_95LCI <- (y$sd_95LCI)^2
y$var_95UCI <- (y$sd_95UCI)^2

y$teyt <- paste(round(y$var, digits = 2), round(y$var_95LCI, digits = 2), sep = " [")
y$teyt <- paste(y$teyt, round(y$var_95UCI, digits = 2), sep = ", ")
y$teyt <- paste(y$teyt, "]", sep = "")

#Manual repeatability
#0.53772813/(0.07487844 + 0.53772813) #0.8777708 for N
#2.40734862/(0.08482087+2.40734862) #0.965965 for C

#GULD_SIAfins.N.rpt <- rpt(d15N ~ (1 | FishID), grname = "FishID", data = GULD_SIAfins, datatype = "Gaussian", 
#                          nboot = 100, npermut = 0)
#save(GULD_SIAfins.N.rpt, file = "./outputs_visualisations/GULD_SIA1.N.rpt.RData")
load(file = "./outputs_visualisations/GULD_SIA1.N.rpt.RData")
GULD_SIAfins.N.rpt #0.878 (matches manual est)


#GULD_SIAfins.C.rpt <- rpt(d13C_post ~ (1 | FishID), grname = "FishID", data = GULD_SIAfins, datatype = "Gaussian", 
#                          nboot = 100, npermut = 0)
#save(GULD_SIAfins.C.rpt, file = "./outputs_visualisations/GULD_SIA1.C.rpt.RData")
load(file = "./outputs_visualisations/GULD_SIA1.C.rpt.RData")
GULD_SIAfins.C.rpt #0.966 (matches manual est)


#Range and mean estimates. 
GULD_SIAfins_means <- setDT(GULD_SIAfins)[ , list(d15N_M = mean(d15N),
                                                      d13C_post_M = mean(d13C_post)),
                                                by = .(FishID)]
summary(GULD_SIAfins_means)



?cor.test
cor.test(GULDbehav_combined$timefrozen_tot, GULDbehav_combined$emergelat, method = "spearman")
