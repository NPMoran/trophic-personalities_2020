# Title: Behavioural and trophic variation within a well-established invasive round goby population 
# Author: Nicholas Moran
# Date: June 2023



#### 3. Stable isotope analysis ####
Sys.setenv(LANG = "en")
#Loading required packages- 
library(dplyr); library(lme4); library(lmerTest); library(rptR); library(data.table); library(operators)
library(ggplot2); library(ggpubr); library(RColorBrewer)

#install.packages("devtools")
#remotes::install_github("brianstock/MixSIAR", dependencies=T)
#install.packages('rjags')
#install.packages('simmr')
#install.packages('MixSIAR')
library(rjags) #need to install JAGS-4.x.y.exe (for any x >=0, y>=0) from http://www.sourceforge.net/projects/mcmc-jags/files
library(simmr) #supposedly updated version of SIAR for running simple or lite versions of mixing models
library(MixSIAR) #MixSIAR: A Bayesian stable isotope mixing model for characterizing intrapopulation niche variation

#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))


# 3.1. Importing and processing datasets ----
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


# _ Lipid correction ----
#Checking C:N ratios
# - Ratios above 4 require lipid correction (Post et al. 2007). All values here below 4.
GULD_SIAfins$CN_ratio <- (GULD_SIAfins$C_percentage/GULD_SIAfins$N_percentage)
summary(GULD_SIAfins$CN_ratio) #Mean- 3.337, Median- 3.370, Max 3.713

cor.test(GULD_SIAfins$CN_ratio, GULD_SIAfins$d13C, method = 'spearman')
plot(GULD_SIAfins$d13C, GULD_SIAfins$CN_ratio)
#Some samples CN ratio exceeds 4, and there is a significant correlation between C and CN, so the correction has been applied to this dataset


# - Post (2002) method.

#GULD_SIAfins$d13C_post <- (GULD_SIAfins$d13C - 3.32 + (0.99*GULD_SIAfins$CN_ratio))
#plot(GULD_SIAfins$d13C_post, GULD_SIAfins$CN_ratio)

# - Kiljunen et al., (2006) method, which is more relevant to the Baltic Sea.

GULD_SIAfins$L_kilj <- 93 / (1 + ((0.246*GULD_SIAfins$CN_ratio)-0.775)^(-1)) 
GULD_SIAfins$d13C_kilj <- GULD_SIAfins$d13C + 7.018 * (0.048 + (3.90/(1+287/GULD_SIAfins$L_kilj)))
#cor.test(GULD_SIAfins$CN_ratio, GULD_SIAfins$d13C_kilj, method = 'spearman') #no sig. correlation with CN ratio
#plot(GULD_SIAfins$d13C_kilj, GULD_SIAfins$CN_ratio)




# _ Prey processing ----

#Building supps table with taxa groups and preferred diet components. 
TaxaGroups <- read.csv("~/trophic-personalities_2020/dat_stableisotope/GULD_taxagroups.csv")
labels(TaxaGroups)
TaxaGroups$Classification <- paste("Phylum: ", TaxaGroups$Phylum, sep = "")
TaxaGroups$Classification <- paste(TaxaGroups$Classification, "Class: ", sep = " ")
TaxaGroups$Classification <- paste(TaxaGroups$Classification, TaxaGroups$Class, sep = "")
TaxaGroups$Classification <- paste(TaxaGroups$Classification, "Order: ", sep = " ")
TaxaGroups$Classification <- paste(TaxaGroups$Classification, TaxaGroups$Order, sep = "")
TaxaGroups$Classification <- paste(TaxaGroups$Classification, "Family: ", sep = " ")
TaxaGroups$Classification <- paste(TaxaGroups$Classification, TaxaGroups$Family, sep = "")
TaxaGroups$Classification <- paste(TaxaGroups$Classification, TaxaGroups$Genus.Species, sep = " ")

TaxaGroups$Diet.component <- TaxaGroups$taxaID

TaxaGroups$Diet.component <- case_when(
  TaxaGroups$Diet.component %in% c("V01","V03","V04","V05","V07","V08","V09","V18") ~ "yes", #from van Duers 2022
  TaxaGroups$Diet.component %in% c("V02","V12","V15","V16") ~ "no", #from Puntila 2016
  TaxaGroups$Diet.component %in% c("V06","V10","V11","V13","V14","V17","V19","V20","V21","V22") ~ "no", #from van Duers 2022
  .default = TaxaGroups$Diet.component
)

TaxaGroups$Comments <- " "
TaxaGroups$Comments <- case_when(
  TaxaGroups$taxaID %in% c("V10","V13","V14","V20") ~ "Not analysed, insufficient biomass", 
  TaxaGroups$taxaID %in% c("V22") ~ "Juvenile round gobies, <8cm", 
  .default = TaxaGroups$Comments
)


labels(TaxaGroups)
TaxaGroups <- TaxaGroups[,c(-2:-6)]
TaxaGroups <- TaxaGroups[,c(-3:-4)]

#write.csv(TaxaGroups, "./outputs_visualisations/tablesupp-taxa.csv")
TaxaGroups <- TaxaGroups[,c(-3:-5)]
GULD_SIAprey <- rename(GULD_SIAprey, taxaID = sourceID)
GULD_SIAprey <- merge(GULD_SIAprey, TaxaGroups, by = 'taxaID', all.x = FALSE)
nrow(GULD_SIAprey)
#write.csv(GULD_SIAprey, "./dat_stableisotope/GULD_prey_processed.csv")


# 3.2. Visualising round goby isotope distributions ----
# _ Data formatting ----
#note 1x fish has only only replicate due to particularly low biomass.
ggplot(GULD_SIAfins) + aes(x = d15N) + geom_histogram(color="black", fill="lightblue", binwidth = 0.3) + simpletheme 
ggqqplot(GULD_SIAfins$d15N) #approximately normal, some potential outliers at the high end

ggplot(GULD_SIAfins) + aes(x = d13C_kilj) + geom_histogram(color="black", fill="lightblue", binwidth = 0.8) + simpletheme 
ggqqplot(GULD_SIAfins$d13C) #approximately normal, some potential outliers at the low end


#Preparing round goby data
GULD_SIAfins_meansd <- setDT(GULD_SIAfins)[ , list(d15N_M = mean(d15N),
                                                   d15N_sd = sd(d15N),
                                                   d13C_M = mean(d13C_kilj),
                                                   d13C_sd = sd(d13C_kilj)),
                                            by = .(FishID)]

#Preparing prey data
GULD_SIAprey_meansd <- setDT(GULD_SIAprey)[ , list(d15N_M = mean(d15N),
                                                   d15N_sd = sd(d15N),
                                                   d13C_M = mean(d13C),
                                                   d13C_sd = sd(d13C)),
                                            by = .(taxaID)]

GULD_SIAprey_meansd$Class <- ""
GULD_SIAprey_meansd$Class <- case_when(
  GULD_SIAprey_meansd$taxaID %in% c("V01","V02","V03") ~ "Bivalvia", 
  GULD_SIAprey_meansd$taxaID %in% c("V04","V05") ~ "Gastropoda", 
  GULD_SIAprey_meansd$taxaID %in% c("V06","V07","V08","V09") ~ "Malacostraca", 
  GULD_SIAprey_meansd$taxaID %in% c("V11") ~ "Ostracoda", 
  GULD_SIAprey_meansd$taxaID %in% c("V12") ~ "Insecta", 
  GULD_SIAprey_meansd$taxaID %in% c("V15","V16") ~ "Polychaeta", 
  GULD_SIAprey_meansd$taxaID %in% c("V17","V18","V19","V21","V22") ~ "Actinopterygii", 
  .default = GULD_SIAprey_meansd$Class
)

#Preparing prod data
GULD_SIAprod <- subset(GULD_SIAprod, sampleID != "S06B") #removing one wrongly identified data point
GULD_SIAprod <- subset(GULD_SIAprod, sampleID != "S07A") #excluding one measure without replicates.

GULD_SIAprod$prodID <- GULD_SIAprod$sampleID
GULD_SIAprod$prodID <- substring(GULD_SIAprod$prodID,0,3)
GULD_SIAprod$prodID
GULD_SIAprod_meansd <- setDT(GULD_SIAprod)[ , list(d15N_M = mean(d15N),
                                                   d15N_sd = sd(d15N),
                                                   d13C_M = mean(d13C),
                                                   d13C_sd = sd(d13C)),
                                            by = .(prodID)]

GULD_SIAprod_meansd$Type <- c("Algae","Algae","Algae","Algae","Riparian","Riparian","FPOM")
#S01 #bladder wrack
#S02 #filamentous algae
#S03 #green algae
#S04 #green algae
#S05 #terrestrial POM
#S06 #riparian veg (reeds, phragmites)
#S08 #phytoplankton/FPOM composite sample


GULD_SIAfins_meansd$ylower <- GULD_SIAfins_meansd$d15N_M - GULD_SIAfins_meansd$d15N_sd
GULD_SIAfins_meansd$yupper <- GULD_SIAfins_meansd$d15N_M + GULD_SIAfins_meansd$d15N_sd
GULD_SIAfins_meansd$xlower <- GULD_SIAfins_meansd$d13C_M - GULD_SIAfins_meansd$d13C_sd
GULD_SIAfins_meansd$xupper <- GULD_SIAfins_meansd$d13C_M + GULD_SIAfins_meansd$d13C_sd

GULD_SIAprey_meansd$ylower <- GULD_SIAprey_meansd$d15N_M - GULD_SIAprey_meansd$d15N_sd
GULD_SIAprey_meansd$yupper <- GULD_SIAprey_meansd$d15N_M + GULD_SIAprey_meansd$d15N_sd
GULD_SIAprey_meansd$xlower <- GULD_SIAprey_meansd$d13C_M - GULD_SIAprey_meansd$d13C_sd
GULD_SIAprey_meansd$xupper <- GULD_SIAprey_meansd$d13C_M + GULD_SIAprey_meansd$d13C_sd

GULD_SIAprod_meansd$ylower <- GULD_SIAprod_meansd$d15N_M - GULD_SIAprod_meansd$d15N_sd
GULD_SIAprod_meansd$yupper <- GULD_SIAprod_meansd$d15N_M + GULD_SIAprod_meansd$d15N_sd
GULD_SIAprod_meansd$xlower <- GULD_SIAprod_meansd$d13C_M - GULD_SIAprod_meansd$d13C_sd
GULD_SIAprod_meansd$xupper <- GULD_SIAprod_meansd$d13C_M + GULD_SIAprod_meansd$d13C_sd

GULD_SIAprodA_meansd <- subset(GULD_SIAprod_meansd, Type == "Algae")
GULD_SIAprodB_meansd <- subset(GULD_SIAprod_meansd, Type == "Riparian")
GULD_SIAprodB_meansd$ylower[2] <- 0
GULD_SIAprodC_meansd <- subset(GULD_SIAprod_meansd, Type == "FPOM")

GULD_SIAprod_meansd$ylower[6] <- 0
GULD_SIAprod_text <- GULD_SIAprod_meansd 
GULD_SIAprod_text$d15N_M <- GULD_SIAprod_text$ylower + 0.1
GULD_SIAprod_text$d13C_M <- GULD_SIAprod_text$xlower + 0.05

# _ The Plot ----
#General theme for SIA plots
SIAtheme <-   theme(axis.text.y = element_text(size = 8, colour = "black"), 
                    axis.text.x = element_text(size = 8, colour = "black"), 
                    panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(colour = "white", linewidth = 0.5, linetype = "dashed" ),
                    axis.title.y  = element_text(size=10, vjust = 0.1),
                    axis.title.x  = element_text(size=10, vjust = 0.1),
                    panel.border = element_rect(colour = "black", fill=NA, size = 1),
                    legend.text = element_text(size=8),
                    legend.box.spacing = unit(0.2, 'cm'),
                    legend.title = element_blank(),
                    legend.position="bottom",
                    legend.justification=c(0,0),
                    legend.key.size = unit(0.3, 'cm'))

#Invert plot
GULD_SIA.fullplot <- ggplot(GULD_SIAfins_meansd, aes (x = d13C_M, y = d15N_M, color = Class)) + 
  SIAtheme +
  geom_vline(xintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  geom_hline(yintercept = 0, linetype = 2, colour = "black", size = 0.5) +
  annotate("rect", xmin = GULD_SIAprodA_meansd$xlower, xmax = GULD_SIAprodA_meansd$xupper, ymin = GULD_SIAprodA_meansd$ylower, ymax = GULD_SIAprodA_meansd$yupper,
           alpha = 0.3, fill = "yellowgreen") +
  annotate("rect", xmin = GULD_SIAprodB_meansd$xlower, xmax = GULD_SIAprodB_meansd$xupper, ymin = GULD_SIAprodB_meansd$ylower, ymax = GULD_SIAprodB_meansd$yupper,
           alpha = 0.35,fill = "aquamarine2") +
  annotate("rect", xmin = GULD_SIAprodC_meansd$xlower, xmax = GULD_SIAprodC_meansd$xupper, ymin = GULD_SIAprodC_meansd$ylower, ymax = GULD_SIAprodC_meansd$yupper,
           alpha = 0.5, fill = "green") +
  geom_point(data = GULD_SIAfins_meansd, col = 'grey40', shape = 19, size = 2, alpha = 1) + 
  geom_segment(data = GULD_SIAfins_meansd, aes(x = d13C_M, y = ylower, xend = d13C_M, yend = yupper), size = 0.3, colour = "grey40") + 
  geom_segment(data = GULD_SIAfins_meansd, aes(x = xlower, y = d15N_M, xend = xupper, yend = d15N_M), size = 0.3, colour = "grey40") + 
  scale_x_continuous(limits = c(-30, 0), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,15), expand = c(0, 0)) +
  geom_point(data = GULD_SIAprey_meansd, aes(fill = Class), shape = 19, size = 2.5, alpha = 1) + 
  geom_segment(data = GULD_SIAprey_meansd, aes(x = d13C_M, y = ylower, xend = d13C_M, yend = yupper, colour = Class), size = 0.3) + 
  geom_segment(data = GULD_SIAprey_meansd, aes(x = xlower, y = d15N_M, xend = xupper, yend = d15N_M, colour = Class), size = 0.3) +
  geom_text(data = GULD_SIAprey_meansd, label = GULD_SIAprey_meansd$taxaID, size = 2, hjust = 1.1, vjust = 1.5, color = 'black') +
  geom_text(data = GULD_SIAprod_text, label = GULD_SIAprod_text$prodID, size = 2, hjust = 0, vjust = 0, color = 'black') +
  #  geom_text(data = GULD_SIAprod_meansd, label = GULD_SIAprod_meansd$Type, size = 2.1, color = 'black') +
  scale_color_manual(values = c("Bivalvia" = "dodgerblue2",
                              "Gastropoda"="plum3",
                              "Malacostraca"="chartreuse2",
                              "Ostracoda"="red2",
                              "Insecta"="yellow4",
                              "Polychaeta"="orange",
                              "Actinopterygii"="lightblue3")) +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)")))
GULD_SIA.fullplot

#ggsave("./outputs_visualisations/Fig_kilj.jpeg", width = 18, height = 12, units = "cm", GULD_SIA.fullplot, dpi = 600)


#Notes
# - note that there was insufficient biomass for replicates for one individual (G48), so error bars were not printed)
# - G34 is an extreme outlier, which may suggest that it is a recent immigrant and may not be included in diet analysis. 

# _ Fish G34 ----

#GULD_correlations$d13C_M.C <- scale(GULD_correlations$d13C_M) 
#GULD_correlations$d15N_M.C <- scale(GULD_correlations$d15N_M) 
# - Z-score for mean is >3 for both mean C and N

#write.csv(GULD_SIAfins, "./dat_stableisotope/GULD_goby_processed.csv")


# 3.3. Round goby variance analysis ----
# _ Variance component models ---- 

#Importing data and removing outlier,
GULD_SIAfins <- read.csv("./dat_stableisotope/GULD_goby_processed.csv")
GULD_SIAfins <- subset(GULD_SIAfins, FishID != "G34")


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


#GULD_SIAfins.C.mod <- lmer(d13C_kilj ~ (1|FishID), data=GULD_SIAfins)
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



# _ Repeatability ---- 

#GULD_SIAfins.N.rpt <- rpt(d15N ~ (1 | FishID), grname = "FishID", data = GULD_SIAfins, datatype = "Gaussian", 
#                          nboot = 100, npermut = 0)
#save(GULD_SIAfins.N.rpt, file = "./outputs_visualisations/GULD_SIA1.N.rpt.RData")
load(file = "./outputs_visualisations/GULD_SIA1.N.rpt.RData")
GULD_SIAfins.N.rpt #0.848


#GULD_SIAfins.C.rpt <- rpt(d13C_kilj ~ (1 | FishID), grname = "FishID", data = GULD_SIAfins, datatype = "Gaussian", 
#                          nboot = 100, npermut = 0)
#save(GULD_SIAfins.C.rpt, file = "./outputs_visualisations/GULD_SIA1.C.rpt.RData")
load(file = "./outputs_visualisations/GULD_SIA1.C.rpt.RData")
GULD_SIAfins.C.rpt #0.925


# _ State effects ---- 
GULD_physdat <- read.csv("~/trophic-personalities_2020/dat_fish/GULD_physdat_processed.csv")
GULD_SIAfins_merge <- merge(GULD_SIAfins, GULD_physdat, by = "FishID", all.x = TRUE) 

#  note, G43, G37, G33, G15, G01 died before physical measurements.

#  scaling continuous variables
GULD_SIAfins_merge$TL.C <- scale(GULD_SIAfins_merge$TL)
GULD_SIAfins_merge$CondManual.C <- scale(GULD_SIAfins_merge$CondManual)

#  running models
#GULD_SIAfins.N.modstate <- lmer(d15N ~ TL.C + CondManual.C + Sex + (1|FishID), data=GULD_SIAfins_merge)
#save(GULD_SIAfins.N.modstate, file = "./outputs_visualisations/GULD_SIAfins.N.modstate.RData")
load(file = "./outputs_visualisations/GULD_SIAfins.N.modstate.RData")
summary(GULD_SIAfins.N.modstate)
confint(GULD_SIAfins.N.modstate)
fix_text <- as.data.frame(summary(GULD_SIAfins.N.modstate)$coefficients)
ci_text <- as.data.frame(confint(GULD_SIAfins.N.modstate))
ci_text <- ci_text[-1:-2,]
fix_text$text <- paste(round(fix_text$Estimate, digits = 2), round(ci_text$`2.5 %`, digits = 2), sep = " [")
fix_text$text <- paste(fix_text$text, round(ci_text$`97.5 %`, digits = 2), sep = ", ")
fix_text$text <- paste(fix_text$text, "]", sep = "")

#GULD_SIAfins.C.modstate <- lmer(d13C_kilj ~ TL.C + CondManual.C + Sex + (1|FishID), data=GULD_SIAfins_merge)
#save(GULD_SIAfins.C.mod, file = "./outputs_visualisations/GULD_SIAfins.C.modstate.RData")
load(file = "./outputs_visualisations/GULD_SIAfins.C.modstate.RData")
summary(GULD_SIAfins.C.modstate)
confint(GULD_SIAfins.C.modstate)
fix_text <- as.data.frame(summary(GULD_SIAfins.C.modstate)$coefficients)
ci_text <- as.data.frame(confint(GULD_SIAfins.N.modstate))
ci_text <- ci_text[-1:-2,]
fix_text$text <- paste(round(fix_text$Estimate, digits = 2), round(ci_text$`2.5 %`, digits = 2), sep = " [")
fix_text$text <- paste(fix_text$text, round(ci_text$`97.5 %`, digits = 2), sep = ", ")
fix_text$text <- paste(fix_text$text, "]", sep = "")


#Range and means - 
#summary(GULD_SIAfins_meansd)
#summary(subset(GULD_SIAfins_meansd, FishID != 'G34'))



# _ Supplementary analysis: mortality bias ----

#Combining phenotypic and isotopic data
Pheno_working <- read.csv('~/trophic-personalities_2020/dat_fish/GULD_physdat_processed.csv')
GULD_mortalitychecks <- merge(GULD_SIAfins_meansd,Pheno_working, by = "FishID", all.x = TRUE)

#Creating variable for survivors versus non-survivors. 
Behav_means <- read.csv('~/trophic-personalities_2020/dat_behaviour/GULDbehav_phenotypes.csv')
Behav_means <- subset(Behav_means, dist_M != 'NA')
nrow(Behav_means) #as only those who survived the whole experiment have mean values for this variable
n_distinct(Behav_means$FishID)
Behav_means <- Behav_means[,1:2]
Behav_means$Status <- "1"
Behav_means <- Behav_means[,-2]

GULD_mortalitychecks <- merge(GULD_mortalitychecks, Behav_means, by = "FishID", all.x = TRUE)

#Creating categorical variable
GULD_mortalitychecks$Status <- case_when(
  GULD_mortalitychecks$Status %in% c(NA) ~ '0',
  .default = GULD_mortalitychecks$Status
)

#Creating binary sex variable
GULD_mortalitychecks$Sex_bin <- NA
GULD_mortalitychecks$Sex_bin <- case_when(
  GULD_mortalitychecks$Sex %in% c(NA, 'j') ~ 'NA',
  GULD_mortalitychecks$Sex %in% c('m') ~ 'm',
  GULD_mortalitychecks$Sex %in% c('f') ~ 'f',
  .default = GULD_mortalitychecks$Sex_bin
)
#GULD_mortalitychecks_sex <- subset(GULD_mortalitychecks, Sex_bin != 'NA')
#GULD_mortalitychecks_sex$Sex_bin <- as.numeric(GULD_mortalitychecks_sex$Sex_bin)

GULD_mortalitychecks$Status <- as.numeric(GULD_mortalitychecks$Status)

summary(glm(Status ~ CondManual, family = binomial, GULD_mortalitychecks))
summary(glm(Status ~ TL, family = binomial, GULD_mortalitychecks))
summary(glm(Status ~ Sex_bin, family = binomial, GULD_mortalitychecks))
summary(glm(Status ~ d15N_M, family = binomial, GULD_mortalitychecks))
summary(glm(Status ~ d13C_M, family = binomial, GULD_mortalitychecks))





# 3.4. Trophic x behavioural correlations ----
# _ Data frame ----
Behav_working <- read.csv('~/trophic-personalities_2020/dat_behaviour/GULDbehav_phenotypes.csv')
SIA_working <- GULD_SIAfins_meansd[,c(1:5)]

GULD_correlations <- merge(Behav_working,SIA_working, by = "FishID", all.x = TRUE)

GULD_correlations <- subset(GULD_correlations, FishID != "G34")

# _ Correlation tests ----
corr_C1_F <- cor.test(GULD_correlations$d13C_M, GULD_correlations$dist, method = "spearman")
corr_C2_F <- cor.test(GULD_correlations$d13C_M, GULD_correlations$avespeed_mob, method = "spearman")
corr_C3_F <- cor.test(GULD_correlations$d13C_M, GULD_correlations$timefrozen_tot, method = "spearman")
corr_C4_F <- cor.test(GULD_correlations$d13C_M, GULD_correlations$centrescore2, method = "spearman")
corr_C5_F <- cor.test(GULD_correlations$d13C_M, GULD_correlations$emergelat, method = "spearman")
corr_C6_F <- cor.test(GULD_correlations$d13C_M, GULD_correlations$endpointlat, method = "spearman")

corr_C1_M <- cor.test(GULD_correlations$d13C_M, GULD_correlations$dist_M, method = "spearman")
corr_C2_M <- cor.test(GULD_correlations$d13C_M, GULD_correlations$avespeed_mob_M, method = "spearman")
corr_C3_M <- cor.test(GULD_correlations$d13C_M, GULD_correlations$timefrozen_tot_M, method = "spearman")
corr_C4_M <- cor.test(GULD_correlations$d13C_M, GULD_correlations$centrescore2_M, method = "spearman")
corr_C5_M <- cor.test(GULD_correlations$d13C_M, GULD_correlations$emergelat_M, method = "spearman")
corr_C6_M <- cor.test(GULD_correlations$d13C_M, GULD_correlations$endpointlat_M, method = "spearman")

corr_N1_F <- cor.test(GULD_correlations$d15N_M, GULD_correlations$dist, method = "spearman")
corr_N2_F <- cor.test(GULD_correlations$d15N_M, GULD_correlations$avespeed_mob, method = "spearman")
corr_N3_F <- cor.test(GULD_correlations$d15N_M, GULD_correlations$timefrozen_tot, method = "spearman")
corr_N4_F <- cor.test(GULD_correlations$d15N_M, GULD_correlations$centrescore2, method = "spearman")
corr_N5_F <- cor.test(GULD_correlations$d15N_M, GULD_correlations$emergelat, method = "spearman")
corr_N6_F <- cor.test(GULD_correlations$d15N_M, GULD_correlations$endpointlat, method = "spearman")

corr_N1_M <- cor.test(GULD_correlations$d15N_M, GULD_correlations$dist_M, method = "spearman")
corr_N2_M <- cor.test(GULD_correlations$d15N_M, GULD_correlations$avespeed_mob_M, method = "spearman")
corr_N3_M <- cor.test(GULD_correlations$d15N_M, GULD_correlations$timefrozen_tot_M, method = "spearman")
corr_N4_M <- cor.test(GULD_correlations$d15N_M, GULD_correlations$centrescore2_M, method = "spearman")
corr_N5_M <- cor.test(GULD_correlations$d15N_M, GULD_correlations$emergelat_M, method = "spearman")
corr_N6_M <- cor.test(GULD_correlations$d15N_M, GULD_correlations$endpointlat_M, method = "spearman")


corre <- as.data.frame(c(corr_C1_F$estimate,corr_C2_F$estimate,corr_C3_F$estimate,corr_C4_F$estimate,
                         corr_C5_F$estimate,corr_C6_F$estimate,corr_C1_M$estimate,corr_C2_M$estimate,
                         corr_C3_M$estimate,corr_C4_M$estimate,corr_C5_M$estimate,corr_C6_M$estimate,
                         corr_N1_F$estimate,corr_N2_F$estimate,corr_N3_F$estimate,corr_N4_F$estimate,
                         corr_N5_F$estimate,corr_N6_F$estimate,corr_N1_M$estimate,corr_N2_M$estimate,
                         corr_N3_M$estimate,corr_N4_M$estimate,corr_N5_M$estimate,corr_N6_M$estimate))
colnames(corre) <- "a" 
corre$a <- round(corre$a, digits = 3) 

corrp <- as.data.frame(c(corr_C1_F$p.value,corr_C2_F$p.value,corr_C3_F$p.value,corr_C4_F$p.value,
                         corr_C5_F$p.value,corr_C6_F$p.value,corr_C1_M$p.value,corr_C2_M$p.value,
                         corr_C3_M$p.value,corr_C4_M$p.value,corr_C5_M$p.value,corr_C6_M$p.value,
                         corr_N1_F$p.value,corr_N2_F$p.value,corr_N3_F$p.value,corr_N4_F$p.value,
                         corr_N5_F$p.value,corr_N6_F$p.value,corr_N1_M$p.value,corr_N2_M$p.value,
                         corr_N3_M$p.value,corr_N4_M$p.value,corr_N5_M$p.value,corr_N6_M$p.value))
colnames(corrp) <- "a"
corrp$rownames <- rownames(corrp)
corrp$a <- round(corrp$a, digits = 3)
corrp$a <- paste("(P = ", corrp$a, sep = "")
corrp$a <- paste(corrp$a, ")", sep = "")
corrp$rownames <- as.numeric(corrp$rownames) 
corrp <- corrp[order(corrp$rownames,decreasing=FALSE),]

corre$a <- paste(corre$a, corrp$a, sep = " ")

table4 <- cbind(corre[1:6,],corre[7:12,],corre[13:18,],corre[19:24,])

#plot(GULD_correlations$centrescore2_M, GULD_correlations$d13C_M)

#write.csv(table4, "./outputs_visualisations/table4.csv")


# 3.5. Diet reconstruction (mixSIAR)----

#Importing data and removing outlier,
GULD_SIAfins <- read.csv("./dat_stableisotope/GULD_goby_processed.csv")
GULD_SIAfins <- subset(GULD_SIAfins, FishID != "G34")

# _ Discrimination factors ----
# - Based on Poslednik 2023
TDF_Poslednik_N_mean <- 4.04
TDF_Poslednik_N_sd <- 0.32 * sqrt(64)
TDF_Poslednik_C_mean <- -0.41
TDF_Poslednik_C_sd <- 0.32 * sqrt(64)

#SD = SE*(sqrt(n))
#N = 64

# - Based on Post 2002
TDF_Post_N_mean <- 3.4
TDF_Post_N_sd <- 0.98
TDF_Post_C_mean <- 0.39
TDF_Post_C_sd <- 1.3


# _ Building data frames for analysis ----
#A- for consumers (i.e., round gobies)
GULD_consumers <- NULL
GULD_consumers$d15N <- GULD_SIAfins$d15N
GULD_consumers$d13C <- GULD_SIAfins$d13C_kilj
GULD_consumers$Individual <- GULD_SIAfins$FishID 
GULD_consumers <- as.data.frame(GULD_consumers)

#write.csv(GULD_consumers, '~/trophic-personalities_2020/dat_stableisotope/GULD_consumers.csv', row.names = FALSE)


#B- for potential prey items (i.e., preferred prey items per Van Deurs 2021)
#Pooling by -
#P01 = V01, V03, Bivalves (currently not including blue mussels V02)
#P02 = V04, V05, Gastropods
#P03 = V07, Malacostraca (decapods only, currently crangon V06 not included)
#P04 = V08, V09, Malacostraca (isopods/amphipods)
#P05 = V18; Actinopterygii (currently doesn't include V17)

GULD_sources <- NULL
GULD_sources$source <- c("V01","V03","V04","V05","V07","V08","V09","V18")
GULD_SIAprey_working <- subset(GULD_SIAprey, taxaID %in% GULD_sources$source)
GULD_SIAprey_working$taxaID <- case_when(
  GULD_SIAprey_working$taxaID %in% c("V01", "V03") ~ "P01",
  GULD_SIAprey_working$taxaID %in% c("V04", "V05") ~ "P02",
  GULD_SIAprey_working$taxaID %in% c("V07") ~ "P03",
  GULD_SIAprey_working$taxaID %in% c("V08", "V09") ~ "P04",
  GULD_SIAprey_working$taxaID %in% c("V18") ~ "P05",
  .default = GULD_SIAprey_working$taxaID
)
GULD_SIAprey_working$source <- GULD_SIAprey_working$taxaID
GULD_SIAprey_working$n <- 1


GULD_sources <- setDT(GULD_SIAprey_working)[ , list(Meand15N = mean(d15N),
                                            SDd15N = sd(d15N),
                                            Meand13C = mean(d13C),
                                            SDd13C = sd(d13C),
                                            n = sum(n)),
                                     by = .(source)]
GULD_sources <- as.data.frame(GULD_sources)

#write.csv(GULD_sources, '~/trophic-personalities_2020/dat_stableisotope/GULD_sources.csv', row.names = FALSE)


#B.2 - for potential prey items (i.e., using an expanded group of prey items per Van Deurs, Puntila 2016, Kornis 2015 and Oesterwind et al., 2017)
#Pooling by -
#P01 = V01, V02, V03, Bivalves
#P02 = V04, V05, Gastropods
#P03 = V06. V07, Malacostraca (decapods)
#P04 = V08, V09, Malacostraca (isopods/amphipods)
#P05 = V11, Ostracods
#P06 = V12, Insects
#P07 = V15, V16 Polychaetes
#P08 = V17, V18, Actinopterygii 
#Excluded: V19 (Syngnathidae spp); V21 (Gobiidae spp.); V22 (N. melanostomus)

GULD_sources2 <- NULL
GULD_sources2$source <- c("V01","V02","V03","V04","V05","V06","V07","V08","V09","V11","V12","V15","V16","V17","V18")
GULD_SIAprey_working2 <- subset(GULD_SIAprey, taxaID %in% GULD_sources2$source)
GULD_SIAprey_working2$taxaID <- case_when(
  GULD_SIAprey_working2$taxaID %in% c("V01", "V02", "V03") ~ "P01",
  GULD_SIAprey_working2$taxaID %in% c("V04", "V05") ~ "P02",
  GULD_SIAprey_working2$taxaID %in% c("V06", "V07") ~ "P03",
  GULD_SIAprey_working2$taxaID %in% c("V08", "V09") ~ "P04",
  GULD_SIAprey_working2$taxaID %in% c("V11") ~ "P05",
  GULD_SIAprey_working2$taxaID %in% c("V12") ~ "P06",
  GULD_SIAprey_working2$taxaID %in% c("V15", "V16") ~ "P07",
  GULD_SIAprey_working2$taxaID %in% c("V17", "V18") ~ "P08",
  .default = GULD_SIAprey_working2$taxaID
)
GULD_SIAprey_working2$source <- GULD_SIAprey_working2$taxaID
GULD_SIAprey_working2$n <- 1


GULD_sources2 <- setDT(GULD_SIAprey_working2)[ , list(Meand15N = mean(d15N),
                                                    SDd15N = sd(d15N),
                                                    Meand13C = mean(d13C),
                                                    SDd13C = sd(d13C),
                                                    n = sum(n)),
                                             by = .(source)]
GULD_sources2 <- as.data.frame(GULD_sources2)

#write.csv(GULD_sources2, '~/trophic-personalities_2020/dat_stableisotope/GULD_sources2.csv', row.names = FALSE)


#C - for discrimination factors
GULD_TDFs <- NULL #for main analysis
working <- c(1,2,3,4,5)
GULD_TDFs$rownumber <- working
GULD_TDFs$source <- GULD_sources$source
GULD_TDFs <- as.data.frame(GULD_TDFs)
GULD_TDFs$Meand15N <- TDF_Poslednik_N_mean
GULD_TDFs$SDd15N <- TDF_Poslednik_N_sd
GULD_TDFs$Meand13C <- TDF_Poslednik_C_mean
GULD_TDFs$SDd13C <- TDF_Poslednik_C_sd
GULD_TDFs <- GULD_TDFs[,-1]
#write.csv(GULD_TDFs, '~/trophic-personalities_2020/dat_stableisotope/GULD_TDF1.csv', row.names = FALSE)
GULD_TDFs2 <- GULD_TDFs #for TDF sensitvity analysis
GULD_TDFs2$Meand15N <- TDF_Post_N_mean
GULD_TDFs2$SDd15N <- TDF_Post_N_sd
GULD_TDFs2$Meand13C <- TDF_Post_C_mean
GULD_TDFs2$SDd13C <- TDF_Post_C_sd
#write.csv(GULD_TDFs2, '~/trophic-personalities_2020/dat_stableisotope/GULD_TDF2.csv', row.names = FALSE)
GULD_TDFs3 <- NULL #for expaned prey analysis.
working <- c(1,2,3,4,5,6,7,8)
GULD_TDFs3$rownumber <- working
GULD_TDFs3$source <- GULD_sources2$source
GULD_TDFs3 <- as.data.frame(GULD_TDFs3)
GULD_TDFs3$Meand15N <- TDF_Poslednik_N_mean
GULD_TDFs3$SDd15N <- TDF_Poslednik_N_sd
GULD_TDFs3$Meand13C <- TDF_Poslednik_C_mean
GULD_TDFs3$SDd13C <- TDF_Poslednik_C_sd
GULD_TDFs3 <- GULD_TDFs3[,-1]
#write.csv(GULD_TDFs3, '~/trophic-personalities_2020/dat_stableisotope/GULD_TDF3.csv', row.names = FALSE)


#Combinations for analysis
# - Main model
# mix (GULD_consumers), source (GULD_sources), discr1 (GULD_TDFs) 
# - TDF Sensitivity model
# mix (GULD_consumers), source (GULD_sources), discr1 (GULD_TDFs2)
# - Expanded prey groups model
# mix (GULD_consumers), source (GULD_sources2), discr1 (GULD_TDFs3)


# _ MixSIAR mods ----
#load consumer data
mix <- load_mix_data(filename="~/trophic-personalities_2020/dat_stableisotope/GULD_consumers.csv", 
                     iso_names=c("d13C","d15N"), 
                     factors="Individual", 
                     fac_random=TRUE, 
                     fac_nested=NULL,
                     cont_effects=NULL)

#load source data
source <- load_source_data(filename="~/trophic-personalities_2020/dat_stableisotope/GULD_sources.csv", 
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", mix)
source2 <- load_source_data(filename="~/trophic-personalities_2020/dat_stableisotope/GULD_sources2.csv", 
                           source_factors=NULL, 
                           conc_dep=FALSE, 
                           data_type="means", mix)

#load discr data
discr1 <- load_discr_data(filename="~/trophic-personalities_2020/dat_stableisotope/GULD_TDF1.csv", mix)
discr2 <- load_discr_data(filename="~/trophic-personalities_2020/dat_stableisotope/GULD_TDF2.csv", mix)
discr3 <- load_discr_data(filename="~/trophic-personalities_2020/dat_stableisotope/GULD_TDF3.csv", mix)


###Isospace plots
isoplot1 <- plot_data(filename="isospace_plot", return_obj=TRUE,
            plot_save_pdf=FALSE,
            plot_save_png=FALSE,
            mix,source,discr1)
isoplot1 <- isoplot1 + theme(legend.position = 'right',
                             legend.text = element_text(size=7),
                             legend.spacing.x = unit(0.5, 'cm'))

isoplot2 <- plot_data(filename="isospace_plot", return_obj=TRUE,
            plot_save_pdf=FALSE,
            plot_save_png=FALSE,
            mix,source,discr2)
isoplot2 <- isoplot2 + theme(legend.position = 'right',
                             legend.text = element_text(size=7),
                             legend.spacing.x = unit(0.5, 'cm'))

isoplot3 <- plot_data(filename="isospace_plot", return_obj=TRUE,
                      plot_save_pdf=FALSE,
                      plot_save_png=FALSE,
                      mix,source2,discr3)
isoplot3 <- isoplot3 + theme(legend.position = 'right',
                             legend.text = element_text(size=7),
                             legend.spacing.x = unit(0.5, 'cm'))


#ggsave("./outputs_visualisations/Fig_isoplot1.jpeg", width = 20, height = 14, units = "cm", isoplot1, dpi = 600)
#ggsave("./outputs_visualisations/Fig_isoplot2.jpeg", width = 20, height = 14, units = "cm", isoplot2, dpi = 600)
#ggsave("./outputs_visualisations/Fig_isoplot3.jpeg", width = 20, height = 14, units = "cm", isoplot3, dpi = 600)


###Writing jags models
#Prior
#plot_prior(alpha.prior=1,source)

#Defining model structure
model_filename <- "MixSIAR_model.txt"
resid_err <- TRUE
process_err <- TRUE
write_JAGS_model(model_filename, resid_err, process_err, mix, source)

#Running test
#GULD_jags_main <- run_model(run="long", mix, source, discr1, model_filename, 
                    alpha.prior = 1, resid_err, process_err)
#save(GULD_jags_main, file = "./outputs_visualisations/GULD_jags_main.RData")
load("./outputs_visualisations/GULD_jags_main.RData")

#GULD_jags_TDFpost <- run_model(run="long", mix, source, discr2, model_filename, 
#                         alpha.prior = 1, resid_err, process_err)
#save(GULD_jags_TDFpost, file = "./outputs_visualisations/GULD_jags_TDFpost.RData")
load("./outputs_visualisations/GULD_jags_TDFpost.RData")

#write_JAGS_model(model_filename, resid_err, process_err, mix, source2)
#GULD_jags_expanded <- run_model(run="long", mix, source2, discr3, model_filename, 
#                         alpha.prior = 1, resid_err, process_err)
#save(GULD_jags_expanded, file = "./outputs_visualisations/GULD_jags_expanded.RData")
#load("./outputs_visualisations/GULD_jags_expanded.RData")

#output options
output_options <- list(summary_save = TRUE,                 
                       summary_name = "summary_statistics", 
                       sup_post = TRUE,                    
                       plot_post_save_pdf = FALSE,           
                       plot_post_name = "posterior_density",
                       sup_pairs = TRUE,             
                       plot_pairs_save_pdf = TRUE,    
                       plot_pairs_name = "pairs_plot",
                       sup_xy = TRUE,           
                       plot_xy_save_pdf = TRUE,
                       plot_xy_name = "xy_plot",
                       gelman = TRUE,
                       heidel = FALSE,  
                       geweke = TRUE,   
                       diag_save = TRUE,
                       diag_name = "diagnostics",
                       indiv_effect = FALSE,       
                       plot_post_save_png = FALSE, 
                       plot_pairs_save_png = FALSE,
                       plot_xy_save_png = FALSE,
                       diag_save_ggmcmc = FALSE,
                       return_obj = TRUE)

#diagnostics, summary statistics, and posterior plots
# - main model
output_JAGS(GULD_jags_main, mix, source, output_options)
diag <- output_diagnostics(GULD_jags_main, mix, source, output_options)
df.stats <- output_stats(GULD_jags_main, mix, source, output_options)
g.post <- output_posteriors(GULD_jags_main, mix, source, output_options)

Fig_global <- g.post$global + simpletheme + 
  scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.05), expand = c(0, 0)) +
  scale_fill_manual(values=c("dodgerblue2", 
                           "plum3",
                           "chartreuse2",
                           "chartreuse3",
                           "lightblue3"), 
                    labels=c('Bivalvia', 'Gastropoda', 'Malacostraca (Decapoda)', 'Malacostraca (Other)', 'Actinopterygii')) +
  scale_color_manual(values=c("dodgerblue2", 
                           "plum3",
                           "chartreuse2",
                           "chartreuse3",
                           "lightblue3"),
                     labels=c('Bivalvia', 'Gastropoda', 'Malacostraca (Decapoda)', 'Malacostraca (Other)', 'Actinopterygii')) +
  theme(axis.text.y = element_text(size = 8, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"),  
        panel.background = element_rect(fill = "white"), 
        axis.title.y  = element_text(size=10, vjust = 2), 
        axis.title.x  = element_text(size=10, vjust = 0.1), 
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        title = element_blank(),
        legend.background = element_blank()) +
 ylab("Scaled posterior density") +
 xlab("Diet proportion")
Fig_global

#ggsave("./outputs_visualisations/Fig_global.jpeg", width = 18, height = 8, units = "cm", Fig_global, dpi = 600)

df.stats.df <- as.data.frame(df.stats)
df.stats.df$text <- paste(round((df.stats.df$Mean*100), digits = 2), round((df.stats.df$`2.5%`*100), digits = 2), sep = '% [')
df.stats.df$text <- paste(df.stats.df$text, round((df.stats.df$`97.5%`*100), digits = 2), sep = '%, ')
df.stats.df$text <- paste(df.stats.df$text, '%]', sep = '')
#write.csv(df.stats.df, '~/trophic-personalities_2020/outputs_visualisations/GULD_jags_main/GULD_jags_main.df.csv', row.names = TRUE)


# - TDF post model
output_JAGS(GULD_jags_TDFpost, mix, source, output_options)
diag <- output_diagnostics(GULD_jags_TDFpost, mix, source, output_options)
df.stats <- output_stats(GULD_jags_TDFpost, mix, source, output_options)
g.post <- output_posteriors(GULD_jags_TDFpost, mix, source, output_options)

Fig_global2 <- g.post$global + simpletheme + 
  scale_x_continuous(limits = c(0,1), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,1.05), expand = c(0, 0)) +
  scale_fill_manual(values=c("dodgerblue2", 
                             "plum3",
                             "chartreuse2",
                             "chartreuse3",
                             "lightblue3"), 
                    labels=c('Bivalvia', 'Gastropoda', 'Malacostraca (Decapoda)', 'Malacostraca (Other)', 'Actinopterygii')) +
  scale_color_manual(values=c("dodgerblue2", 
                              "plum3",
                              "chartreuse2",
                              "chartreuse3",
                              "lightblue3"),
                     labels=c('Bivalvia', 'Gastropoda', 'Malacostraca (Decapoda)', 'Malacostraca (Other)', 'Actinopterygii')) +
  theme(axis.text.y = element_text(size = 8, colour = "black"), 
        axis.text.x = element_text(size = 8, colour = "black"),  
        panel.background = element_rect(fill = "white"), 
        axis.title.y  = element_text(size=10, vjust = 2), 
        axis.title.x  = element_text(size=10, vjust = 0.1), 
        panel.border = element_rect(colour = "black", fill=NA, linewidth = 1),
        title = element_blank(),
        legend.background = element_blank()) +
  ylab("Scaled posterior density") +
  xlab("Diet proportion")
Fig_global2

ggsave("./outputs_visualisations/Fig_global_TDFpost.jpeg", width = 18, height = 8, units = "cm", Fig_global2, dpi = 600)

df.stats.df <- as.data.frame(df.stats)
df.stats.df$text <- paste(round((df.stats.df$Mean*100), digits = 2), round((df.stats.df$`2.5%`*100), digits = 2), sep = '% [')
df.stats.df$text <- paste(df.stats.df$text, round((df.stats.df$`97.5%`*100), digits = 2), sep = '%, ')
df.stats.df$text <- paste(df.stats.df$text, '%]', sep = '')
#write.csv(df.stats.df, '~/trophic-personalities_2020/outputs_visualisations/GULD_jags_TDFpost.df.stats.df.csv', row.names = TRUE)


# - Expanded model (convergence issues)
#output_JAGS(GULD_jags_expanded, mix, source2, output_options)
#diag <- output_diagnostics(GULD_jags_expanded, mix, source2, output_options)
#df.stats <- output_stats(GULD_jags_expanded, mix, source2, output_options)
#g.post <- output_posteriors(GULD_jags_expanded, mix, source2, output_options)






#### ... #### 