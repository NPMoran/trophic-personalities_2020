# Title: Behavioural and trophic variation within a well-established invasive round goby population 
# Author: Nicholas Moran
# Date: June 2023



#### 3. Stable isotope analysis ####
Sys.setenv(LANG = "en")
#Loading required packages- 
library(dplyr); library(lme4); library(lmerTest); library(rptR); library(data.table)
library(ggplot2); library(ggpubr); library(RColorBrewer)
library(MixSIAR); library(SIBER); library(tRophicPosition)


#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))

#References: 
#https://cdnsciencepub.com/doi/full/10.1139/cjz-2014-0127


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

#GULD_SIAdisc <- subset(GULD_SIAfull, sortID == 'discrim')
#nrow(GULD_SIAdisc) #additional samples to check discrimination factor (not currently in use)



# - Lipid correction ----
# - Checking C:N ratios
#   ratios above 4 require lipid correction (Post et al. 2007). All values here below 4.
GULD_SIAfins$CN_ratio <- (GULD_SIAfins$C_percentage/GULD_SIAfins$N_percentage)
summary(GULD_SIAfins$CN_ratio) #Mean- 3.337, Median- 3.370, Max 3.713

cor.test(GULD_SIAfins$CN_ratio, GULD_SIAfins$d13C, method = 'spearman')
plot(GULD_SIAfins$d13C, GULD_SIAfins$CN_ratio)

# - Some samples CN ratio exceeds 4, and there is a signfiicant correlation between C and CN, so the correction has been applied to this dataset
GULD_SIAfins$d13C_post <- (GULD_SIAfins$d13C - 3.32 + (0.99*GULD_SIAfins$CN_ratio))
plot(GULD_SIAfins$d13C_post, GULD_SIAfins$CN_ratio)

#alternate mothod, which is more relevant to the Baltic Sea.
GULD_SIAfins$L_kilj <- 93 / (1 + ((0.246*GULD_SIAfins$CN_ratio)-0.775)^(-1)) 
GULD_SIAfins$d13C_kilj <- GULD_SIAfins$d13C + 7.018 * (0.048 + (3.90/(1+287/GULD_SIAfins$L_kilj)))


#summary(GULD_SIAfins$d13C)
#summary(GULD_SIAfins$d13C_post)
summary(GULD_SIAfins$d13C_kilj)



# - Prey Processing ----
# - building supps table with taxa groups and preferred diet components. 
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
  TaxaGroups$Diet.component %in% c("V02","V12","V15","V16") ~ "yes", #from Puntila 2016
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


# 3.2. Variance components round goby ----
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

#Manual repeatability
#0.53772813/(0.07487844 + 0.53772813) #0.8777708 for N
#2.2304194/(0.1315285+2.2304194) #0.9443135 for C

#GULD_SIAfins.N.rpt <- rpt(d15N ~ (1 | FishID), grname = "FishID", data = GULD_SIAfins, datatype = "Gaussian", 
#                          nboot = 100, npermut = 0)
#save(GULD_SIAfins.N.rpt, file = "./outputs_visualisations/GULD_SIA1.N.rpt.RData")
load(file = "./outputs_visualisations/GULD_SIA1.N.rpt.RData")
GULD_SIAfins.N.rpt #0.878 (matches manual est)


GULD_SIAfins.C.rpt <- rpt(d13C_kilj ~ (1 | FishID), grname = "FishID", data = GULD_SIAfins, datatype = "Gaussian", 
                          nboot = 100, npermut = 0)
save(GULD_SIAfins.C.rpt, file = "./outputs_visualisations/GULD_SIA1.C.rpt.RData")
load(file = "./outputs_visualisations/GULD_SIA1.C.rpt.RData")
GULD_SIAfins.C.rpt #0.966 (matches manual est)


#Range and mean estimates. 
GULD_SIAfins_meansd <- setDT(GULD_SIAfins)[ , list(d15N_M = mean(d15N),
                                                   d15N_sd = sd(d15N),
                                                   d13C_M = mean(d13C_kilj),
                                                   d13C_sd = sd(d13C_kilj)),
                                                by = .(FishID)]
summary(GULD_SIAfins_meansd)
#note 1x fish has only onle replicate due to particuarly low biomass.



# 3.3. Visualising round goby isotope distributions ----
ggplot(GULD_SIAfins) + aes(x = d15N) + geom_histogram(color="black", fill="lightblue", binwidth = 0.3) + simpletheme 
ggqqplot(GULD_SIAfins$d15N) #approximately normal, some potential outliers at the high end

ggplot(GULD_SIAfins) + aes(x = d13C_post) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(GULD_SIAfins$d13C) #approximately normal, some potential outliers at the low end



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

ggsave("./outputs_visualisations/Fig_3kilj.jpeg", width = 18, height = 12, units = "cm", GULD_SIA.fullplot, dpi = 600)


#Notes
# - G34 is an extreme outlier, which may suggest that it is a recent immigrant and should not be included in diet analysis. 
# - note that there was insufficient biomass for replicates for one individual (G48), so error bars were not printed)


#write.csv(GULD_SIAfins, "./dat_stableisotope/GULD_goby_processed.csv")


# 3.4. Diet reconstruction (mixSIAR)----

#Discrimination options
#Based on Post 2002, McCutchan et al 2003 ----
TDF(author = "Post", element = "both")
#  3.4 +- 0.98 sd
# 0.39 +- 1.3 sd
TDF(author = "McCutchan", element = "both", type = "muscle")
#tissue d15N: 15 values with mean 2.9 +- 0.32 se
#Muscle tissue d13C: 18 values with mean 1.3 +- 0.3 se
?TDF


#Based on Caut et al 2013 ----
#Diet dependent TDFs for fish, all tissue
diet_d13C <- #(fill in from data)
  deltaC <- -0.213 * (diet_d13C) - 2.848

diet_d15N <- #(fill in from data) 
  deltaN <- -0.261 * (diet_d15N) + 4.985


