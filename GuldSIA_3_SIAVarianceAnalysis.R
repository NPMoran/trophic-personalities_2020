# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#### Guldborgsund SIA 3. SIA Variance analysis #### 


Sys.setenv(LANG = "en")

#Loading required packages- 
library(dplyr); library(lme4); library(lmerTest); library(rptR); library(data.table)
library(ggplot2); library(ggpubr); library(RColorBrewer)

#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"),axis.text.x = element_text(size = 10, colour = "black"), panel.background = element_rect(fill = "white"),axis.title.y  = element_text(size=12, vjust = 2),axis.title.x  = element_text(size=12, vjust = 0.1),panel.border = element_rect(colour = "black", fill=NA, size = 1))


#Importing and sorting datasets
GULD_SIA1 <- read.csv("~/trophic-personalities_2020/Data_GuldborgsundSIA/GULD_SIAbatch1dat.csv")
GULD_SIA2 <- read.csv("~/trophic-personalities_2020/Data_GuldborgsundSIA/GULD_SIAbatch2dat.csv")
GULD_SIA3 <- read.csv("~/trophic-personalities_2020/Data_GuldborgsundSIA/GULD_SIAbatch3dat.csv")
nrow(GULD_SIA1); nrow(GULD_SIA2); nrow(GULD_SIA3) 
labels(GULD_SIA1); labels(GULD_SIA2); labels(GULD_SIA3) 
GULD_SIAfull <- rbind(GULD_SIA1, GULD_SIA2, GULD_SIA3)
GULD_SIAfull <- subset(GULD_SIAfull, sortID != 'NA')
nrow(GULD_SIAfull); labels(GULD_SIAfull)


GULD_SIAfins <- subset(GULD_SIAfull, sortID == 'fin')
nrow(GULD_SIAfins) #round goby fish fins
GULD_SIAfins <- subset(GULD_SIAfins, sampleID != "G21C") #excluding row with NAs (sample failed in analyzer)
GULD_SIAfins <- subset(GULD_SIAfins, sampleID != "G39A") #excluding samples suspects of being mixed up in batch 1
GULD_SIAfins <- subset(GULD_SIAfins, sampleID != "G40A") #excluding samples suspects of being mixed up in batch 1
GULD_SIAfins <- rename(GULD_SIAfins, FishID = sourceID)


GULD_SIAprey <- subset(GULD_SIAfull, sortID == 'prey')
GULD_SIAfins <- subset(GULD_SIAfins, sampleID != "G21C") #excluding row with NAs (sample failed in analyzer)
nrow(GULD_SIAprey) #round goby potential prey items

GULD_SIAprod <- subset(GULD_SIAfull, sortID == 'prod')
nrow(GULD_SIAprod) #primary producers

GULD_SIAdisc <- subset(GULD_SIAfull, sortID == 'discrim')
nrow(GULD_SIAdisc) #additional samples to estimate discrimination factor


## SIA.3.1. Round goby isotope distribution and variation  ----

#Looking at distributions
ggplot(GULD_SIAfins) + aes(x = d15N) + geom_histogram(color="black", fill="lightblue", binwidth = 0.3) + simpletheme 
ggqqplot(GULD_SIAfins$d15N) #approximately normal, some potential outliers at the high end

ggplot(GULD_SIAfins) + aes(x = d13C) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(GULD_SIAfins$d13C) #approximately normal, some potential outliers at the low end

ggplot(GULD_SIAfins) + aes(x = N_percentage) + geom_histogram(color="black", fill="lightblue", binwidth = 0.008) + simpletheme 
ggqqplot(GULD_SIAfins$N_percentage) #approximately normal

ggplot(GULD_SIAfins) + aes(x = C_percentage) + geom_histogram(color="black", fill="lightblue", binwidth = 0.02) + simpletheme 
ggqqplot(GULD_SIAfins$C_percentage) #approximately normal


#Checking C:N ratios
# ratios above 4 require lipid correction (Post et al. 2007). All values here below 4.
GULD_SIAfins$CN_ratio <- (GULD_SIAfins$C_percentage/GULD_SIAfins$N_percentage)
summary(GULD_SIAfins$CN_ratio) #All values here below 4


##Adding individual fish IDs to dataframe
#GULD_SIAfish <- NULL
#GULD_SIAfish1 <- GULD_SIAfish
#GULD_SIAfish10<- GULD_SIAfish
#GULD_SIAfish1$row.id <- c(1:9)
#GULD_SIAfish1 <- as.data.frame(GULD_SIAfish1)
#GULD_SIAfish10$row.id <- c(10:55)
#GULD_SIAfish10 <- as.data.frame(GULD_SIAfish10)
#GULD_SIAfish1$FishID <- paste("G", GULD_SIAfish1$row.id, sep = "0")
#GULD_SIAfish10$FishID <- paste("G", GULD_SIAfish10$row.id, sep = "")
#GULD_SIAfish <- as.data.frame(rbind(GULD_SIAfish1, GULD_SIAfish10))
#
#GULD_SIAfishA <- GULD_SIAfish; GULD_SIAfishA$rep <- "A"
#GULD_SIAfishB <- GULD_SIAfish; GULD_SIAfishB$rep <- "B"
#GULD_SIAfishC <- GULD_SIAfish; GULD_SIAfishC$rep <- "C"
#GULD_SIAfish<- as.data.frame(rbind(GULD_SIAfishA, GULD_SIAfishB, GULD_SIAfishC))
#GULD_SIAfish <- GULD_SIAfish[order(GULD_SIAfish$row.id),]
#GULD_SIAfish$sampleID <- paste(GULD_SIAfish$FishID, GULD_SIAfish$rep, sep = "")
#
#GULD_SIA1 <- rename(GULD_SIA1, sampleID = sample.ID)
#GULD_SIA1 <- merge(GULD_SIA1, GULD_SIAfish, all.x = TRUE)
#GULD_SIA1 <- select(GULD_SIA1, -c(row.id))


#Variance estimates
GULD_SIAfins.N.mod <- lmer(d15N ~ (1|FishID), data=GULD_SIAfins)
GULD_SIAfins.N.mod
GULD_SIAfins.N.VarCorr <- VarCorr(GULD_SIAfins.N.mod, sigma = 1)
print(GULD_SIAfins.N.VarCorr,comp=c("Variance"))
plot(GULD_SIAfins.N.mod) 
GULD_SIAfins.N.rpt <- rpt(d15N ~ (1 | FishID), grname = "FishID", data = GULD_SIAfins, datatype = "Gaussian", 
    nboot = 100, npermut = 0)
GULD_SIAfins.N.rpt

GULD_SIAfins.C.mod <- lmer(d13C ~ (1|FishID), data=GULD_SIAfins)
summary(GULD_SIAfins.C.mod)
GULD_SIAfins.C.VarCorr <- VarCorr(GULD_SIAfins.C.mod, sigma = 1)
print(GULD_SIAfins.C.VarCorr,comp=c("Variance"))
plot(GULD_SIAfins.C.mod) 
GULD_SIAfins.C.rpt <- rpt(d13C ~ (1 | FishID), grname = "FishID", data = GULD_SIAfins, datatype = "Gaussian", 
    nboot = 100, npermut = 0)
GULD_SIAfins.C.rpt

save(GULD_SIAfins.N.mod, file = "./Output_GuldborgsundSIA/GULD_SIA1.N.mod.RData")
save(GULD_SIAfins.C.mod, file = "./Output_GuldborgsundSIA/GULD_SIA1.C.mod.RData")

save(GULD_SIAfins.N.rpt, file = "./Output_GuldborgsundSIA/GULD_SIA1.N.rpt.RData")
save(GULD_SIAfins.C.rpt, file = "./Output_GuldborgsundSIA/GULD_SIA1.C.rpt.RData")

#Summary:
#d15N:      _ repeatability 0.878 [0.815, 0.914]
#d13C:      _ repeatability 0.974 [0.959, 0.983]

write.csv(GULD_SIAfins, "~/trophic-personalities_2020/Data_GuldborgsundSIA/GULD_processed.fins.csv")



## SIA.3.2. Invertebrate data processing ----

#loading taxanomic data
GULD_SIAtaxa <- read.csv("~/trophic-personalities_2020/Data_GuldborgsundSIA/GULD_SIA_taxagroups.csv")
labels(GULD_SIAtaxa)
GULD_SIAtaxa <- select(GULD_SIAtaxa, -c(ID.Notes, Key.Ref))
GULD_SIAtaxa 
GULD_SIAprey <- rename(GULD_SIAprey, taxaID = sourceID)
GULD_SIAprey <- merge(GULD_SIAprey, GULD_SIAtaxa, by = 'taxaID', all.x = FALSE)

write.csv(GULD_SIAprey, "~/trophic-personalities_2020/Data_GuldborgsundSIA/GULD_processed.prey.csv")



## SIA.3.3. Visualising SIA values ----
#Round Goby data
GULD_SIA.plot.goby <- setDT(GULD_SIAfins)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                           d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                   by = .(FishID)]
range(GULD_SIA.plot.goby$d15N_mean) #N range 9.489771 - 13.529778
range(GULD_SIA.plot.goby$d13C_mean) #C range -22.11031 - -13.42035
GULD_SIA.plot.goby$label <- 'Round goby'

#Prey taxa data plotting by class
GULD_SIAprey.Bival <- subset(GULD_SIAprey, GULD_SIAprey$Class == 'Bivalvia')
GULD_SIAprey.Bival.plot <- setDT(GULD_SIAprey.Bival)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                           d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                    by = .(taxaID)]
GULD_SIAprey.Bival.plot$label <- 'Bivalvia'

GULD_SIAprey.Gastr <- subset(GULD_SIAprey, GULD_SIAprey$Class == 'Gastropoda')
GULD_SIAprey.Gastr.plot <- setDT(GULD_SIAprey.Gastr)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIAprey.Gastr.plot$label <- 'Gastropoda'

GULD_SIAprey.Malac <- subset(GULD_SIAprey, GULD_SIAprey$Class == 'Malacostraca')
GULD_SIAprey.Malac.plot <- setDT(GULD_SIAprey.Malac)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIAprey.Malac.plot$label <- 'Malacostraca'

GULD_SIAprey.Ostra <- subset(GULD_SIAprey, GULD_SIAprey$Class == 'Ostracoda')
GULD_SIAprey.Ostra.plot <- setDT(GULD_SIAprey.Ostra)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIAprey.Ostra.plot$label <- 'Ostracoda'

GULD_SIAprey.Insec <- subset(GULD_SIAprey, GULD_SIAprey$Class == 'Insecta')
GULD_SIAprey.Insec.plot <- setDT(GULD_SIAprey.Insec)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIAprey.Insec.plot$label <- 'Insecta'

GULD_SIAprey.Polyc <- subset(GULD_SIAprey, GULD_SIAprey$Class == 'Polychaeta')
GULD_SIAprey.Polyc.plot <- setDT(GULD_SIAprey.Polyc)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIAprey.Polyc.plot$label <- 'Polychaeta'

GULD_SIAprey.Actin <- subset(GULD_SIAprey, GULD_SIAprey$Class == 'Actinopterygii')
GULD_SIAprey.Actin.plot <- setDT(GULD_SIAprey.Actin)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIAprey.Actin.plot$label <- 'Actinopterygii'


#Building full dataframe
GULD_SIA.plot.taxa <- rbind(GULD_SIAprey.Bival.plot, GULD_SIAprey.Gastr.plot, GULD_SIAprey.Malac.plot,
                       GULD_SIAprey.Ostra.plot, GULD_SIAprey.Insec.plot, GULD_SIAprey.Polyc.plot,
                       GULD_SIAprey.Actin.plot)

#Adding primary sources
GULD_SIAprod <- subset(GULD_SIAprod, sampleID != "S06B") #excluding datapoint that is a suspected wrong ID
GULD_SIAprod.plot <- setDT(GULD_SIAprod)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                             d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                      by = .(sourceID)]
GULD_SIAprod.plot <- rename(GULD_SIAprod.plot, taxaID = sourceID)
GULD_SIAprod.plot$label <- GULD_SIAprod.plot$taxaID
GULD_SIAprod.plot$label[GULD_SIAprod.plot$label == "ALG"] <- "Algae"
GULD_SIAprod.plot$label[GULD_SIAprod.plot$label == "TERR"] <- "Riparian veg"
GULD_SIAprod.plot$label[GULD_SIAprod.plot$label == "PHYTO"] <- "Phytoplankton/FPOM"
GULD_SIAprod.plot$ylower <- GULD_SIAprod.plot$d15N_mean - GULD_SIAprod.plot$d15N_sd
GULD_SIAprod.plot$yupper <- GULD_SIAprod.plot$d15N_mean + GULD_SIAprod.plot$d15N_sd
GULD_SIAprod.plot$xlower <- GULD_SIAprod.plot$d13C_mean - GULD_SIAprod.plot$d13C_sd
GULD_SIAprod.plot$xupper <- GULD_SIAprod.plot$d13C_mean + GULD_SIAprod.plot$d13C_sd


#Setting values for manual errorbar plotting
GULD_SIA.plot.goby$ylower <- GULD_SIA.plot.goby$d15N_mean - GULD_SIA.plot.goby$d15N_sd
GULD_SIA.plot.goby$yupper <- GULD_SIA.plot.goby$d15N_mean + GULD_SIA.plot.goby$d15N_sd
GULD_SIA.plot.goby$xlower <- GULD_SIA.plot.goby$d13C_mean - GULD_SIA.plot.goby$d13C_sd
GULD_SIA.plot.goby$xupper <- GULD_SIA.plot.goby$d13C_mean + GULD_SIA.plot.goby$d13C_sd

GULD_SIA.plot.taxa$ylower <- GULD_SIA.plot.taxa$d15N_mean - GULD_SIA.plot.taxa$d15N_sd
GULD_SIA.plot.taxa$yupper <- GULD_SIA.plot.taxa$d15N_mean + GULD_SIA.plot.taxa$d15N_sd
GULD_SIA.plot.taxa$xlower <- GULD_SIA.plot.taxa$d13C_mean - GULD_SIA.plot.taxa$d13C_sd
GULD_SIA.plot.taxa$xupper <- GULD_SIA.plot.taxa$d13C_mean + GULD_SIA.plot.taxa$d13C_sd


#Ordering groups for plotting
GULD_SIA.plot.goby$label <- as.factor(GULD_SIA.plot.goby$label)
GULD_SIA.plot.taxa$label <- as.factor(GULD_SIA.plot.taxa$label)
#GULD_SIA.plot$label <- ordered(GULD_SIA.plot$label, levels = c("Round goby", "Ostracoda", "Insecta", "Polychaeta", "Actinopterygii", "Bivalvia", "Gastropoda", "Malacostraca"))


#General theme for SIA plots
SIAtheme <-   theme(axis.text.y = element_text(size = 8.5, colour = "black"), 
                    axis.text.x = element_text(size = 8.5, colour = "black"), 
                    panel.background = element_rect(fill = "white"),
                    panel.grid.major = element_line(colour = "white", size = 0.5, linetype = "dashed" ),
                    axis.title.y  = element_text(size=13, vjust = 0.1),
                    axis.title.x  = element_text(size=13, vjust = 0.1),
                    panel.border = element_rect(colour = "black", fill=NA, size = 1),
                    legend.box.background = element_rect(color="white"))


#Invert plot
GULD_SIA.fullplot <- ggplot(GULD_SIA.plot.taxa, aes (x = d13C_mean, y = d15N_mean)) + 
  SIAtheme +
  geom_point(data = GULD_SIA.plot.goby, col = 'orangered2', shape = 16, size = 3.5, alpha = 0.7) + 
  geom_segment(data = GULD_SIA.plot.goby, aes(x = d13C_mean, y = ylower, xend = d13C_mean, yend = yupper), size = 0.3, colour = "black") + 
  geom_segment(data = GULD_SIA.plot.goby, aes(x = xlower, y = d15N_mean, xend = xupper, yend = d15N_mean), size = 0.3, colour = "black") + 
  geom_point(data = GULD_SIA.plot.taxa, aes(col = label), shape = 18, size = 4, alpha = 1) + 
  geom_segment(data = GULD_SIA.plot.taxa, aes(x = d13C_mean, y = ylower, xend = d13C_mean, yend = yupper), size = 0.3, colour = "black") + 
  geom_segment(data = GULD_SIA.plot.taxa, aes(x = xlower, y = d15N_mean, xend = xupper, yend = d15N_mean), size = 0.3, colour = "black") +
  scale_x_continuous(limits = c(-30, -5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,15), expand = c(0, 0)) +
  labs(colour = "Taxa") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  scale_color_brewer(palette = "PuOr")
GULD_SIA.fullplot

ggsave("~/trophic-personalities_2020/Output_GuldborgsundSIA/GULD_SIA.fullplot1.jpg", width = 25, height = 13, units = "cm", GULD_SIA.fullplot, dpi = 600)

#Phytoplankton/alg plot
GULD_SIA.fullplot2 <- ggplot(GULD_SIAprod.plot, aes (x = d13C_mean, y = d15N_mean)) + 
  SIAtheme +
  geom_point(data = GULD_SIA.plot.goby, col = 'orangered2', shape = 16, size = 3.5, alpha = 0.7) + 
  geom_segment(data = GULD_SIA.plot.goby, aes(x = d13C_mean, y = ylower, xend = d13C_mean, yend = yupper), size = 0.3, colour = "black") + 
  geom_segment(data = GULD_SIA.plot.goby, aes(x = xlower, y = d15N_mean, xend = xupper, yend = d15N_mean), size = 0.3, colour = "black") + 
  geom_point(data = GULD_SIAprod.plot, aes(col = label), shape = 18, size = 4, alpha = 1) + 
  geom_segment(data = GULD_SIAprod.plot, aes(x = d13C_mean, y = ylower, xend = d13C_mean, yend = yupper), size = 0.3, colour = "black") + 
  geom_segment(data = GULD_SIAprod.plot, aes(x = xlower, y = d15N_mean, xend = xupper, yend = d15N_mean), size = 0.3, colour = "black") + 
  scale_x_continuous(limits = c(-30.2, -5), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-0.5,15), expand = c(0, 0)) +
  labs(colour = "Taxa") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  scale_color_brewer(palette = "Greens")
GULD_SIA.fullplot2


ggsave("~/trophic-personalities_2020/Output_GuldborgsundSIA/GULD_SIA.fullplot2.jpg", width = 25, height = 13, units = "cm", GULD_SIA.fullplot2, dpi = 600)




  



#Discrimination Factor visualisation
GULD_SIAdisc <- subset(GULD_SIAdisc, sampleID != "D_F14") #excluding sample lost during analysis
GULD_SIAdisc$rep[GULD_SIAdisc$rep == "S1"] <- "S"
GULD_SIAdisc$rep[GULD_SIAdisc$rep == "S2"] <- "S"
GULD_SIAdisc$rep[GULD_SIAdisc$rep == "S3"] <- "S"
GULD_SIAdisc$rep[GULD_SIAdisc$rep == "S4"] <- "S"
GULD_SIAdisc$rep[GULD_SIAdisc$rep == "S5"] <- "S"

GULD_SIAdisc.plot <- setDT(GULD_SIAdisc)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                 d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                          by = .(rep)]

GULD_SIAdisc.plot$rep[GULD_SIAdisc.plot$rep == "F"] <- "Fin tissue"
GULD_SIAdisc.plot$rep[GULD_SIAdisc.plot$rep == "M"] <- "Muscle tissue"
GULD_SIAdisc.plot$rep[GULD_SIAdisc.plot$rep == "S"] <- "Laboratory feed"
GULD_SIAdisc.plot$label <- ordered(GULD_SIAdisc.plot$label, levels = c("Fin tissue", "Muscle tissue", "Laboratory feed"))

GULD_SIAdisc.plot$ylower <- GULD_SIAdisc.plot$d15N_mean - GULD_SIAdisc.plot$d15N_sd
GULD_SIAdisc.plot$yupper <- GULD_SIAdisc.plot$d15N_mean + GULD_SIAdisc.plot$d15N_sd
GULD_SIAdisc.plot$xlower <- GULD_SIAdisc.plot$d13C_mean - GULD_SIAdisc.plot$d13C_sd
GULD_SIAdisc.plot$xupper <- GULD_SIAdisc.plot$d13C_mean + GULD_SIAdisc.plot$d13C_sd


GULD_SIA.discplot <- ggplot(GULD_SIAdisc.plot, aes (x = d13C_mean, y = d15N_mean)) + 
  SIAtheme +
  geom_point(data = GULD_SIAdisc.plot, aes(col = rep), shape = 18, size = 4, alpha = 1) + 
  geom_segment(data = GULD_SIAdisc.plot, aes(x = d13C_mean, y = ylower, xend = d13C_mean, yend = yupper), size = 0.3, colour = "black") + 
  geom_segment(data = GULD_SIAdisc.plot, aes(x = xlower, y = d15N_mean, xend = xupper, yend = d15N_mean), size = 0.3, colour = "black") +
  scale_x_continuous(limits = c(-30, -10), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,15), expand = c(0, 0)) +
  labs(colour = "Taxa") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  scale_color_brewer(palette = "Set1")
GULD_SIA.discplot

ggsave("~/trophic-personalities_2020/Output_GuldborgsundSIA/GULD_SIA.discriminationfactor.jpg", width = 15, height = 11, units = "cm", GULD_SIA.discplot, dpi = 600)

