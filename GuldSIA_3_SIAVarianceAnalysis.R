# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#### Guldborgsund SIA 3. SIA Variance analysis #### 



Sys.setenv(LANG = "en")

#Loading required packages- 
library(dplyr); library(lme4); library(rptR); library(data.table)
library(ggplot2); library(ggpubr); library(RColorBrewer)


## SIA.3.1. Round goby isotope distribution and variation  ----

GULD_SIA1 <- read.csv("~/trophicpersonalities_A/Data_GuldborgsundSIA/GULD_SIAbatch1dat.csv")
labels(GULD_SIA1)
GULD_SIA1 <- rename(GULD_SIA1, d15N = d15N....vs.AIR)
GULD_SIA1 <- rename(GULD_SIA1, d13C = d13C....vs.VPDB)
GULD_SIA1 <- rename(GULD_SIA1, N_percent = X..N)
GULD_SIA1 <- rename(GULD_SIA1, C_percent = X..C)
GULD_SIA1 <- select(GULD_SIA1, -c(run.ID, run.line, row, position, date_packed, weight_packed..mg.))

#Looking at distributions
ggplot(GULD_SIA1) + aes(x = d15N) + geom_histogram(color="black", fill="lightblue", binwidth = 0.3) + simpletheme 
ggqqplot(GULD_SIA1$d15N) #approximately normal

ggplot(GULD_SIA1) + aes(x = d13C) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(GULD_SIA1$d13C) #approximately normal, with some particularly low values

ggplot(GULD_SIA1) + aes(x = N_percent) + geom_histogram(color="black", fill="lightblue", binwidth = 0.008) + simpletheme 
ggqqplot(GULD_SIA1$N_percent) #approximately normal

ggplot(GULD_SIA1) + aes(x = C_percent) + geom_histogram(color="black", fill="lightblue", binwidth = 0.02) + simpletheme 
ggqqplot(GULD_SIA1$C_percent) #approximately normal

#Checking C:N ratios
# ratios above 4 require lipid correction (Post et al. 2007). All values here below 4.
GULD_SIA1$CN_ratio <- (GULD_SIA1$C_percent/GULD_SIA1$N_percent)
summary(GULD_SIA1$CN_ratio) #All values here below 4

#Adding individual fish IDs to dataframe
GULD_SIAfish <- NULL
GULD_SIAfish1 <- GULD_SIAfish
GULD_SIAfish10<- GULD_SIAfish
GULD_SIAfish1$row.id <- c(1:9)
GULD_SIAfish1 <- as.data.frame(GULD_SIAfish1)
GULD_SIAfish10$row.id <- c(10:55)
GULD_SIAfish10 <- as.data.frame(GULD_SIAfish10)
GULD_SIAfish1$FishID <- paste("G", GULD_SIAfish1$row.id, sep = "0")
GULD_SIAfish10$FishID <- paste("G", GULD_SIAfish10$row.id, sep = "")
GULD_SIAfish <- as.data.frame(rbind(GULD_SIAfish1, GULD_SIAfish10))

GULD_SIAfishA <- GULD_SIAfish; GULD_SIAfishA$rep <- "A"
GULD_SIAfishB <- GULD_SIAfish; GULD_SIAfishB$rep <- "B"
GULD_SIAfishC <- GULD_SIAfish; GULD_SIAfishC$rep <- "C"
GULD_SIAfish<- as.data.frame(rbind(GULD_SIAfishA, GULD_SIAfishB, GULD_SIAfishC))
GULD_SIAfish <- GULD_SIAfish[order(GULD_SIAfish$row.id),]
GULD_SIAfish$sampleID <- paste(GULD_SIAfish$FishID, GULD_SIAfish$rep, sep = "")

GULD_SIA1 <- rename(GULD_SIA1, sampleID = sample.ID)
GULD_SIA1 <- merge(GULD_SIA1, GULD_SIAfish, all.x = TRUE)
GULD_SIA1 <- select(GULD_SIA1, -c(row.id))


#Variance estimates
GULD_SIA1.N.mod <- lmer(d15N ~ (1|FishID), data=GULD_SIA1)
GULD_SIA1.N.mod
confint(GULD_SIA1.N.mod)
GULD_SIA1.N.VarCorr <- VarCorr(GULD_SIA1.N.mod, sigma = 1)
print(GULD_SIA1.N.VarCorr,comp=c("Variance"))
plot(GULD_SIA1.N.mod) 
GULD_SIA1.N.rpt <- rpt(d15N ~ (1 | FishID), grname = "FishID", data = GULD_SIA1, datatype = "Gaussian", 
    nboot = 100, npermut = 0)
GULD_SIA1.N.rpt

GULD_SIA1.C.mod <- lmer(d13C ~ (1|FishID), data=GULD_SIA1)
summary(GULD_SIA1.C.mod)
confint(GULD_SIA1.C.mod)
GULD_SIA1.C.VarCorr <- VarCorr(GULD_SIA1.C.mod, sigma = 1)
print(GULD_SIA1.C.VarCorr,comp=c("Variance"))
plot(GULD_SIA1.Cmod) 
GULD_SIA1.C.rpt <- rpt(d13C ~ (1 | FishID), grname = "fishID", data = GULD_SIA1, datatype = "Gaussian", 
    nboot = 100, npermut = 0)
GULD_SIA1.C.rpt

save(GULD_SIA1.N.mod, file = "./Output_GuldborgsundSIA/GULD_SIA1.N.mod.RData")
save(GULD_SIA1.C.mod, file = "./Output_GuldborgsundSIA/GULD_SIA1.C.mod.RData")

save(GULD_SIA1.N.rpt, file = "./Output_GuldborgsundSIA/GULD_SIA1.N.rpt.RData")
save(GULD_SIA1.C.rpt, file = "./Output_GuldborgsundSIA/GULD_SIA1.C.rpt.RData")

#Summary:
#d15N:      _ repeatability 0.845 [0.743, 0.897]
#d13C:      _ repeatability 0.979 [0.963, 0.989]

write.csv(GULD_SIA1, "~/trophicpersonalities_A/Data_GuldborgsundSIA/GULD_processed.fish.csv")



## SIA.3.2. Invertebrate data processing ----
GULD_SIA2 <- read.csv("~/trophicpersonalities_A/Data_GuldborgsundSIA/GULD_SIAbatch2dat.csv")
labels(GULD_SIA2)
GULD_SIA2 <- rename(GULD_SIA2, d15N = d15N....vs.AIR)
GULD_SIA2 <- rename(GULD_SIA2, d13C = d13C....vs.VPDB)
GULD_SIA2 <- rename(GULD_SIA2, N_percent = X..N)
GULD_SIA2 <- rename(GULD_SIA2, C_percent = X..C)
GULD_SIA2 <- rename(GULD_SIA2, sampleID = sample.ID)
GULD_SIA2 <- select(GULD_SIA2, -c(run.ID, run.line, row, position, date_packed, weight_packed..mg., notes))
GULD_SIA2$CN_ratio <- (GULD_SIA2$C_percent/GULD_SIA2$N_percent) #not planning to use this, but for interest

#Adding in taxanomic data
GULD_SIA2taxa <- read.csv("~/trophicpersonalities_A/Data_GuldborgsundSIA/GULD_SIA_taxagroups.csv")
labels(GULD_SIA2taxa)
GULD_SIA2taxa <- select(GULD_SIA2taxa, -c(ID.Notes, Key.Ref))
GULD_SIA2taxa
GULD_SIA2 <- merge(GULD_SIA2, GULD_SIA2taxa, by = 'taxaID', all.x = FALSE)

write.csv(GULD_SIA2, "~/trophicpersonalities_A/Data_GuldborgsundSIA/GULD_processed.prey.csv")



## SIA.3.3. Visualising SIA values ----
#Round Goby data
GULD_SIA.plot.goby <- setDT(GULD_SIA1)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                           d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                   by = .(FishID)]
range(GULD_SIA.plot.goby$d15N_mean) #N range 9.7 - 13.5
range(GULD_SIA.plot.goby$d13C_mean) #C range -22.1 - -13.4
GULD_SIA.plot.goby$label <- 'Round goby'


#Prey taxa data plotting by class
GULD_SIA2.Bival <- subset(GULD_SIA2, GULD_SIA2$Class == 'Bivalvia')
GULD_SIA2.Bival.plot <- setDT(GULD_SIA2.Bival)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                           d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                    by = .(taxaID)]
GULD_SIA2.Bival.plot$label <- 'Bivalvia'

GULD_SIA2.Gastr <- subset(GULD_SIA2, GULD_SIA2$Class == 'Gastropoda')
GULD_SIA2.Gastr.plot <- setDT(GULD_SIA2.Gastr)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIA2.Gastr.plot$label <- 'Gastropoda'

GULD_SIA2.Malac <- subset(GULD_SIA2, GULD_SIA2$Class == 'Malacostraca')
GULD_SIA2.Malac.plot <- setDT(GULD_SIA2.Malac)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIA2.Malac.plot$label <- 'Malacostraca'

GULD_SIA2.Ostra <- subset(GULD_SIA2, GULD_SIA2$Class == 'Ostracoda')
GULD_SIA2.Ostra.plot <- setDT(GULD_SIA2.Ostra)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIA2.Ostra.plot$label <- 'Ostracoda'

GULD_SIA2.Insec <- subset(GULD_SIA2, GULD_SIA2$Class == 'Insecta')
GULD_SIA2.Insec.plot <- setDT(GULD_SIA2.Insec)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIA2.Insec.plot$label <- 'Insecta'

GULD_SIA2.Polyc <- subset(GULD_SIA2, GULD_SIA2$Class == 'Polychaeta')
GULD_SIA2.Polyc.plot <- setDT(GULD_SIA2.Polyc)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIA2.Polyc.plot$label <- 'Polychaeta'

GULD_SIA2.Actin <- subset(GULD_SIA2, GULD_SIA2$Class == 'Actinopterygii')
GULD_SIA2.Actin.plot <- setDT(GULD_SIA2.Actin)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                                       d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                                                by = .(taxaID)]
GULD_SIA2.Actin.plot$label <- 'Actinopterygii'


#Building full dataframe
GULD_SIA.plot.taxa <- rbind(GULD_SIA2.Bival.plot, GULD_SIA2.Gastr.plot, GULD_SIA2.Malac.plot,
                       GULD_SIA2.Ostra.plot, GULD_SIA2.Insec.plot, GULD_SIA2.Polyc.plot,
                       GULD_SIA2.Actin.plot)


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
                    panel.grid.major = element_line(colour = "black", size = 0.5, linetype = "dashed" ),
                    axis.title.y  = element_text(size=13, vjust = 0.1),
                    axis.title.x  = element_text(size=13, vjust = 0.1),
                    panel.border = element_rect(colour = "black", fill=NA, size = 1),
                    legend.box.background = element_rect(color="white"))


#Full plot
GULD_SIA.fullplot <- ggplot(GULD_SIA.plot.taxa, aes (x = d13C_mean, y = d15N_mean)) + 
  SIAtheme +
  geom_point(data = GULD_SIA.plot.goby, col = 'orangered2', shape = 16, size = 3.5, alpha = 0.7) + 
#  geom_segment(data = GULD_SIA.plot.goby, aes(x = d13C_mean, y = ylower, xend = d13C_mean, yend = yupper), size = 0.3, colour = "black") + 
#  geom_segment(data = GULD_SIA.plot.goby, aes(x = xlower, y = d15N_mean, xend = xupper, yend = d15N_mean), size = 0.3, colour = "black") + 
  geom_point(data = GULD_SIA.plot.taxa, aes(col = label), shape = 18, size = 4, alpha = 1) + 
  geom_segment(data = GULD_SIA.plot.taxa, aes(x = d13C_mean, y = ylower, xend = d13C_mean, yend = yupper), size = 0.3, colour = "black") + 
  geom_segment(data = GULD_SIA.plot.taxa, aes(x = xlower, y = d15N_mean, xend = xupper, yend = d15N_mean), size = 0.3, colour = "black") +
  scale_x_continuous(limits = c(-25, 0), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,15), expand = c(0, 0)) +
  labs(colour = "Taxa") +
  ylab(expression(paste(delta^{15}, "N (\u2030)"))) +
  xlab(expression(paste(delta^{13}, "C (\u2030)"))) + 
  scale_color_brewer(palette = "Set1")
GULD_SIA.fullplot


ggsave("~/trophicpersonalities_A/Output_GuldborgsundSIA/GULD_SIA.fullplot1.jpg", width = 20, height = 10, units = "cm", GULD_SIA.fullplot, dpi = 600)



