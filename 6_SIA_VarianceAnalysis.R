# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG = "en")
library(dplyr)
library(lme4)
library(rptR)
library(data.table)
library(ggplot2)


#Round goby isotope distribution and variation ----
GULD_SIA1 <- read.csv("~/trophicpersonalities_GULD/5_SIA_Analysis/SIA_batch1.csv")
labels(GULD_SIA1)
GULD_SIA1 <- rename(GULD_SIA1, d15N = d15N....vs.AIR)
GULD_SIA1 <- rename(GULD_SIA1, d13C = d13C....vs.VPDB)
GULD_SIA1 <- rename(GULD_SIA1, N_percent = X..N)
GULD_SIA1 <- rename(GULD_SIA1, C_percent = X..C)

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
GULD_SIAfish1$fishID <- paste("G", GULD_SIAfish1$row.id, sep = "0")
GULD_SIAfish10$fishID <- paste("G", GULD_SIAfish10$row.id, sep = "")
GULD_SIAfish <- as.data.frame(rbind(GULD_SIAfish1, GULD_SIAfish10))

GULD_SIAfishA <- GULD_SIAfish; GULD_SIAfishA$rep <- "A"
GULD_SIAfishB <- GULD_SIAfish; GULD_SIAfishB$rep <- "B"
GULD_SIAfishC <- GULD_SIAfish; GULD_SIAfishC$rep <- "C"
GULD_SIAfish<- as.data.frame(rbind(GULD_SIAfishA, GULD_SIAfishB, GULD_SIAfishC))
GULD_SIAfish <- GULD_SIAfish[order(GULD_SIAfish$row.id),]
GULD_SIAfish$sampleID <- paste(GULD_SIAfish$fishID, GULD_SIAfish$rep, sep = "")

GULD_SIA1 <- rename(GULD_SIA1, sampleID = sample.ID)
GULD_SIA1 <- merge(GULD_SIA1, GULD_SIAfish, all.x = TRUE)


#Variance estimates
GULD_SIA1.N.mod <- lmer(d15N ~ (1|fishID), data=GULD_SIA1)
GULD_SIA1.N.mod
confint(GULD_SIA1.N.mod)
GULD_SIA1.N.VarCorr <- VarCorr(GULD_SIA1.N.mod, sigma = 1)
print(GULD_SIA1.N.VarCorr,comp=c("Variance"))
plot(GULD_SIA1.N.mod) 
GULD_SIA1.N.rpt <- rpt(d15N ~ (1 | fishID), grname = "fishID", data = GULD_SIA1, datatype = "Gaussian", 
    nboot = 100, npermut = 0)
GULD_SIA1.N.rpt

GULD_SIA1.C.mod <- lmer(d13C ~ (1|fishID), data=GULD_SIA1)
GULD_SIA1.C.mod
confint(GULD_SIA1.C.mod)
GULD_SIA1.C.VarCorr <- VarCorr(GULD_SIA1.C.mod, sigma = 1)
print(GULD_SIA1.C.VarCorr,comp=c("Variance"))
plot(GULD_SIA1.Cmod) 
GULD_SIA1.C.rpt <- rpt(d13C ~ (1 | fishID), grname = "fishID", data = GULD_SIA1, datatype = "Gaussian", 
    nboot = 100, npermut = 0)
GULD_SIA1.C.rpt


#Summary:
#d15N:      _ repeatability 0.954 [0.948, 0.998] 
#d15N:      _ repeatability 0.979 [0.963, 0.987] 



#Visualising distibution of SIA values ----
GULD_SIA1.plot <- setDT(GULD_SIA1)[ , list(d15N_mean = mean(d15N), d15N_sd = sd(d15N),
                                           d13C_mean = mean(d13C), d13C_sd = sd(d13C)), 
                   by = .(fishID)]
range(GULD_SIA1.plot$d15N_mean) #N range 9.7 - 13.5
range(GULD_SIA1.plot$d13C_mean) #C range -22.1 - -13.4


#Setting values for manual errorbar plotting
GULD_SIA1.plot$ylower <- GULD_SIA1.plot$d15N_mean - GULD_SIA1.plot$d15N_sd
GULD_SIA1.plot$yupper <- GULD_SIA1.plot$d15N_mean + GULD_SIA1.plot$d15N_sd
GULD_SIA1.plot$xlower <- GULD_SIA1.plot$d13C_mean - GULD_SIA1.plot$d13C_sd
GULD_SIA1.plot$xupper <- GULD_SIA1.plot$d13C_mean + GULD_SIA1.plot$d13C_sd


GULD_SIA1.plot1 <- ggplot(GULD_SIA1.plot, aes(x = d13C_mean, y = d15N_mean)) +
  geom_point(size = 2, shape = 21, fill = "lightblue") + 
  theme(axis.text.y = element_text(size = 8.5, colour = "black"), 
        axis.text.x = element_text(size = 8.5, colour = "black"), 
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(colour = "black", size = 0.5, linetype = "dashed" ),
        axis.title.y  = element_text(size=13, vjust = 0.1),
        axis.title.x  = element_text(size=13, vjust = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size = 1)) +
  geom_segment(mapping = aes(x = d13C_mean, y = ylower, xend = d13C_mean, yend = yupper), size = 0.3, colour = "black") + 
  geom_segment(mapping = aes(x = xlower, y = d15N_mean, xend = xupper, yend = d15N_mean), size = 0.3, colour = "black") + 
  scale_x_continuous(limits = c(-25, 0), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,15), expand = c(0, 0)) +
  labs(x = "d13C",
       y = "d15N") 
GULD_SIA1.plot1

ggsave("~/trophicpersonalities_GULD/4_SIA_Analysis/GULD_SIA1.plot1.jpg", width = 20, height = 12, units = "cm", GULD_SIA1.plot1, dpi = 600)


#Exploratory correlation tests
cor.test(GULD_SIA1.plot$d15N_mean, GULD_SIA1.plot$d13C_mean, method = "spearman")
cor.test(GULD_SIA1.plot$d15N_mean, GULD_SIA1.plot$d13C_mean, method = "pearson")
#N and C are strongly correlated


