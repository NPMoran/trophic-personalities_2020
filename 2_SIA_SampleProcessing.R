# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG= 'en')
library(dplyr); library(lme4); library(rptR)



#2. SIA_Sample processing data ---- 

### 2.1. Estimating replicates needed to estimate variance components in individual isotopic data ----

#Using 2014 data from Chlamydogobius eremius 2014, to estimate varance components 
#3 Populations:
#Cowa = high among individual variation
#Alge = moderate among individual variance
#Warr = low among individual variance
##n_ind = 20 (per pop), replicates = 3 (per ind)

#Case 1: high among-individual variation
Var_testdat <- read.csv("~/trophicpersonalities_GULD/2_SIA_SampleProcessing/Var_testdat2.csv")
Var_testdat_high <- subset(Var_testdat, PopID == "Cowa")
hist(Var_testdat_high$d15N)
hist(Var_testdat_high$d13C)

Var_testdat_high.Nmod = lmer(d15N ~ (1|IndID), data=Var_testdat_high)
Var_testdat_high.Nmod
confint(Var_testdat_high.Nmod)
rpt(d15N ~ (1 | IndID), grname = "IndID", data = Var_testdat_high, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

Var_testdat_high.Cmod = lmer(d13C ~ (1|IndID), data=Var_testdat_high)
Var_testdat_high.Cmod
confint(Var_testdat_high.Cmod)
rpt(d13C ~ (1 | IndID), grname = "IndID", data = Var_testdat_high, datatype = "Gaussian", 
    nboot = 100, npermut = 0)


#Case 2: medium among-individual variation
Var_testdat_medi <- subset(Var_testdat, PopID == "Alge")
hist(Var_testdat_medi$d15N)
hist(Var_testdat_medi$d13C)

Var_testdat_medi.Nmod = lmer(d15N ~ (1|IndID), data=Var_testdat_medi)
Var_testdat_medi.Nmod
confint(Var_testdat_medi.Nmod)
rpt(d15N ~ (1 | IndID), grname = "IndID", data = Var_testdat_medi, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

Var_testdat_medi.Cmod = lmer(d13C ~ (1|IndID), data=Var_testdat_medi)
summary(Var_testdat_medi.Cmod)
confint(Var_testdat_medi.Cmod)
rpt(d13C ~ (1 | IndID), grname = "IndID", data = Var_testdat_medi, datatype = "Gaussian", 
    nboot = 0, npermut = 0)


#Case 3: low among-individual variation
Var_testdat_loww <- subset(Var_testdat, PopID == "Alge")
hist(Var_testdat_loww$d15N)
hist(Var_testdat_loww$d13C)

Var_testdat_loww.Nmod = lmer(d15N ~ (1|IndID), data=Var_testdat_loww)
Var_testdat_loww.Nmod
confint(Var_testdat_loww.Nmod)
rpt(d15N ~ (1 | IndID), grname = "IndID", data = Var_testdat_loww, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

Var_testdat_loww.Cmod = lmer(d13C ~ (1|IndID), data=Var_testdat_loww)
summary(Var_testdat_loww.Cmod)
confint(Var_testdat_loww.Cmod)
rpt(d13C ~ (1 | IndID), grname = "IndID", data = Var_testdat_loww, datatype = "Gaussian", 
    nboot = 100, npermut = 0)

#estimates for among-individual and residual variance are robust and non-zero in all cases, 
#sufficient to estimate repeatability for this isotope data with high confidence 


### 2.2. First SIA analysis batch prep ----
#Aims: to estimate among and within-individual isotopic variance in the Goldborgsund goby population
#Approach: Provide 1x replicate for all individuals, and 3x for 20 individuals (randomly selected)


#Building data frame for fish fin processing
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

GULD_SIAfish$DateRemoved <- ""
GULD_SIAfish$DateCleaned <- ""
GULD_SIAfish$DateDried <- ""
GULD_SIAfish$Notes <- ""
write.csv(GULD_SIAfish, "~/trophicpersonalities_GULD/2_SIA_SampleProcessing/GULD_SIAfishproc.csv")


#Building data frame for SIA analysis batch 1

#to include repA for each individual
GULD_SIAfish.repA <- subset(GULD_SIAfish, rep == "A")

#to include additional replicates from 20 individuals
GULD_SIAfish.repBC <- subset(GULD_SIAfish, rep != "A")

#excluding 6 individuals with limited biomass samples
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G01'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G03'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G05'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G16'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G47'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G50'))

#excluding 8 individuals with no behavioural data
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G16'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G03'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G50'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G01'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G15'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G33'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G37'))
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID != c('G43'))
nrow(GULD_SIAfish.repBC)

#randomly selecting 20 of the remaining individuals to have replicated isoptope samples
set.seed(120)
GULD_SIAfish.repBC.B <- subset(GULD_SIAfish.repBC, rep == "B")
GULD_SIAfish.repBC.B <- sample_n(GULD_SIAfish.repBC.B, 20, replace = FALSE, weight = NULL)
GULD_SIAfish.repBC <- subset(GULD_SIAfish.repBC, fishID %in% GULD_SIAfish.repBC.B$fishID)

#adding it all into a dataframe
GULD_SIAbatch1 <- rbind(GULD_SIAfish.repA, GULD_SIAfish.repBC)
GULD_SIAbatch1 <- GULD_SIAbatch1[order(GULD_SIAbatch1$fishID),]
GULD_SIAbatch1 <- select(GULD_SIAbatch1, -c(row.id, DateRemoved, DateCleaned, DateDried, Notes))
GULD_SIAbatch1$date_packed <- ""
GULD_SIAbatch1$weight_packed <- ""
GULD_SIAbatch1$rack <- ""
GULD_SIAbatch1$row <- ""
GULD_SIAbatch1$position <- ""
GULD_SIAbatch1$δ15N <- ""
GULD_SIAbatch1$δC13 <- ""
GULD_SIAbatch1$N_percentage <- ""
GULD_SIAbatch1$C_percentage <- ""
write.csv(GULD_SIAbatch1, "~/trophicpersonalities_GULD/2_SIA_SampleProcessing/GULD_SIAbatch1.csv")



