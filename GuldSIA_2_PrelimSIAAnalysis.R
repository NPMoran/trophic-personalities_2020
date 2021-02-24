# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#### Guldborgsund SIA 2. Preliminary SIA analysis #### 


Sys.setenv(LANG = "en")

#Loading required packages- 
library(dplyr)



## SIA.2.1. SIA analysis of individual fin clip samples  ----
#Aims: to estimate among and within-individual isotopic variance in the Goldborgsund goby population
#Approach: Provide 1x replicate for all individuals, and 3x for 20 individuals (randomly selected) in batch 1. Analyse the remaining samples if there is evidence of among individual variation 


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
write.csv(GULD_SIAfish, "~/trophicpersonalities_A/Data_GuldborgsundSIA/GULD_SIAfishproc.csv")


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
write.csv(GULD_SIAbatch1, "~/trophicpersonalities_A/Data_GuldborgsundSIA/GULD_SIAbatch1.csv")


#Building data frame for SIA analysis batch 3
#running all remaining fin clip samples

GULD_SIAbatch3 <- subset(GULD_SIAfish, sampleID %!in% GULD_SIAbatch1$sampleID)
nrow(GULD_SIAbatch3)
GULD_SIAbatch3 <- select(GULD_SIAbatch3, -c(row.id, DateRemoved, DateCleaned, DateDried, Notes))
GULD_SIAbatch3$date_packed <- ""
GULD_SIAbatch3$weight_packed <- ""
GULD_SIAbatch3$rack <- ""
GULD_SIAbatch3$row <- ""
GULD_SIAbatch3$position <- ""
GULD_SIAbatch3$δ15N <- ""
GULD_SIAbatch3$δC13 <- ""
GULD_SIAbatch3$N_percentage <- ""
GULD_SIAbatch3$C_percentage <- ""
write.csv(GULD_SIAbatch3, "~/trophicpersonalities_A/Data_GuldborgsundSIA/GULD_SIAbatch3.csv")


#checking all samples are accounted for
GULD_SIAbatchcheck <- rbind(GULD_SIAbatch1, GULD_SIAbatch3)
nrow(GULD_SIAbatchcheck)
n_distinct(GULD_SIAbatchcheck$sampleID)
n_distinct(GULD_SIAbatchcheck$fishID)
n_distinct(GULD_SIAbatchcheck$rep)
setdiff(GULD_SIAbatchcheck$sampleID, GULD_SIAfish$sampleID)
intersect(GULD_SIAbatchcheck$sampleID, GULD_SIAfish$sampleID)



