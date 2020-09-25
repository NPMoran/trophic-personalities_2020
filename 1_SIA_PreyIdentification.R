# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG= 'en'); library(dplyr)



#1. Potential prey taxa ----

###Using species occurence databases for Guldborgsund, to produce a list of potential species to inform species IDs

###1.1. Invertebrate species records retreived from Novana https://odaforalle.au.dk/ ----
#Extraction details: Hent data -> Hav -> Bundafauna -> Artsliste
#Dates: 1/1/2020 - 25/9/2020 (also extraction date)
#Vandomraade: Guldborgsund
#Observationsstednr: Vaelg alle
#Bundfaunaraekke: Vaelg alle
#Bundfaunaart: Vaelg alle
#Prøvetagningsudstyr: Vaelg alle
Novanadat <- read.csv("~/trophicpersonalities_GULD/1_SIA_PreyIdentification/HentData_UYGUPNWUIZ.csv", sep = ";")
head(Novanadat)

#Frequency of species by entries
Novanadat_specfreq <- as.data.frame(table(Novanadat$Artsnavn))
write.csv(Novanadat_specfreq, "~/trophicpersonalities_GULD/1_SIA_PreyIdentification/HentData_UYGUPNWUIZ_specfreq.csv")

##Positional data format editing
## Change positional data from integer to character
#Novanadat$ObsSted_bredde <- as.character(Novanadat$ObsSted_bredde)
#Novanadat$ObsSted_længde <- as.character(Novanadat$ObsSted_længde)
#
## Splitting to degree, arc, minute, decimal
#Novanadat$ObsSted_bredde_degree <- as.numeric(as.character(substr(Novanadat$ObsSted_bredde, 1,2)))
#Novanadat$ObsSted_længde_degree <- as.numeric(as.character(substr(Novanadat$ObsSted_længde, 1,2)))
#Novanadat$ObsSted_bredde_minute <- as.numeric(as.character(substr(Novanadat$ObsSted_bredde, 3,7)))
#Novanadat$ObsSted_længde_minute <- as.numeric(as.character(substr(Novanadat$ObsSted_længde, 3,7)))
#
## Degrees minutes second to decimal degrees: DD = d + (min/60) + (sec/3600)
## Degrees miutes decimal to decimal degrees: DD = d + (min+decimal/60)   dvs:
## Degrees Minutes.m to Decimal Degrees .d = M.m / 60
##Decimal Degrees = Degrees + .d
#Novanadat$br_decimal <- Novanadat$ObsSted_bredde_grad + (Novanadat$ObsSted_bredde_minut/60)
#Novanadat$lg_decimal <- Novanadat$ObsSted_længde_grad + (Novanadat$ObsSted_længde_minut/60)
#
#
#unique(Novanadat$br_decimal)
#unique(Novanadat$lg_decimal)



##1.2. Invetebrate records retreived from OBIS (https://obis.org/) ----
##Accessed via: Historical data on invertebrates from the Baltic Sea and Gdansk Bay: https://obis.org/dataset/5fbbc3e4-e951-4aa2-a16c-ac9258933d6c
##All records retreived from approximately 50km radius around GULD site (so less specific than Novana search)
#OBISdat <- read.csv('~/trophicpersonalities_GULD/1_SIA_PreyIdentification/OBISdat.csv', strip.white = TRUE)
#summary(OBISdat$scientificName)
#n_distinct(OBISdat$scientificName) #998 distinct taxa names, likely some overlap
#
##assessing the most commonly recorded species in the area
#OBISdat_freq <- as.data.frame(table(OBISdat$scientificName))
#nrow(subset(OBISdat_freq, Freq >= 1000))
#write.csv(subset(OBISdat_freq, Freq >= 1000), '~/trophicpersonalities_GULD/1_SIA_PreyIdentification/OBISdat_freq1000.csv') 
##found many were fish, so re-running with fish excluded
#
#OBISdat_inverts <- subset(OBISdat, class != 'Actinopterygii') #to exclude fish
#OBISdat_inverts_freq <- as.data.frame(table(OBISdat_inverts$scientificName))
#nrow(subset(OBISdat_inverts_freq, Freq >= 100))
#write.csv(subset(OBISdat_inverts_freq, Freq >= 100), '~/trophicpersonalities_GULD/1_SIA_PreyIdentification/OBISdat_freq100.csv') 



###1.3. Prex taxa exploratory analysis ----








