# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#### Guldborgsund SIA 1. Preliminary prey analysis #### 

Sys.setenv(LANG = "en")


## SIA.1.1. Identifying potential prey taxa from Novana database ----

# Using species occurence databases for Guldborgsund, to produce a list of potential species to inform species IDs
# records retreived from Novana https://odaforalle.au.dk/
#Extraction details: Hent data -> Hav -> Bundafauna -> Artsliste
#Dates: 1/1/2000 - 25/9/2020 (inclusive of extraction date)
#Vandomraade: Guldborgsund
#Observationsstednr: Vaelg alle
#Bundfaunaraekke: Vaelg alle
#Bundfaunaart: Vaelg alle
#Prøvetagningsudstyr: Vaelg alle
Novanadat <- read.csv("~/trophicpersonalities_A/Data_GuldborgsundSIA/HentData_UYGUPNWUIZ.csv", sep = ";")
head(Novanadat)

#Frequency of species by entries
Novanadat_specfreq <- as.data.frame(table(Novanadat$Artsnavn))
write.csv(Novanadat_specfreq, "~/trophicpersonalities_A/Data_GuldborgsundSIA/HentData_UYGUPNWUIZ_specfreq.csv")


##Positional data format editing (not currently required)
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


## SIA.1.2. Compiling prey data ----
# Taxonomic groups, identified to at least family where possible.







