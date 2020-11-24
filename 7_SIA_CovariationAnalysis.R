# Experiment: Guldborgsund Pilot 2020
#
# Author: Nicholas Moran (Centre for Ocean Life- DTU Aqua, Technical University of Denmark)
#
# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER


Sys.setenv(LANG = "en")


#4. Compiling individual behavioural and isotope datasets

#Behavioural Datasets
GULD_EXPL.proc <- read.csv("~/trophicpersonalities_GULD/4_Behaviour_Analysis/GULD_EXPL.processed.csv")
#variables of interest: endpointlat.bin.B, emergelat.bin.B

GULD_ACT.proc <- read.csv("~/trophicpersonalities_GULD/4_Behaviour_Analysis/GULD_ACT.processed.csv")
#variables of interest: avespeed_tot, aveacceler, propmoving.exp, dist, timefrozen_tot.ln, centretime.lnplus1


