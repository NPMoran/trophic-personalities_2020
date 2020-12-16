# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark
#         Dec 2020



Sys.setenv(LANG = "en")
library(dplyr); library(ggpubr); library(ggplot2); library(lme4); library(rptR); library(lmerTest); library(rptR)




KARR_ACT <- read.csv('~/trophicpersonalities_A/5_Karrebaek_PreliminaryAnalysis/KARR_ACT112020.csv')
head(KARR_ACT)
KARR_ACT2<- subset(KARR_ACT, TrialDay == 5)
responsemodel <- lm(avespeed_tot ~ X , data=KARR_ACT2)
responsemodel
hist(KARR_ACT$avespeed_tot)
