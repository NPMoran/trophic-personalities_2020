# Project: Personality in Community Ecology Responses: Integrating the behaviour and species interactions of a marine invader — PinCER
#
# Experiment: Quantification of among-individual behavioural and trophic variation the invasive round goby
#
# Author: Nicholas Moran, The Centre for Ocean Life- DTU Aqua, Technical University of Denmark



#Guldborgsund 2. Variance analysis ----


Sys.setenv(LANG = "en")


#Loading required packages- 
library(dplyr); library(ggplot2); library(lme4); library(lmerTest); library(car)
library(survival); library(survminer); library(rptR); library(performance)




### G.1.3 Distribution checks for behavioural variables (ACT) ----
#avespeed_tot: (mm/s) the average speed of the individual accross the full trial period
ggplot(GULDact) + aes(x = avespeed_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 4.5) + simpletheme 
ggqqplot(GULDact$avespeed_tot) #relatively close to normality
#ggplot(GULDact) + aes(x = sqrt(avespeed_tot)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
#ggqqplot(sqrt(GULDact$avespeed_tot)) #root transformation worse

#avespeed_mob: (mm/s) the average speed of the individual excluding periods when it was immobile
ggplot(GULDact) + aes(x = avespeed_mob) + geom_histogram(color="black", fill="lightblue", binwidth = 5) + simpletheme 
ggqqplot(GULDact$avespeed_mob) #minimal negative skew due to 5 - 10 very inactive fish


#propmoving: (proportional) proportion of time mobile
ggplot(GULDact) + aes(x = propmoving) + geom_histogram(color="black", fill="lightblue", binwidth = 0.05) + simpletheme 
ggqqplot(GULDact$propmoving) #negative skew, potential issues with high proportion of low activity fish
ggplot(GULDact) + aes(x = exp(propmoving)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.055) + simpletheme 
ggqqplot(exp(GULDact$propmoving)) #exp is improved
#ggplot(GULDact) + aes(x = sqrt(propmoving)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.05) + simpletheme 
#ggqqplot(sqrt(GULDact$propmoving)) #root transformation worse

#dist: (mm) total distance travelled during trial
ggplot(GULDact) + aes(x = dist) + geom_histogram(color="black", fill="lightblue", binwidth = 4800) + simpletheme 
ggqqplot(GULDact$dist) #relatively close to normal
#ggplot(GULDact) + aes(x = log(dist)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.1) + simpletheme 
#ggqqplot(sqrt(GULDact$dist)) ##root transformation worse

#frozenevents: (count) 
ggplot(GULDact) + aes(x = frozenevents) + geom_histogram(color="black", fill="lightblue", binwidth = 8) + simpletheme 
ggqqplot(GULDact$frozenevents) #positive skew to (could try poisson)
ggplot(GULDact) + aes(x = sqrt(frozenevents)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.7) + simpletheme 
ggqqplot(sqrt(GULDact$frozenevents)) #very normal

#timefrozen_tot: (s) total time spent frozen during trial
ggplot(GULDact) + aes(x = timefrozen_tot) + geom_histogram(color="black", fill="lightblue", binwidth = 85) + simpletheme 
ggqqplot(GULDact$timefrozen_tot) #potential issues with high proportion of low activity fish (may try binomial)

#timefrozen_ave: (s) total duration of frozen periods
ggplot(GULDact) + aes(x = timefrozen_ave) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
ggqqplot(GULDact$timefrozen_ave) #severe positive skew zero so log transformation applied (consider running as a poisson distribution)
ggplot(GULDact) + aes(x = log(timefrozen_ave)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.45) + simpletheme 
ggqqplot(log(GULDact$timefrozen_ave)) #log transformation is improved but still quite skewed,

#centretime50: (s) time >5cm away from edge
ggplot(GULDact) + aes(x = centretime50) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
ggqqplot(GULDact$centretime50) #positive skew with zeros so log(x+1) and sqrt transformation applied
ggplot(GULDact) + aes(x = log(centretime50 + 1)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULDact$centretime50 + 1)) #left skewed
ggplot(GULDact) + aes(x = sqrt(centretime50)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
ggqqplot(sqrt(GULDact$centretime50)) #approximately normal


#centretime75: (s) time >7.5cm away from edge
ggplot(GULDact) + aes(x = centretime75) + geom_histogram(color="black", fill="lightblue", binwidth = 45) + simpletheme 
ggqqplot(GULDact$centretime75) #positive skew with zeros so log(x+1) and sqrt transformation applied
ggplot(GULDact) + aes(x = log(centretime75 + 1)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULDact$centretime75 + 1)) #minor negative skewed
ggplot(GULDact) + aes(x = sqrt(centretime75)) + geom_histogram(color="black", fill="lightblue", binwidth = 2) + simpletheme 
ggqqplot(sqrt(GULDact$centretime75)) #closer to normal, still right skew


#centretime100: (s) time >10cm away from edge
ggplot(GULDact) + aes(x = centretime100) + geom_histogram(color="black", fill="lightblue", binwidth = 15) + simpletheme 
ggqqplot(GULDact$centretime100) #positive skew with zeros so log(x+1) and sqrt transformation applied
ggplot(GULDact) + aes(x = log(centretime100 + 1)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULDact$centretime100 + 1)) # minor negative skewed
ggplot(GULDact) + aes(x = sqrt(centretime100)) + geom_histogram(color="black", fill="lightblue", binwidth = 1.5) + simpletheme 
ggqqplot(sqrt(GULDact$centretime100)) #closer to normal


#centretime100: (s) time >10cm away from edge
ggplot(GULDact) + aes(x = centretime100) + geom_histogram(color="black", fill="lightblue", binwidth = 15) + simpletheme 
ggqqplot(GULDact$centretime100) #positive skew with zeros so log(x+1) and sqrt transformation applied
ggplot(GULDact) + aes(x = log(centretime100 + 1)) + geom_histogram(color="black", fill="lightblue", binwidth = 0.5) + simpletheme 
ggqqplot(log(GULDact$centretime100 + 1)) # minor negative skewed
ggplot(GULDact) + aes(x = sqrt(centretime100)) + geom_histogram(color="black", fill="lightblue", binwidth = 1.5) + simpletheme 
ggqqplot(sqrt(GULDact$centretime100)) #closer to normal
