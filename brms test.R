
Sys.setenv(LANG = "en")

#Loading required packages- 


library(dplyr); library(ggplot2); library(ggpubr)
library(lme4); library(lmerTest); library(car); library(rptR); library(performance)

library(devtools)
find_rtools()
install.packages("brms")
library(arm); library(brms)
#writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
#Sys.which("make")
### "C:\\rtools40\\usr\\bin\\make.exe"

#General theme for ggplots-
simpletheme <-   theme(axis.text.y = element_text(size = 10, colour = "black"), axis.text.x = element_text(size = 10, colour = "black"),  panel.background = element_rect(fill = "white"), axis.title.y  = element_text(size=12, vjust = 2), axis.title.x  = element_text(size=12, vjust = 0.1), panel.border = element_rect(colour = "black", fill=NA, size = 1))



#lmer approach
KARRact.processed <- read.csv("~/trophicpersonalities_A/Data_Karrebaek/KARR_ACTdat_processed.csv")
labels(KARRact.processed)
KARRact.processed2 <- subset(KARRact.processed, TrialDay != 'day 10')

versionlmer <- lmer(sqrt(dist) ~ 
                                1 + TrialDay + (1 + TrialDay|Treatment) + (1|FishID), data=KARRact.processed2)
Anova(versionlmer)                 #Sex, ConditionFactor.C effects
summary(versionlmer)               #TankID resolves no variance, TrialRound extremely little
plot(versionlmer)                  #No clustering issues
r2_nakagawa(versionlmer)           #random structure error
coef(versionlmer)


#preliminary model specifications
adapt_delta_value <- 0.9999
max_treedepth_value <- 20
iterations <- 6000
burnin <- 3000
thinning <- 2

#for saving model
filename <- paste0("models/brms.smd.base",
                   "sharedcontrol_",
                   iterations,"iter_",
                   burnin,"burnin_",
                   thinning,"thin_",
                   adapt_delta_value,"delta_",
                   max_treedepth_value,"treedepth.RData")

versionbrms <- brm(dist ~ 1 + TrialDay + (1 | Treatment), 
                              data   = KARRact.processed2, 
                              warmup = 100, 
                              iter   = 200, 
                              chains = 2, 
                              inits  = "random",
                              cores  = 2)



