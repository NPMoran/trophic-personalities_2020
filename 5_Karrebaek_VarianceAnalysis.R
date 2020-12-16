
responsemodel <- lmer(behav.var ~ TrialDay + TrialDay*Treatment + (1|FishID), data=KARRdat)
