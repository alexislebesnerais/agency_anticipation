
###############################################################################
###################### SCRIPT FOR RESULTS OF EXPERIMENT 1 : 
###############################################################################

###################### ANALYSES WITHOUT video stop as a factor


pseXP1 = fit$par[fit$par$ID_exp == 'affordant',] %>% 
  select(c(-parinf, -parsup)) %>%
  tidyr::pivot_wider(names_from = parn, 
                     values_from = par) %>%
  rename_at(vars(c(p1, p2)), ~ c("PSE", "JND"))

pseXP1$ID_subj = as.factor(pseXP1$ID_subj)
pseXP1$trialType = as.factor(pseXP1$trialType)#trialType specifies the agency condition (self, external)

############################################ 
###################### PSE ANALYSES
############################################

model_xp1 = lm(PSE~trialType, data=pseXP1)

### Testing ANOVA Assumptions (data normally distributed)
jarque.bera.test(model_xp1$residuals)
hist(model_xp1$residuals)

### ANOVA
anova_pse_xp1 <- ezANOVA(data=pseXP1 , dv= .(PSE), wid= .(ID_subj), within=.(trialType), 
                         detailed=TRUE, return_aov=TRUE, type=2)
anova_pse_xp1$ANOVA #results of ANOVA


##### t-test
self = pseXP1 %>%
  filter(trialType == 'self')
t.test(self$PSE)

external = pseXP1 %>%
  filter(trialType == 'external')
t.test(external$PSE)


# get mean and sd of pse values
msdpse = pseXP1 %>%
  group_by(trialType) %>% 
  summarise_at(vars(PSE), list(mean = mean, sd=sd))
msdpse  


############################################ 
###################### JND ANALYSES
############################################
model_JND_xp1 = lm(JND~trialType, data=pseXP1)

### Testing ANOVA Assumptions (data normally distributed)
jarque.bera.test(model_JND_xp1$residuals) 

anova_JND_xp1 <- ezANOVA(data=pseXP1 , dv= .(JND), wid= .(ID_subj), within=.(trialType), 
                         detailed=TRUE, return_aov=TRUE, type=2)
anova_JND_xp1$ANOVA #results of ANOVA


# get general mean and sd of jnd values as there is no main effect
msdJND = pseXP1 %>%
  group_by() %>%
  summarise_at(vars(JND), list(mean = mean, sd=sd))
msdJND  

##################################################################
###################### ANALYSES WITH video stop as a factor
##################################################################
pseXP1_stop = fit_stop$par[fit_stop$par$ID_exp == 'affordant',] %>% 
  select(c(-parinf, -parsup)) %>%
  tidyr::pivot_wider(names_from = parn, 
                     values_from = par) %>%
  rename_at(vars(c(p1, p2)), ~ c("PSE", "JND"))

pseXP1_stop$ID_subj = as.factor(pseXP1_stop$ID_subj)
pseXP1_stop$trialType = as.factor(pseXP1_stop$trialType)
pseXP1_stop$timeVideo = as.factor(pseXP1_stop$timeVideo)


############################################ 
###################### PSE ANALYSES
############################################
model_xp1_stop = lm(PSE~trialType*timeVideo, data=pseXP1_stop)

### Testing ANOVA Assumptions (data normally distributed)
jarque.bera.test(model_xp1_stop$residuals)

anova_pse_xp1_stop <- ezANOVA(data=pseXP1_stop , dv= .(PSE), wid= .(ID_subj), within=.(trialType, timeVideo), 
                         detailed=TRUE, return_aov=TRUE, type=2)
anova_pse_xp1_stop$ANOVA #results of ANOVA

##### t-test for each combination of condition
## self-agency / Video Stop 670 ms
self_670 = pseXP1_stop %>%
  filter(trialType == 'self', timeVideo == 670)
t.test(self_670$PSE)

## self-agency / Video Stop 700 ms
self_700 = pseXP1_stop %>%
  filter(trialType == 'self', timeVideo == 700)
t.test(self_700$PSE)

## external-agency / Video Stop 670 ms
external_670 = pseXP1_stop %>%
  filter(trialType == 'external', timeVideo == 670)
t.test(external_670$PSE)

## external-agency / Video Stop 700 ms
external_700 = pseXP1_stop %>%
  filter(trialType == 'external', timeVideo == 700)
t.test(external_700$PSE)

# get mean and sd of pse values 
msdpse_stop = pseXP1_stop %>%
  group_by(trialType, timeVideo) %>% 
  summarise_at(vars(PSE), list(mean = mean, sd=sd))
msdpse_stop  



############################################ 
###################### JND ANALYSES
############################################
model_JND_xp1_stop = lm(JND~trialType*timeVideo, data=pseXP1_stop)

### Testing ANOVA Assumptions (data normally distributed)
jarque.bera.test(model_JND_xp1_stop$residuals)

anova_JND_xp1_stop <- ezANOVA(data=pseXP1_stop , dv= .(JND), wid= .(ID_subj), within=.(trialType, timeVideo), 
                              detailed=TRUE, return_aov=TRUE, type=2)
anova_JND_xp1_stop$ANOVA #results of ANOVA

# get general mean and sd of jnd values
msdJND_stop = pseXP1_stop %>%
  group_by() %>%
  summarise_at(vars(JND), list(mean = mean, sd=sd))
msdJND_stop 

