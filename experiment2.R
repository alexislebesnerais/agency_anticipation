
###############################################################################
###################### SCRIPT FOR RESULTS OF EXPERIMENT 2 : 
###################### HINDERED SOCIAL AFFORDANCES
###############################################################################



###################### ANALYSES WITHOUT video stop as a factor

pseXP2 = fit$par[fit$par$ID_exp == 'nonaffordant',] %>% 
  select(c(-parinf, -parsup)) %>%
  tidyr::pivot_wider(names_from = parn, 
                     values_from = par) %>%
  rename_at(vars(c(p1, p2)), ~ c("PSE", "JND"))

pseXP2$ID_subj = as.factor(pseXP2$ID_subj)
pseXP2$trialType = as.factor(pseXP2$trialType)#trialType specifies the agency condition (self, external)


############################################ 
###################### PSE ANALYSES
############################################

model_xp2 = lm(PSE~trialType, data=pseXP2)

### Testing ANOVA Assumptions (data normally distributed & equal variance between groups)
jarque.bera.test(model_xp2$residuals)

### ANOVA
anova_pse_xp2 <- ezANOVA(data=pseXP2 , dv= .(PSE), wid= .(ID_subj), within=.(trialType), 
                         detailed=TRUE, return_aov=TRUE, type=2)
anova_pse_xp2$ANOVA #results of ANOVA


## t-tests
self2 = pseXP2 %>%
  filter(trialType == 'self')
t.test(self2$PSE)

external2 = pseXP2 %>%
  filter(trialType == 'external')
t.test(external2$PSE)

# get mean and sd of pse values 
msdpse2 = pseXP2 %>%
  group_by(trialType) %>% #timeVideo,
  summarise_at(vars(PSE), list(mean = mean, sd=sd))
msdpse2  

############################################ 
###################### JND ANALYSES
############################################
model_JND_xp2 = lm(JND~trialType, data=pseXP2)

# Test if our data follow a normal distribution (Jarque-Bera)
jarque.bera.test(model_JND_xp2$residuals)
hist(model_JND_xp2$residuals)

# Is there an outlier in the data ?
# Use boxplot representation to find a possible outlier
Boxplot(JND ~ trialType, id.method="y", data=pseXP2)
# boxplot representation find row 16 (ID_subj=41) to be an outlier

xp2_outlier = pseXP2 %>%
  filter(ID_subj!=41) ## Remove outlier 16 (subject 41 in external agency condition)

model_outlier = lm(JND~trialType, data=xp2_outlier)

# Test if our data follow a normal distribution (Jarque-Bera)
jarque.bera.test(model_outlier$residuals)
hist(model_outlier$residuals)

## ANOVA
anova_JND_xp2 <- ezANOVA(data=xp2_outlier , dv= .(JND), wid= .(ID_subj), within=.(trialType), 
                         detailed=TRUE, return_aov=TRUE, type=2)
anova_JND_xp2$ANOVA #results of ANOVA

# get mean and sd of jnd values
msdJND = pseXP2 %>%
  group_by(ID_exp) %>%
  summarise_at(vars(JND), list(mean = mean, sd=sd))
msdJND  



##################################################################
###################### ANALYSES WITH video stop as a factor
##################################################################

pseXP2_stop = fit_stop$par[fit_stop$par$ID_exp == 'nonaffordant',] %>% 
  select(c(-parinf, -parsup)) %>%
  tidyr::pivot_wider(names_from = parn, 
                     values_from = par) %>%
  rename_at(vars(c(p1, p2)), ~ c("PSE", "JND"))

pseXP2_stop$ID_subj = as.factor(pseXP2_stop$ID_subj)
pseXP2_stop$trialType = as.factor(pseXP2_stop$trialType)
pseXP2_stop$timeVideo = as.factor(pseXP2_stop$timeVideo)

############################################ 
###################### PSE ANALYSES
############################################

##### Model
model_xp2_stop = lm(PSE~trialType*timeVideo, data=pseXP2_stop)

### Testing ANOVA Assumptions (data normally distributed)
# Test if our data follow a normal distribution (Jarque-Bera test)
jarque.bera.test(model_xp2_stop$residuals)


### ANOVA
anova_pse_xp2_stop <- ezANOVA(data=pseXP2_stop , dv= .(PSE), wid= .(ID_subj), within=.(timeVideo, trialType), 
                         detailed=TRUE, return_aov=TRUE, type=2)
anova_pse_xp2_stop$ANOVA #results of ANOVA

msdpse_stop = pseXP2_stop %>%
  group_by(trialType, timeVideo) %>% #ID_exp
  summarise_at(vars(PSE), list(mean = mean, sd=sd))
msdpse_stop  


## t-tests
## self-agency / Video Stop 670 ms
self2_670 = pseXP2_stop %>%
  filter(trialType == 'self', timeVideo == 670)
t.test(self2_670$PSE)

## self-agency / Video Stop 700 ms
self2_700 = pseXP2_stop %>%
  filter(trialType == 'self', timeVideo == 700)
t.test(self2_700$PSE)

## external-agency / Video Stop 670 ms
external2_670 = pseXP2_stop %>%
  filter(trialType == 'external', timeVideo == 670)
t.test(external2_670$PSE)

## external-agency / Video Stop 700 ms
external2_700 = pseXP2_stop %>%
  filter(trialType == 'external', timeVideo == 700)
t.test(external2_700$PSE)

############################################ 
###################### JND ANALYSES
############################################
model_JND_xp2_stop = lm(JND~timeVideo*trialType, data=pseXP2_stop)

# Test if our data follow a normal distribution (Jarque-Bera)
jarque.bera.test(model_JND_xp2_stop$residuals)
hist(model_JND_xp2_stop$residuals)

xp2_stop_outlier = pseXP2_stop %>%
  filter(ID_subj!=41) # Remove outlier subject 41

model_outlier_stop = lm(JND~timeVideo*trialType, data=xp2_stop_outlier)

# Test if our data follow a normal distribution (Jarque-Bera)
jarque.bera.test(model_outlier_stop$residuals)
hist(model_outlier_stop$residuals)

## ANOVA
anova_JND_xp2_stop <- ezANOVA(data=xp2_stop_outlier , dv= .(JND), wid= .(ID_subj), within=.(timeVideo, trialType), 
                         detailed=TRUE, return_aov=TRUE, type=2)
anova_JND_xp2_stop$ANOVA #results of ANOVA

## POST HOC 
contrast(emmeans(model_outlier_stop, ~ timeVideo:trialType), method = "tukey") #pairwise

# get mean and sd of pse values
msdJND = pseXP2_stop %>%
  group_by(trialType, timeVideo) %>%
  summarise_at(vars(JND), list(mean = mean, sd=sd))
msdJND  


