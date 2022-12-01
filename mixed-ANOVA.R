

###############################################################################
###################### SCRIPT OF MAIN ANALYSES : 
###################### ANALYSES FOR MIXED ANOVA : added experiment as a between factor
###############################################################################


###################### WITHOUT video stop as a factor

# get PSE from sigmoid fit
pse = fit$par %>% 
  select(c(-parinf, -parsup)) %>%
  tidyr::pivot_wider(names_from = parn, values_from = par) %>%
  rename_at(vars(c(p1, p2)), ~ c("PSE", "JND"))

pse$ID_subj = as.factor(pse$ID_subj)
pse$ID_exp = as.factor(pse$ID_exp)
pse$trialType = as.factor(pse$trialType)

############################################ 
###################### PSE ANALYSES
############################################
model_mixed = lm(PSE ~ ID_exp*trialType, data=pse)

#### Testing ANOVA Assumptions for mixed-ANOVA (data normally distributed & equal variance between groups)
jarque.bera.test(model_mixed$residuals) # Test normality of data distribution
hist(model_mixed$residuals)
bartlett.test(PSE ~ ID_exp, data = pse) # Test homoscedasticity

#### ANOVA on PSEs
anova_pse <- ezANOVA(data=pse , dv= .(PSE), wid= .(ID_subj), between = .(ID_exp), within=.(trialType), 
                     detailed=TRUE, return_aov=TRUE, type=2)
anova_pse$ANOVA #results of ANOVA



############################################ 
###################### JND ANALYSES
############################################

model_mixed_JND = lm(JND~ID_exp*trialType, data=pse)

### ANOVA assumptions
jarque.bera.test(model_mixed_JND$residuals) # Test normality 
hist(model_mixed_JND$residuals)
bartlett.test(JND ~ ID_exp, data = pse) # Test homoscedastiticy

####### Analysis without outlier
mixed_outlier = pse %>%
  filter(ID_subj!=41)

model_mixed_outlier = lm(JND~ID_exp*trialType, data=mixed_outlier)
jarque.bera.test(model_mixed_outlier$residuals) # Test normality 
hist(model_mixed_outlier$residuals)
bartlett.test(JND ~ ID_exp, data = mixed_outlier) # Test homoscedastiticy

#### ANOVA on JNDs
anova_JND <- ezANOVA(data=mixed_outlier , dv= .(JND), wid= .(ID_subj), between = .(ID_exp), within=.(trialType),
                     detailed=TRUE, return_aov=TRUE, type=2)
anova_JND$ANOVA #results of ANOVA



###################### WITH video stop as a factor


# get PSE from sigmoid fit
pse_stop = fit_stop$par %>% 
  select(c(-parinf, -parsup)) %>%
  tidyr::pivot_wider(names_from = parn, 
                     values_from = par) %>%
  rename_at(vars(c(p1, p2)), ~ c("PSE", "JND"))

pse_stop$timeVideo = as.factor(pse_stop$timeVideo)
pse_stop$ID_subj = as.factor(pse_stop$ID_subj)
pse_stop$ID_exp = as.factor(pse_stop$ID_exp)
pse_stop$trialType = as.factor(pse_stop$trialType)


############################################ 
###################### PSE ANALYSES
############################################

############ Testing ANOVA Assumptions for between-ANOVA (data normally distributed & equal variance between groups)
model_mixed_stop = lm(PSE~ID_exp*trialType*timeVideo, data=pse_stop)
jarque.bera.test(model_mixed_stop$residuals) # Test normality
#hist(model_mixed_stop$residuals)
bartlett.test(PSE ~ ID_exp, data = pse_stop) # Test homoscedasticity

### ANOVA pse
anova_pse2 <- ezANOVA(data=pse_stop , dv= .(PSE), wid= .(ID_subj), between = .(ID_exp), within=.(trialType, timeVideo), 
                      detailed=TRUE, return_aov=TRUE, type=2)
anova_pse2$ANOVA #results of ANOVA


############################################ 
###################### JND ANALYSES
############################################

model_mixed_stop_JND = lm(JND~ID_exp*trialType*timeVideo, data=pse_stop)

jarque.bera.test(model_mixed_stop_JND$residuals) # Test normality
hist(model_mixed_stop_JND$residuals)
bartlett.test(JND ~ ID_exp, data = pse_stop) # Test homoscedasticity

# Visual identification of outlier with a boxplot
ggboxplot(
  pse_stop, x = "ID_exp", y = "JND", color = "trialType") 

###### Analysis without outlier(s)
mixed_outlier_stop = pse_stop %>% 
  filter(ID_subj!=41) # Remove outlier

# Boxplot after removing outlier
ggboxplot(
  mixed_outlier_stop, x = "ID_exp", y = "JND", color = "trialType") 

model_mixed_outlier_stop = lm(JND~ID_exp*trialType*timeVideo, data=mixed_outlier_stop)

#### Testing ANOVA Assumptions for mixed-ANOVA (data normally distributed & equal variance between groups)
jarque.bera.test(model_mixed_outlier_stop$residuals)  # Test normality
hist(model_mixed_outlier_stop$residuals)
bartlett.test(JND ~ ID_exp, data = mixed_outlier_stop) # Test homoscedasticity

###### Data do not follow a normal distribution
###### Will proceed with an Aligned Ranked Transform ANOVA with the R package "ARTool":
###### Wobbrock J, Findlater L, Gergle D, Higgins J (2011). "The Aligned Rank Transform for Nonparametric 
######         Factorial Analyses Using Only ANOVA Procedures." In Proceedings of the ACM Conference on Human Factors 
######         in Computing Systems (CHI '11), 143-146. doi:10.1145/1978942.1978963, 
######         https://depts.washington.edu/acelab/proj/art/. 

model = art(JND ~ ID_exp*trialType*timeVideo + (1|ID_subj),
            data = mixed_outlier_stop)
model
Result = anova(model)
Result$part.eta.sq = with(Result, `F` * `Df` / (`F` * `Df` + `Df.res`))
Result

