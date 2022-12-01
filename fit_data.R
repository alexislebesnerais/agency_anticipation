my_packages <- c("dplyr", "plyr", "readr", "ez", "quickpsy",
                 "tseries", "car", "ez", "ggpubr", "ARTool", "emmeans")     
lapply(my_packages, require, character.only = TRUE)    # Load multiple packages

# Need to specify the path from you computer. 
# If you use windows, use slash (/) or double backslash (\\) in the path
data = read.csv("raw_results_XP_affordant-and-nonaffordant.csv", sep=';')

# Fit sigmoid function for each participant by agency conditions for both experiments 
fit <- quickpsy(data, ImgCondition, adaptedResponse, grouping = .(trialType, ID_subj, ID_exp), B=5)

# Fit sigmoid function for each participant by agency and video stop conditions for both experiments 
fit_stop <- quickpsy(data, ImgCondition, adaptedResponse, grouping = .(trialType, ID_subj, ID_exp, timeVideo), B=5) #timeVideo

