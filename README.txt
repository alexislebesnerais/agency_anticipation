In this README file you will find 1/ a description of the columns and their signification of csv file (results) and 2/ how to proceed
in order to correctly run the different R scripts as well as the library used that need to be installed beforehand.

****************************************************

1/ Description of the csv file

The csv files contains the results of each trial for every participants (experiment 1 and 2). Each row is a unique trial.
trialType corresponds to our Agency factor with self and external conditions.
timeVideo corresponds to our Video Stop factor with 670 and 700 ms conditions.
ID_exp corresponds to our different experiments. Their data was concatenated inside a single csv file. Affordant correspond to our
first experiment and nonaffordant to our second experiment.
Response corresponds to the key the participant pressed to answer.
adaptedResponse is composed of 0s and 1s. "0" matches with "6" in the "Response" column, "1" with "4". It is used for the sigmoid fit.

------------------------------------------------------------------
2/ Executing scripts
2.1/ Install the required libraries with the following command :
>>> install.packages(c("dplyr", "plyr", "readr", "ez", "quickpsy","tseries", "car", "ez", "ggpubr", "ARTool", "emmeans"))

2.2/ Order of script execution

1- First you need to fit the data. "fit_data.R" will do it with the "quickpsy" package you previously installed. This script will also
load the different packages used in the different R scripts.
2- The others scripts are independant from each other. You can run them in the order you want.

2.3/ Global Structure of the scripts

The analyses scripts (experiment1.R; experiment2.R; mixed-ANOVA.R) follow the same structure:

1- Analyses without Video Stop as a factor
1.1- PSE analyses
1.2- JND analyses
2- Analyses with Video Stop as a factor
2.1- PSE analyses
2.2- JND analyses