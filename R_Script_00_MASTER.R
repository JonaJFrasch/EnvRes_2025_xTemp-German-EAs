
#	-------------------------------------------------------------------------------------------------------------------
# ----
#	----	ANALYSIS: EFFECTS OF EXTREME TEMPERATURE ON GERMAN EMERGENCY CARE
#	----
#	----  *** MASTER SCRIPT ***
# ----  R_Script_00_MASTER
#	----
#	-------------------------------------------------------------------------------------------------------------------

#   Author: 					          Jona Frasch 
#   E-Mail: 					          j.frasch@uke.de 
#   Tel.: 						          +49 40 7410 52676


#   Data :
#           datain              County- & day-level observations of the number of emergency admissions and the mean daily temperature.
#                               The here provided data was filtered based on data-protection / privacy regulations imposed by the data-owner.
#                               Cells based on fewer than 3 emergency admissions were removed.
#           data_counties       County information for random-effects meta-analysis
#

#   Variables :
#       datain :
#           county                County-identifier
#           Date                  Date of Admission
#           time                  Number of Days since the beginning of the observation period
#           school_holiday        Indicator variable for school holidays
#           dow                   Day of the week
#           TMKp                  County-specific percentile of the daily average temperature
#           N_Admissions          Number of Emeregncy Admissions on a given day in a given county
#
#       data_counties :
#           county - County-identifier
#           AGS_uni - Official county key
#           county_name - County name
#           EWZ - population size
#           X - X-coordinate of geogrpahical center
#           Y - Y-coordinate of geogrpahical center

#----------------------------------------------------------------------------------------
# NOTE ON THE STUDY AND SCRIPT
#----------------------------------------------------------------------------------------

# NOTE: The here provided script and data aim to reproduce an analysis on the effects of temperature and 
# extreme temperature on emergency admissions to German hospitals. To reproduce the analysis, 
# we provide a dataset that has been filteredb based on data-protection rules, but import and report results from analysis
# on the complete data-set.
#
# The study implements a two stage time-series design.
# In the first stage, a distributed lag non-linear model is fit to data of each of Germanys counties spanning 10 years. 
# Here, the effect of temperature is modeled using a natural cubic spline along both the space of the predictor and its lag. 
# Covariates include the day of the week, an indicator for school holidays, as well as a natural cubic spline function with 
# dfperyear degrees of freedom per year. In the final study, the number of knots along the space of the predictor and its lag, 
# dfperyear, and the length of the lag period is varied. Here, we report only those of the main analysis. 
#In the second stage, results from the first stage are aggregated using meta analysis. 


#----------------------------------------------------------------------------------------
# SETTINGS
#----------------------------------------------------------------------------------------

installrequiredpackages <- FALSE # SET THIS TO TRUE, IF REQUIRED PACKAGES SHOULD BE INSTALLED

runexploratoryanalyses <- FALSE # SET THIS TO TRUE, IF THE EXPLORATORY ANALYSIS SHOULD BE REPRODUCED (somewhat lengthy)

#----------------------------------------------------------------------------------------
# CALLING SCRIPTS
#----------------------------------------------------------------------------------------

###-----------------------------------------------###
###-------------- R_Script_01_Setup --------------###

# NOTE: Script imports data, defines lists required later in the analysis, and defines the parameters used for the 
# parametrization of Stage 1 models (number and placemen of knots in the spline-functions, length of the lag period, etc).

source("R_Script_01_Setup.R", echo = TRUE, max.deparse.length = 99999)


###-----------------------------------------------###
###----------- R_Script_02_Stage-1-DLNM ----------###

# Note: Stage 1 analysis with the model parametrizations defined previously (here, only one model is implemented). 

source("R_Script_02_Stage-1-DLNM.R", echo = TRUE, max.deparse.length = 99999)


###-----------------------------------------------###
###------ R_Script_03_Stage-2-Meta-Analysis ------###

# NOTE: A Fixed and a random effects meta analysis based on the reduced Stage 1 estimates of stage 1 results. 
# Separate analysis for the overall effect, and for the lagged effect of temperature at the 1st and 99th percentile.
# Non-reduced meta-analysis would have to be uncommented.

source("R_Script_03_Stage-2-Meta-Analysis.R", echo = TRUE, max.deparse.length = 99999)


###-----------------------------------------------###
###------------- R_Script_04_Results -------------###

# NOTE: Results of Stage 2 Results are reported.

source("R_Script_04_Results.R", echo = TRUE, max.deparse.length = 99999)

###-----------------------------------------------###
### R_Script_05_Exploratory-Analysis-County-Level ### 

# NOTE: County-specific distribution of outcomes, visualization on seasonal trend, plots of Stage 1 analyses (county specific models).

if(runexploratoryanalyses==TRUE) source("R_Script_05_Exploratory-Analysis-County-Level.R", echo = TRUE, max.deparse.length = 99999)


