#	-------------------------------------------------------------------------------------------------------------------
# ----
#	----	ANALYSIS: EFFECTS OF EXTREME TEMPERATURE ON GERMAN EMERGENCY CARE
#	----
#	----  R_Script_01_Setup
#	----
#	-------------------------------------------------------------------------------------------------------------------

#   Author: 					          Jona Frasch 
#   E-Mail: 					          j.frasch@uke.de 
#   Tel.: 						          +49 40 7410 52676


#	-------------------------------------------------------------------------------------------------------------------
#	----
#	----	Block I: SET-UP ANALYSIS
# ----
#	-------------------------------------------------------------------------------------------------------------------


#----------------------------------------------------------------------------------------
# I.A LOAD REQUIRED PACKAGES
#----------------------------------------------------------------------------------------

if(installrequiredpackages==TRUE) install.packages(pkgs=c("tidyverse", "readxl", "writexl", "xlsx", "plyr",
                        "scales", "rstatix", "parallel", "dlookr", "gridExtra", "ggforce", "ggpubr",
                        "grid", "RColorBrewer", "ggpattern", "magick",
                        "dlnm", "splines", "tsModel", "mixmeta", "MASS"), dependencies=TRUE)

library(tidyverse)
library(readxl)
library(writexl)
library(xlsx)
library(plyr)
library(scales)
library(rstatix)
library(parallel) 
library(dlookr)
library(gridExtra)
library(ggforce)
library(ggpubr)
library(grid)
library(RColorBrewer)
library(ggpattern)
library(magick)

# FOR DLNM
library(dlnm)
library(splines)
library(tsModel)
library(mixmeta)
library(MASS)



#----------------------------------------------------------------------------------------
# I.B LOAD REQUIRED PACKAGES
#----------------------------------------------------------------------------------------

####	IMPORT-DATA ###

# MAIN ANALYSIS-DATA ON EMERGENCY ADMISSIONS
load("Data/data_N_Admissions.RData") 

datain <- datain %>% arrange(county, time)

# BACKGROUND-INFORMATION ON COUNTIES FOR META-ANALYSIS
load("Data/data_counties.RData")


#----------------------------------------------------------------------------------------
# I.C DEFINE LISTS AND PARAMETERS
#----------------------------------------------------------------------------------------

# LIST OF COUNTY-IDs
list_county_id <- as.list(unique(datain$county))

# LIST COUNTY-LEVEL-DATA  
list_countydata <- lapply(list_county_id, function(county_id) datain[datain$county==county_id,])
names(list_countydata) <- list_county_id

county_order_revers_by_population <- rev(order(data_counties$EWZ))


#------------------------------------------------------------------------------------------------------------------
# I.D DEFINE THE PARAMETERS APPLIED IN THE PARAMETRIZATION OF THE STAGE 1 MODEL 
#------------------------------------------------------------------------------------------------------------------

### ktemp: PLACEMENT OF KNOTS ALONG THE SPACE OF THE PREDICTOR ###

# ktemp <- c(.10, .75, .90)
ktemp <- c(.10, .75, .90)


### list_nk_lag_in: NUMBER OF KNOTS ALONG THE LAGGED SPACE OF THE PREDICTOR (THOSE LISTED ARE TRIED IN SEPARATE MODELS) ###

# list_nk_lag_in <- c(1, 2, 3, 4)
list_nk_lag_in <- c(2)


### list_nd_lag_in: NUMBER OF DAYS IN THE LAG PERIOD THE LAGGED SPACE OF THE PREDICTOR (THOSE LISTED ARE TRIED IN SEPARATE MODELS) ###

# list_nd_lag_in <- c(21, 28, 35, 42)
list_nd_lag_in <- c(28)


### list_dfperyear_in: NUMBER OF DEGREES OF FREEDOM IN THE NATURAL SPLINE FUNCTION TO CONTROL FOR SEASONAL- AND LONG-TERM TRENDS (THOSE LISTED ARE TRIED IN SEPARATE MODELS) ###

# list_dfperyear_in <- c(7, 8, 9, 10)
list_dfperyear_in <- c(8)


#------------------------------------------------------------------------------------------------------------------
# ADDITIONAL STEP TO SHOW FINAL STUDY RESULTS: LOAD STUDYIES FINAL STAGE 1 ESTIMATES, ANALYSED USING COMPLETE DATA
#------------------------------------------------------------------------------------------------------------------

# Note: The dataset datain (from data_N_Admissions.RData) and list_countydata made available here are comprised of data data filtered for cells (county-day) 
# where the N_Admissions variable is larger than 3. That is, more than three emergency admissions were observed in the given county on a given day.
# This accomodates data-protection and privacy regulations imposed by the data-owner. 
# This data is provided here to make the analysis reproducible. It does not allow to reproduce the final results of the current study, 
# as these were calculated based on the complete dataset. 
# To show the final results parallel to those calculated here, we import the study's final Stage 1 results here.
# The meta analysis is subsequently run on Stage 1 results based on both the complete and filtered data in Block .

load("Data/data_coefs_vcovs_S1_final.RData")













