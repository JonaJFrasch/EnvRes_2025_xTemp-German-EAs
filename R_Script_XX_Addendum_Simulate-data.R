#	-------------------------------------------------------------------------------------------------------------------
# ----
#	----	ANALYSIS: EFFECTS OF EXTREME TEMPERATURE ON GERMAN EMERGENCY CARE
#	----
#	----  R_Script_XX_Addendum_Simulated-data
#	----
#	-------------------------------------------------------------------------------------------------------------------

#   Author: 					          Jona Frasch 
#   E-Mail: 					          j.frasch@uke.de 
#   Tel.: 						          +49 40 7410 52676

# This script was used to create "simulated" version of the data on the number of emergency admissions, to avoid infrigements of 
# data protection regulations. It is applied in the public repository to allow a replication of our analysis. 
# It does not allow a replication of the final published results. 
# The code and results in this script are not reproducible using the publicly available files, as it uses the original data. 

#	-------------------------------------------------------------------------------------------------------------------
#	----
#	----	Block I: SET-UP ANALYSIS
# ----
#	-------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------
# I.A LOAD REQUIRED PACKAGES
#----------------------------------------------------------------------------------------

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
# I LOAD REQUIRED PACKAGES
#----------------------------------------------------------------------------------------

####	IMPORT-DATA ###

# MAIN ANALYSIS-DATA ON EMERGENCY ADMISSIONS
load("Data/data_N_Admissions_org.RData") 

datain <- datain %>% arrange(county, time)

# BACKGROUND-INFORMATION ON COUNTIES FOR META-ANALYSIS
load("Data/data_counties.RData")


#----------------------------------------------------------------------------------------
# II DEFINE LISTS AND PARAMETERS
#----------------------------------------------------------------------------------------

# LIST OF COUNTY-IDs
list_county_id <- as.list(unique(datain$county))

# LIST COUNTY-LEVEL-DATA  
list_countydata <- lapply(list_county_id, function(county_id) datain[datain$county==county_id,])
names(list_countydata) <- list_county_id

county_order_revers_by_population <- rev(order(data_counties$EWZ))

#------------------------------------------------------------------------------------------------------------------
# III DEFINE THE PARAMETERS APPLIED IN THE PARAMETRIZATION OF THE STAGE 1 MODEL 
#------------------------------------------------------------------------------------------------------------------

ktemp <- c(.10, .75, .90)
nk_lag <- 2
nd_lag <- 28
dfperyear <- 8
linkfamily <- quasipoisson()
linkfamilyname <- "qp"


#------------------------------------------------------------------------------------------------------------------
# IV GET GLMs FOR PREDICTION 
#------------------------------------------------------------------------------------------------------------------

# Implement model fitting using parallel processing 
list_S1_data <- llply(list_countydata, function(countydata_used){
  
  # Data import, editing and transformation
  library(plyr)
  library(dplyr)
  
  # For the implementation of distributed lag non-linear models 
  library(dlnm)
  library(splines)
  library(tsModel)
  library(MASS)
  
  county_id <- unique(countydata_used$county)
  
    # SETUP ERROR HANDLING
    
    # Select County Specific Sample 
    data_interm <- countydata_used %>% 
      dplyr::mutate(N_CASES=N_Admissions) %>%
      dplyr::select(all_of(c("Date", "TMKp",  "N_Admissions", "time", "dow", "school_holiday"))) %>% 
      arrange(time)
    
    county_id <- unique(countydata_used$county)
    
    
    # FIXING THE KNOTS AT EQUALLY SPACED LOG-VALUES OF LAG
    klag <- logknots(nd_lag,nk=nk_lag)
    
    if(linkfamily[[1]]=="binomial"){
      weight <- data_interm$N_CASES
    } else {
      weight <- NULL
    }
    
    # CROSSBASIS SPECIFICATION
    
    # CROSSBASIS MATRIX: Natural splines
    basis <- crossbasis(data_interm$TMKp,
                        argvar=list(knots=ktemp, fun="ns"),
                        arglag=list(knots=klag, fun="ns"),
                        lag=nd_lag)
    
    if(linkfamily[[1]]=="Negative Binomial(NA)"){
      model <- glm.nb(N_Admissions ~  basis  + ns(time, dfperyear*10) + dow + school_holiday,
                      data_interm)
    } else {
      
      model <- glm(N_Admissions ~  basis  + ns(time, dfperyear*10) + dow + school_holiday,
                   family=linkfamily,
                   data_interm,
                   weights=weight)
    }
    
    model.class <- class(model)
    model.link <- dlnm:::getlink(model,model.class)
    
    
    # COEFS AND VCOVS COMPLETE MODEL
    crosspredictions <- crosspred(basis,model,cen=.5)
    coef <- coef(crosspredictions)
    vcov <- vcov(crosspredictions)
    
    ### MODEL PARAMETERS AND FIT-STATISTICS ###
    # SAVE THE AIC VALUE
    fit <- model$aic
    
    if(is.na(fit)){
      # COMPUTE AND SAVE THE Q-AIC VALUE
      loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
      phi <- summary(model)$dispersion
      fit <- -2*loglik + 2*summary(model)$df[3]*phi
      
      fitstat <- "QAIC"
    }  else {fitstat  <- "AIC"}
    
    # SAVE MODEL DEGREES OF FREEDOM
    df1 <- attributes(basis)$df[1]
    df2 <- attributes(basis)$df[2]
    
    # SAVE KNOT-PERCENTILES AS STRING
    ktemp_string <- paste(ktemp,collapse=' ')
    
    # OUTPUT IF NO MODEL ERROR
    MODEL_ERROR <- FALSE
    
   
    return(model)
    
})



#------------------------------------------------------------------------------------------------------------------
# V SIMULATE N_ADMISSIONS 
#------------------------------------------------------------------------------------------------------------------

dataout <- ldply(names(list_countydata), function(label){

  data_use <- list_countydata[[label]] 
  model_use <- list_S1_data[[label]]
  
  data_use$N_Admissions_pred <- round(predict(model_use, newdata = data_use, type="response"))
  
  return(data_use)  

})


datain <- dataout %>% 
  dplyr::mutate(N_Admissions=N_Admissions_pred) %>% 
  dplyr::select(-N_Admissions_pred)



#------------------------------------------------------------------------------------------------------------------
# V OUTPUT
#------------------------------------------------------------------------------------------------------------------


save(datain, file="Data/data_N_Admissions.RData")
getwd()


