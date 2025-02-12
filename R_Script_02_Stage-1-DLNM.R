#	-------------------------------------------------------------------------------------------------------------------
# ----
#	----	ANALYSIS: EFFECTS OF EXTREME TEMPERATURE ON GERMAN EMERGENCY CARE
#	----
#	----  R_Script_02_Stage-1-DLNM
#	----
#	-------------------------------------------------------------------------------------------------------------------

#   Author: 					          Jona Frasch 
#   E-Mail: 					          j.frasch@uke.de 
#   Tel.: 						          +49 40 7410 52676


#	-------------------------------------------------------------------------------------------------------------------
#	-------------------------------------------------------------------------------------------------------------------
# 
# Block II: STAGE 1 ANALYSIS
#
# -------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------

#	-------------------------------------------------------------------------------------------------------------------
#	II.A FUNCTION FOR STAGE 1 ANALYSIS
# -------------------------------------------------------------------------------------------------------------------

fun_run_dlnm <- function(predictor, outcome, linkfamily, linkfamilyname){
  
  # Get starting time to determine length
  starttime <- Sys.time()
  print(paste("Start Analysis", predictor, outcome, "on", starttime))
  
  # Set-up parallel processing
  no_cores <- detectCores() - 1
  if(no_cores>8){
    no_cores <- 8
  }
  if(no_cores<=0){
    no_cores <- 1
  }
  
  outlabel_use <- paste(outcome, predictor, paste0("nkl", nk_lag), paste0("dfy", dfperyear), paste0(nd_lag, "l"), linkfamilyname, sep="_")
  
  cl <- makeCluster(no_cores)
  clusterExport(cl, c("nd_lag", "dfperyear", "nk_lag", "ktemp"
                      # , "predictor", "outcome", "linkfamily", "linkfamilyname", "list_countydata"
  ))
  
  # Implement model fitting using parallel processing 
  list_S1_data <- parLapply(cl, list_countydata, function(countydata_used){
    
    # Data import, editing and transformation
    library(plyr)
    library(dplyr)
    
    # For the implementation of distributed lag non-linear models 
    library(dlnm)
    library(splines)
    library(tsModel)
    library(MASS)
    
    county_id <- unique(countydata_used$county)
    
    tryCatch({
      # SETUP ERROR HANDLING
      
      # Select County Specific Sample 
      data_interm <- countydata_used %>% 
        dplyr::mutate(N_CASES=N_Admissions) %>%
        rename("predictor"=predictor, "outcome"=outcome) %>% 
        dplyr::select(all_of(c("Date", "predictor",  "outcome", "time", "dow", "school_holiday"))) %>% 
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
      basis <- crossbasis(data_interm$predictor,
                          argvar=list(knots=ktemp, fun="ns"),
                          arglag=list(knots=klag, fun="ns"),
                          lag=nd_lag)
      
      if(linkfamily[[1]]=="Negative Binomial(NA)"){
        model <- glm.nb(outcome ~  basis  + ns(time, dfperyear*10) + dow + school_holiday,
                        data_interm)
      } else {
        
        model <- glm(outcome ~  basis  + ns(time, dfperyear*10) + dow + school_holiday,
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
      
      model_parameters_S1 <- c(outcome, predictor, linkfamilyname, county_id, ktemp_string, nk_lag, nd_lag, dfperyear, fit, fitstat, df1, df2, outlabel_use)
      
      
      # ATTACH MODEL INFO TO CROSSPREDICTIONS
      crosspredictions$outcome <- outcome
      crosspredictions$predictor <- predictor
      crosspredictions$linkfamilyname <- linkfamilyname
      crosspredictions$county_id <- county_id
      crosspredictions$ktemp_string <- ktemp_string
      crosspredictions$nk_lag <- nk_lag
      crosspredictions$nd_lag <- nd_lag
      crosspredictions$dfperyear <- dfperyear
      crosspredictions$label <- outlabel_use
      
      ### RETURN S1 DATA ###
      
      return(list(county_id = county_id
                  , coef = coef
                  , vcov = vcov
                  , crosspredictions = crosspredictions
                  , model_parameters_S1 = model_parameters_S1
      ))
      
    },
    error = function(e) {
      # OUTPUT IF MODEL ERROR
      MODEL_ERROR <- TRUE
      
      return(list("error", county_id))
    })
  })
  
  stopCluster(cl)
  print(paste("Stage 1 complete on", Sys.time()))
  
  # FILTER OUT COUNTIES, WHERE ANALYSIS DID NOT RUN
  list_S1_data_use <- list_S1_data[lapply(list_S1_data, length) > 2]
  
  county_ids_use <- laply(list_S1_data_use, function(S1_data_county_level){
    S1_data_county_level$county_id
  })
  
  # CREATE COEF-TABLE (YMAT) & VCOV-LIST (SLIST)
  coefs <- laply(list_S1_data_use, function(S1_data_county_level){
    S1_data_county_level$coef
  })
  
  coefs <- as.matrix(coefs)
  
  vcovs <- llply(list_S1_data_use, function(S1_data_county_level){
    
    matrix_out <- as.matrix(S1_data_county_level$vcov)
    rownames(matrix_out) <- colnames(matrix_out)
    return(matrix_out)
  })
  
  rownames(coefs) <- names(vcovs) <- county_ids_use
  
  # CREATE LIST WITH COUNTY-LEVEL CROSS-PREDICTIONS 
  list_S1_crosspredictions <- llply(list_S1_data_use, function(S1_data_county_level){
    S1_data_county_level$crosspredictions
  })
  
  # CREATE TABLE WITH MODEL PARAMETERS
  model_parameters_S1 <- ldply(list_S1_data_use, function(S1_data_county_level){
    S1_data_county_level$model_parameters_S1
  })  %>% dplyr::select(-any_of(".id"))
  
  colnames(model_parameters_S1) <-  c("outcome", "predictor", "linkfamilyname", "county", "ktemp", "nk_lag", "nd_lag", "dfperyear",  "fit", "fitstat", "DF1", "DF2", "label")
  
  # OUTPUT RESULTS 
  assign(paste("coefs_S1", outlabel_use, sep="_"), coefs, envir = .GlobalEnv)
  assign(paste("vcovs_S1", outlabel_use, sep="_"), vcovs, envir = .GlobalEnv)
  assign(paste("cps_S1", outlabel_use, sep="_"), list_S1_crosspredictions, envir = .GlobalEnv)
  assign(paste("model_parameters_S1", outlabel_use, sep="_"), model_parameters_S1, envir = .GlobalEnv)
  
}  


#	-------------------------------------------------------------------------------------------------------------------
#	II.B FUNCTION TO EXECUTE STAGE 1 ANALYSIS WITH VARYING PARAMETRIZATIONS
# -------------------------------------------------------------------------------------------------------------------

fun_callmodels <- function(list_nd_lag, list_dfperyear, list_nk_lag){
  
  for(i in 1:length(list_nd_lag)){      # cycle through list_nd_lag
    for(j in 1:length(list_dfperyear)){ # cycle through list_dfperyear
      for(k in 1:length(list_nk_lag)){  # cycle through list_nk_lag
        
        nd_lag <<- list_nd_lag[i]
        dfperyear <<- list_dfperyear[j]
        nk_lag <<- list_nk_lag[k]
        
        
        # N_Admissions
        
        try(fun_run_dlnm(predictor="TMKp",
                         outcome="N_Admissions",
                         linkfamily=quasipoisson(),
                         linkfamilyname="qp"))
        
        # ### N_fatalAdmissions ###
        # 
        # try(fun_run_dlnm(predictor="TMKp",
        #                  outcome="N_fatalAdmissions",
        #                  linkfamily=quasipoisson(),
        #                  linkfamilyname="qp"))
        # 
        # ### N_fatalAdmissions_w3d ###
        # 
        # try(fun_run_dlnm(predictor="TMKp",
        #                  outcome="N_fatalAdmissions_w3d",
        #                  linkfamily=quasipoisson(),
        #                  linkfamilyname="qp"))
        # 
        # ### Hospitalcosts ###
        # 
        # try(fun_run_dlnm(predictor="TMKp",
        #                  outcome="Hospitalcosts_VPI",
        #                  linkfamily=gaussian(link="log"),
        #                  linkfamilyname="nl"))
        # 
        # # Casecosts_VPI
        # 
        # try(fun_run_dlnm(predictor="TMKp",
        #                  outcome="Casecosts_VPI",
        #                  linkfamily=gaussian(link="log"),
        #                  linkfamilyname="nl"))
        # 
        # # LOS_surv
        # 
        # try(fun_run_dlnm(predictor="TMKp",
        #                  outcome="LOS_surv",
        #                  linkfamily=gaussian(link="identity"),
        #                  linkfamilyname="ni"))
        # 
        # # Casemortality
        # 
        # try(fun_run_dlnm(predictor="TMKp",
        #                  outcome="Casemortality",
        #                  linkfamily=binomial(link="logit"),
        #                  linkfamilyname="bl"))
        
      }
    }
  }
}


#	-------------------------------------------------------------------------------------------------------------------
#	II.C EXECUTE STAGE 1 ANALYSIS WITH VARYING MODEL PARAMETERS 
# -------------------------------------------------------------------------------------------------------------------

### CALL FUNCTION TO EXECUTE ANALYSIS ###

fun_callmodels(list_nd_lag=list_nd_lag_in, list_dfperyear=list_dfperyear_in, list_nk_lag=list_nk_lag_in)

rm(nd_lag, dfperyear, nk_lag)
try(gc())


#	-------------------------------------------------------------------------------------------------------------------
#	ADDITIONAL STEP: CREATE CROSS-PREDICTIONS FOR STAGE 1 ANALYSIS OF FINAL STUDY RESULTS
# -------------------------------------------------------------------------------------------------------------------


cps_S1_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final <- llply(1:401, function(i){
  
  cb <- crossbasis(list_countydata[[i]]$TMKp,
                            argvar=list(knots=c(.10, .75, .90), fun="ns"),
                            arglag=list(knots=logknots(28,nk=2), fun="ns"),
                            lag=28)
  
  coefs_use <- coefs_S1_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final[i,]
  vcovs_use <- vcovs_S1_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final[[i]]

  crosspredictions <- crosspred(cb,coef=coefs_use, vcov=vcovs_use, model.link = "log", cen=.50, at=1:1000/1000)
  
  
  # ATTACH MODEL INFO TO CROSSPREDICTIONS
  crosspredictions$outcome <- "N_Admissions"
  crosspredictions$predictor <- "TMKp"
  crosspredictions$linkfamilyname <- "qp"
  crosspredictions$county_id <- unique(list_countydata[[i]]$county)
  crosspredictions$ktemp_string <- ".10, .75, .90"
  crosspredictions$nk_lag <- 2
  crosspredictions$nd_lag <- 28
  crosspredictions$dfperyear <- 8
  crosspredictions$label <- "N_Admissions_TMKp_nkl2_dfy8_28l_qp_final"
  
  return(crosspredictions)
})


