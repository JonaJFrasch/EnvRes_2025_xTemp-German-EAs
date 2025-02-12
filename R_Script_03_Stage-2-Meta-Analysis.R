#	-------------------------------------------------------------------------------------------------------------------
# ----
#	----	ANALYSIS: EFFECTS OF EXTREME TEMPERATURE ON GERMAN EMERGENCY CARE
#	----
#	----  R_Script_03_Stage-2-Meta-Analysis
#	----
#	-------------------------------------------------------------------------------------------------------------------

#   Author: 					          Jona Frasch 
#   E-Mail: 					          j.frasch@uke.de 
#   Tel.: 						          +49 40 7410 52676

#	-------------------------------------------------------------------------------------------------------------------
#	----
#	----	Block III: STAGE 2 ANALYSIS
#	----
#	-------------------------------------------------------------------------------------------------------------------

#	-------------------------------------------------------------------------------------------------------------------
#	III.A SET UP STAGE-2 ANALYSIS 
# -------------------------------------------------------------------------------------------------------------------

### LIST PARAMETERS OF ALL MODEL PARAMETRIZATIONS RUN

all_model_parameters <- ldply(ls(pattern="model_parameters_S1"), function(table_label){
  get(table_label, envir=.GlobalEnv)
}) %>%  
  group_by(outcome, predictor, linkfamilyname, ktemp, nk_lag, nd_lag, dfperyear, fitstat, DF1, DF2, label) %>%
  summarize(outcome=first(outcome),
            predictor=first(predictor),
            linkfamilyname=first(linkfamilyname),
            ktemp=first(ktemp),
            nk_lag=first(as.numeric(nk_lag)),
            nd_lag=first(as.numeric(nd_lag)),
            dfperyear=first(as.numeric(dfperyear)),
            fitstat=first(fitstat),
            DF1=first(as.numeric(DF1)),
            DF2=first(as.numeric(DF2)),
            fit=sum(as.numeric(fit)),
            label=first(label))


### ADDITIONLA STEP TO REPRODUCE FINAL STUDY RESULTS: ADD LAYER TO all_model_parameters FOR FINAL S1 STUDY-RESULTS
all_model_parameters <- rbind(all_model_parameters, all_model_parameters)
all_model_parameters[2, "label"] <- paste0(all_model_parameters[1, "label"], "_final")

all_model_parameters


#	-------------------------------------------------------------------------------------------------------------------
#	III.B LOOP FOR STAGE 2 ANALYSIS ACROSS ALL MODEL PARAMETRIZATIONS 
# -------------------------------------------------------------------------------------------------------------------

for(i in 1:nrow(all_model_parameters)){
  
  use_model_parameters <- all_model_parameters[i,]
  
  label <- use_model_parameters$label
  
  if(use_model_parameters$linkfamilyname=="qp"){linkfamily <- quasipoisson()}
  if(use_model_parameters$linkfamilyname=="ni"){linkfamily <- gaussian(link="identity")}
  if(use_model_parameters$linkfamilyname=="nl"){linkfamily <- gaussian(link="log")}
  if(use_model_parameters$linkfamilyname=="bl"){linkfamily <- binomial(link="logit")}
  
  ### GET MODEL PARAMETERS ###
  ktemp <- as.numeric(strsplit(use_model_parameters$ktemp, " ")[[1]])
  nk_lag <- use_model_parameters$nk_lag
  nd_lag <- use_model_parameters$nd_lag
  klag <- logknots(nd_lag,nk=nk_lag)
  dfperyear <- use_model_parameters$dfperyear
  
  temp_perc <- seq(0, 1,length=nd_lag+1)
  
  # DEFINING THE CROSS-BASIS FUNCTION FOR THE 2ND-STAGE ANALYSIS
  cb <- crossbasis(temp_perc,
                   argvar=list(knots=ktemp, fun="ns"),
                   arglag=list(fun="ns", knots=klag),
                   lag=nd_lag)
  
  #----------------------------------------------------------------------------------------
  # REDUCED ESTIMATES - OVERALL EFFECT
  #----------------------------------------------------------------------------------------
  
  ### GET ESTIMATES ###
  
  coefs_S1 <- get(paste("coefs_S1", label, sep="_"), envir=.GlobalEnv)
  vcovs_S1 <- get(paste("vcovs_S1", label, sep="_"), envir=.GlobalEnv)      
  
  list_county_id_use <- rownames(coefs_S1)
  names(vcovs_S1) <- list_county_id_use
  
  # REDUCE ESTIMATES TO OVERALL-EFFECT FOR MORE EFFICIENT META ANALYSIS
  list_reduced_overall <- llply(list_county_id_use, function(county_id){
    
    vcov_county <- vcovs_S1[[county_id]]
    coefs_county <- coefs_S1[county_id, ]
    
    reduced_overall <- crossreduce(basis=cb, type="overall", coef=coefs_county, vcov=vcov_county, model.link=linkfamily[[2]], cen=.5)
    
    return(reduced_overall)
    
  })
  
  # CREATE COEF-TABLE
  
  coefs_S1_overall <- ldply(list_reduced_overall, function(reduced_estimates){
    coef(reduced_estimates)
  })
  
  coefs_S1_overall <- as.matrix(coefs_S1_overall)
  
  # CREATE VCOV-TABLE
  
  vcovs_S1_overall <- llply(list_reduced_overall, function(reduced_estimates){
    as.matrix(vcov(reduced_estimates))
  })
  
  names(vcovs_S1_overall) <- list_county_id_use
  
  #----------------------------------------------------------------------------------------
  # STAGE 2 ANALYSIS
  #----------------------------------------------------------------------------------------
  
  ### OVERALL EFFECT ---------------------------------------------------------------------
  
  meta_overall_fixed <- mixmeta(coefs_S1_overall, vcovs_S1_overall, method="fixed", control=list(showiter=F))
  meta_overall_random <- mixmeta(coefs_S1_overall, vcovs_S1_overall, method="reml", control=list(showiter=F), data=data_counties, random=~1|county)
  
  ### GET CENTERING VALUE AT MINIMUM-OVERALL-RISK-TEMPERATURE BETWEEN 50th AND 99th PERCENTILE ---------------------------------------------------------------------
  
  predvar <- 50:99/100
  bvar <- do.call("onebasis",c(list(x=predvar),attr(cb,"argvar")))
  
  min_interm <- ((predvar)[which.min((bvar%*%coef(meta_overall_random)))])
  
  
  if(min_interm == .99){
    
    centering_value <- .50 # SET TO 50th PERCENTILE IF MINIMUM FALLS ONTO THE EXTREME
    
  } else {
    
    centering_value <- min_interm
    
  }
  
  ### MODEL EXTREME TEMPERATURES ONLY ---------------------------------------------------------------------
  
  list_reduced_heat_cold <- llply(list_county_id_use, function(county_id){
    
    coefs_county <- coefs_S1[county_id, ]
    vcov_county <- as.matrix(vcovs_S1[[county_id]])
    
    # DEFINING THE CROSS-BASIS FUNCTION FOR THE 2ND-STAGE ANALYSIS
    cb <- crossbasis(temp_perc,
                     argvar=list(knots=ktemp, fun="ns"),
                     arglag=list(fun="ns", knots=klag),
                     lag=nd_lag)
    
    reduced_heat <- crossreduce(basis=cb, type="var", value=0.99, coef=coefs_county, vcov=vcov_county, model.link=linkfamily[[2]], cen=centering_value)
    reduced_cold <- crossreduce(basis=cb, type="var",value=0.01, coef=coefs_county, vcov=vcov_county, model.link=linkfamily[[2]], cen=centering_value)
    
    list_reduced_out <- list(reduced_heat, reduced_cold)
    names(list_reduced_out) <- c("reduced_heat", "reduced_cold")
    return(list_reduced_out)
    
  })
  
  coefs_S1_heat <- ldply(list_reduced_heat_cold, function(reduced_estimates){
    coef(reduced_estimates$reduced_heat)
  })
  coefs_S1_heat <- as.matrix(coefs_S1_heat)
  
  coefs_S1_cold <- ldply(list_reduced_heat_cold, function(reduced_estimates){
    coef(reduced_estimates$reduced_cold)
  })
  coefs_S1_cold <- as.matrix(coefs_S1_cold)
  
  vcovs_S1_heat <- llply(list_reduced_heat_cold, function(reduced_estimates){
    as.matrix(vcov(reduced_estimates$reduced_heat))
  })
  
  vcovs_S1_cold <- llply(list_reduced_heat_cold, function(reduced_estimates){
    as.matrix(vcov(reduced_estimates$reduced_cold))
  })
  
  
  ### HEAT & COLD EFFECT ---------------------------------------------------------------------------------------
  
  meta_heat_fixed <- mixmeta(coefs_S1_heat, vcovs_S1_heat, method="fixed", control=list(showiter=F))
  meta_cold_fixed <- mixmeta(coefs_S1_cold, vcovs_S1_cold, method="fixed", control=list(showiter=F))
  
  meta_heat_random <- mixmeta(coefs_S1_heat, vcovs_S1_heat, method="reml", control=list(showiter=F), data=data_counties, random=~1|county)
  meta_cold_random <- mixmeta(coefs_S1_cold, vcovs_S1_cold, method="reml", control=list(showiter=F), data=data_counties, random=~1|county)
  
  
  ### CROSS-PREDICTIONS ---------------------------------------------------------------------------------------
  
  # REDEFINE THE FUNCTION USING ALL THE ARGUMENTS (BOUNDARY KNOTS INCLUDED)
  
  xvar <- seq(0,1,by=0.001)
  bvar <- do.call("onebasis",c(list(x=xvar),attr(cb,"argvar")))
  
  xlag <- 0:nd_lag
  blag <- do.call("onebasis",c(list(x=xlag),attr(cb,"arglag")))
  
  
  # CROSS-PREDICTIONS FIXED-EFFECTS META ANALYSIS
  cp.meta_overall_fixed <- crosspred(bvar,coef=coef(meta_overall_fixed),vcov=vcov(meta_overall_fixed),model.link=linkfamily[[2]], at=c(xvar), cen=centering_value)
  cp.meta_heat_fixed <- crosspred(blag,coef=coef(meta_heat_fixed),vcov=vcov(meta_heat_fixed),model.link=linkfamily[[2]], at=c(xlag), cen=centering_value)
  cp.meta_cold_fixed <- crosspred(blag,coef=coef(meta_cold_fixed),vcov=vcov(meta_cold_fixed),model.link=linkfamily[[2]], at=c(xlag), cen=centering_value)
  
  # CROSS-PREDICTIONS RANDOM-EFFECTS META ANALYSIS
  cp.meta_overall_random <- crosspred(bvar,coef=coef(meta_overall_random),vcov=vcov(meta_overall_random),model.link=linkfamily[[2]], at=c(xvar), cen=centering_value)
  cp.meta_heat_random <- crosspred(blag,coef=coef(meta_heat_random),vcov=vcov(meta_heat_random),model.link=linkfamily[[2]], at=c(xlag), cen=centering_value)
  cp.meta_cold_random <- crosspred(blag,coef=coef(meta_cold_random),vcov=vcov(meta_cold_random),model.link=linkfamily[[2]], at=c(xlag), cen=centering_value)
  
  cp.meta_overall_fixed$cen <- cp.meta_heat_fixed$cen <- cp.meta_cold_fixed$cen <-
    cp.meta_overall_random$cen <- cp.meta_heat_random$cen <- cp.meta_cold_random$cen <- centering_value
  
  
  # #----------------------------------------------------------------------------------------
  # # COMPLETE META ANALYSIS
  # #----------------------------------------------------------------------------------------
  # 
  # meta_complete_fixed <- mixmeta(coefs_use, vcovs_use, method="fixed", control=list(showiter=F))
  # meta_complete_random <- mixmeta(coefs_use, vcovs_use, method="reml", control=list(showiter=F), data=data_counties, random=~1|county)
  # 
  # # DEFINING THE CROSS-BASIS FUNCTION FOR THE 2ND-STAGE ANALYSIS
  # cb <- crossbasis(temp_perc,
  #                  argvar=list(knots=ktemp, fun="ns"),
  #                  arglag=list(fun="ns", knots=klag),
  #                  lag=nd_lag)
  # 
  # cp.meta_complete_fixed <- crosspred(cb,coef=coef(meta_complete_fixed),vcov=vcov(meta_complete_fixed),model.link=linkfamily[[2]],at=c(xvar), cen=centering_value)
  # cp.meta_complete_random <- crosspred(cb,coef=coef(meta_complete_random),vcov=vcov(meta_complete_random),model.link=linkfamily[[2]],at=c(xvar), cen=centering_value)
  
  #----------------------------------------------------------------------------------------
  # OUTPUT
  #----------------------------------------------------------------------------------------
  
  assign(paste("meta_overall_fixed", label, sep="_"), meta_overall_fixed, envir=.GlobalEnv)
  assign(paste("meta_heat_fixed", label, sep="_"), meta_heat_fixed, envir=.GlobalEnv)
  assign(paste("meta_cold_fixed", label, sep="_"), meta_cold_fixed, envir=.GlobalEnv)
  
  assign(paste("meta_overall_random", label, sep="_"), meta_overall_random, envir=.GlobalEnv)
  assign(paste("meta_heat_random", label, sep="_"), meta_heat_random, envir=.GlobalEnv)
  assign(paste("meta_cold_random", label, sep="_"), meta_cold_random, envir=.GlobalEnv)
  
  assign(paste("cp.meta_overall_fixed", label, sep="_"), cp.meta_overall_fixed, envir=.GlobalEnv)
  assign(paste("cp.meta_cold_fixed", label, sep="_"), cp.meta_cold_fixed, envir=.GlobalEnv)
  assign(paste("cp.meta_heat_fixed", label, sep="_"), cp.meta_heat_fixed, envir=.GlobalEnv)
  
  assign(paste("cp.meta_overall_random", label, sep="_"), cp.meta_overall_random, envir=.GlobalEnv)
  assign(paste("cp.meta_cold_random", label, sep="_"), cp.meta_cold_random, envir=.GlobalEnv)
  assign(paste("cp.meta_heat_random", label, sep="_"), cp.meta_heat_random, envir=.GlobalEnv)
  
  # assign(paste("meta_complete_fixed", label, sep="_"), meta_complete_fixed, envir=.GlobalEnv)
  # assign(paste("meta_complete_random", label, sep="_"), meta_complete_random, envir=.GlobalEnv)
  
  # assign(paste("cp.meta_complete_fixed", label, sep="_"), cp.meta_complete_fixed, envir=.GlobalEnv)
  # assign(paste("cp.meta_complete_random", label, sep="_"), cp.meta_complete_random, envir=.GlobalEnv)
  
}