#	-------------------------------------------------------------------------------------------------------------------
# ----
#	----	ANALYSIS: EFFECTS OF EXTREME TEMPERATURE ON GERMAN EMERGENCY CARE
#	----
#	----  R_Script_05_Exploratory-Analysis-County-Level
#	----
#	-------------------------------------------------------------------------------------------------------------------

#   Author: 					          Jona Frasch 
#   E-Mail: 					          j.frasch@uke.de 
#   Tel.: 						          +49 40 7410 52676


#	-------------------------------------------------------------------------------------------------------------------
#	----
#	----	BLOCK V: EXPLORATORY ANALYSIS ON COUNTY LEVEL
#	----
#	-------------------------------------------------------------------------------------------------------------------

dpi_use <-300
textsize <- 20 # in mm
textsize_note <- 20 # in mm
textsize_margins <- 30

#----------------------------------------------------------------------------------------
# V.A INSPECT DISTRIBUTION OF OUTCOME 
#----------------------------------------------------------------------------------------

### Function to plot county-level distributions ###

comparedistributions <- function(variable){
  list_plots_out <- llply(list_county_id, function(county_id){
    data_use <- list_countydata[[county_id]] %>% 
      dplyr::select(all_of(variable)) %>% 
      dplyr::rename(variable=1)
    data_use_2 <- data_use %>% filter(!is.na(variable) & variable>0)
    data_use_3 <- data_use %>% filter(!is.na(variable) & variable>0) %>% mutate(variable=round(variable))

    # Define values for the distributions
    x_vals <- round(seq(min(data_use$variable, na.rm=T) - 10, max(data_use$variable, na.rm=T) + 10, length = length(data_use$variable)))

    # Parameters for Poisson distribution
    params_pois <- fitdistr(data_use_2$variable, "Poisson")
    lambda_pois <- params_pois$estimate[1]

    # Parameters for negative binomial distribution
    params_nbd <- fitdistr(data_use_3$variable, "negative binomial")
    size_nbd <- params_nbd$estimate[1]
    mu_nbd <- params_nbd$estimate[2]

    # Create a density plot for the variable
    plot_out <- data_use %>%
      ggplot(aes(x = variable)) +
      geom_density(aes(y = after_stat(density)), fill = "#E69F00", alpha = 0.5) +
      # Add Poisson distribution curve
      geom_line(aes(x = x_vals, y = dpois(floor(x_vals), lambda = lambda_pois)),
                color = "#CC79A7") +
      # Add the negative binomial distribution
      geom_line(aes(x = x_vals, y = dnbinom(x_vals, size=size_nbd, mu=mu_nbd, log = FALSE)),
                color = "#009E73") +
      theme(text = element_text(size = textsize, 
                                colour="black")
            ,axis.title.x=element_blank()
            # ,axis.text.x=element_blank()
            ,axis.title.y=element_blank()
            # ,axis.text.y=element_blank()
            )

    ggplotGrob(plot_out)
    
  })
  
  names(list_plots_out) <- list_county_id

  assign(paste0("list_plots_distribution_", variable), list_plots_out, envir=.GlobalEnv)
}

# Call Function 
comparedistributions("N_Admissions")


list_plotS_distribution_N_Admissions <- marrangeGrob(list_plots_distribution_N_Admissions[county_order_revers_by_population],
                                  nrow=10,
                                  ncol=5,
                                  as.table=FALSE,
                                  top=textGrob("County Specifc Distribution of the Number of Emergency Admissions", vjust = 0.5, gp = gpar(col = "black", fontsize = textsize_margins, fontface="bold")),
                                  left=textGrob("Density", rot = 90, vjust = 0.5, gp = gpar(col = "black", fontsize = textsize_margins)),
                                  right=textGrob("violet: expected poisson distribution; green: expected negative binomial distribution; counties sorted in descending sequence by population size", rot = 270, vjust = 0.5, gp = gpar(col = "black", fontsize = textsize_margins)),
                                  bottom=textGrob("Number of Emergency Admission per County per Day", hjust = 0.4, gp = gpar(col = "black", fontsize = textsize_margins)), bycol=TRUE)


ggsave("Output/Distribution/plots_distribution_N_Admissions_%02d.jpg", list_plotS_distribution_N_Admissions,
       width=210, height=297, units="mm", dpi=dpi_use)


#	-------------------------------------------------------------------------------------------------------------------
#	V.B INSPECTION OF SEASONAL TREND IN EMERGENCY ADMISSIONS 
# -------------------------------------------------------------------------------------------------------------------

index_of_dispersion <- datain %>% dplyr::group_by(county) %>% 
  dplyr::summarize(sd_N_Admissions=sd(N_Admissions, na.rm=T), var_N_Admissions=var(N_Admissions, na.rm=T), mean_N_Admissions=mean(N_Admissions, na.rm=T)) %>%
  mutate(CV=sd_N_Admissions/mean_N_Admissions, index_of_disperion=var_N_Admissions/mean_N_Admissions) %>% 
  left_join(data_counties) %>% relocate(county, county_name, EWZ, index_of_disperion) %>% dplyr::select(-c("X", "Y", "AGS_uni")) %>%
  arrange(desc(index_of_disperion))

index_of_dispersion_dow_stratified <- datain %>% dplyr::group_by(county, dow) %>% 
  dplyr::summarize(sd_N_Admissions=sd(N_Admissions, na.rm=T), var_N_Admissions=var(N_Admissions, na.rm=T), mean_N_Admissions=mean(N_Admissions, na.rm=T)) %>%
  mutate(CV=sd_N_Admissions/mean_N_Admissions, index_of_disperion=var_N_Admissions/mean_N_Admissions) %>% left_join(data_counties) %>% 
  relocate(county, county_name, EWZ, index_of_disperion) %>% dplyr::select(-c("X", "Y", "AGS_uni")) %>%
  arrange(desc(index_of_disperion))

index_of_dispersion
index_of_dispersion_dow_stratified

save(index_of_dispersion, index_of_dispersion_dow_stratified, file="Output/index_of_dispersion.Rdata")


#	-------------------------------------------------------------------------------------------------------------------
#	V.C INSPECTION OF SEASONAL TREND IN EMERGENCY ADMISSIONS 
# -------------------------------------------------------------------------------------------------------------------

data_for_seasonal_analysis <- datain %>%
  mutate(Date=as.Date(Date, "%Y-%m-%d")) %>% 
  dplyr::group_by(Date) %>% 
  dplyr::summarize(N_Admissions=sum(N_Admissions, na.rm=T), 
            Date=first(Date),
            time=first(time)) 

for(dfperyear in 7:10){
  
  glm_season_N_Admissions <- glm(N_Admissions ~  ns(time, dfperyear*10),
                                 family=quasipoisson(),
                                 data_for_seasonal_analysis)
  
  data_for_seasonal_analysis$predict_N_Admissions <- glm_season_N_Admissions$fitted.values

  data_for_seasonal_analysis_weekly <- data_for_seasonal_analysis %>%
    mutate(week=week(Date), year=year(Date)) %>%
    group_by(year, week) %>% 
    dplyr::summarise(across(c("N_Admissions", "predict_N_Admissions"), \(x) sum(x, na.rm=T)), time=first(time), Date=first(Date)) 
  data_for_seasonal_analysis_weekly
  
  plot_N_Admissions_weekly <- ggplot(data=data_for_seasonal_analysis_weekly, aes(x=Date, y=N_Admissions)) + 
    geom_line() + 
    geom_line(aes(y=predict_N_Admissions), color="red") + 
    ylab(NULL) + 
    ggtitle(paste(dfperyear, " DF in the seasonal control")) + 
    theme_classic() +
    theme(text = element_text(size = textsize*0.7, 
                              colour="black"))
  
  assign(paste0("plot_N_Admissions_weekly_", df=dfperyear), plot_N_Admissions_weekly, envir=.GlobalEnv)
  
}


Time_Series_Seasonal_Trend_N_Admissions <- grid.arrange(arrangeGrob(plot_N_Admissions_weekly_7, 
                                                       plot_N_Admissions_weekly_8,
                                                       plot_N_Admissions_weekly_9,
                                                       plot_N_Admissions_weekly_10,
                                                       nrow = 4), left=textGrob("Number of Emergency Admissions (aggregated across county and week)", rot = 90, vjust = 0.5, gp = gpar(col = "black", fontsize = textsize)))

ggsave("Output/Plot_Time_Series_Seasonal_Trend_N_Admission.jpg", Time_Series_Seasonal_Trend_N_Admissions,
       width=210, height=297, units="mm", dpi=dpi_use)



#	-------------------------------------------------------------------------------------------------------------------
#	V.D PLOTS OF STAGE 1 RESULTS  
# -------------------------------------------------------------------------------------------------------------------

source("Functions/function_S1_plots.R")

# use_pattern <- "cps_S1_"
use_pattern <- "cps_S1_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final"

l_ply(ls(pattern=use_pattern), function(cps_S1_label){
  
  list_crosspredictions <- get(cps_S1_label, envir=.GlobalEnv)
  
  outcome_label <- outcome <- list_crosspredictions[[1]]$outcome
  predictor <- predictor_label <- list_crosspredictions[[1]]$predictor
  nd_lag <- list_crosspredictions[[1]]$nd_lag
  nk_lag <- list_crosspredictions[[1]]$nk_lag
  label <- list_crosspredictions[[1]]$label
  
  uselims <- ldply(list_crosspredictions, function(cp_interm){

    if(cp_interm$model.link=="identity"){
      minoverall <- min(cp_interm$allfit, na.rm=T)-0.05*sd(cp_interm$allfit, na.rm=T)
      maxoverall <- max(cp_interm$allfit, na.rm=T)+0.05*sd(cp_interm$allfit, na.rm=T)
      minslices <- min(cp_interm$matfit, na.rm=T)-0.05*sd(cp_interm$matfit, na.rm=T)
      maxslices <- max(cp_interm$matfit, na.rm=T)+0.05*sd(cp_interm$matfit, na.rm=T)
    } else {
      minoverall <- min(cp_interm$allRRfit, na.rm=T)-0.05*sd(cp_interm$allRRfit, na.rm=T)
      maxoverall <- max(cp_interm$allRRfit, na.rm=T)+0.05*sd(cp_interm$allRRfit, na.rm=T)
      minslices <- min(cp_interm$matRRfit, na.rm=T)-0.05*sd(cp_interm$matRRfit, na.rm=T)
      maxslices <- max(cp_interm$matRRfit, na.rm=T)+0.05*sd(cp_interm$matRRfit, na.rm=T)
    }
    
    c(minoverall, maxoverall, minslices, maxslices)
  }) %>% summarize(minoverall=min(V1),
                   maxoverall=max(V2),
                   minslices=min(V3),
                   maxslices=max(V4))
  
  # OVERALL EFFECT PLOT #
  
  fun_create_plots3("overall", uselims$minoverall, uselims$maxoverall, list_crosspredictions)
  
  overall_plots <- get(paste("list_ggp_overall", label, sep="_"), envir=.GlobalEnv)
  
  list_plots_overall <- marrangeGrob(overall_plots[county_order_revers_by_population]                                     ,
                                     nrow=10,
                                     ncol=5,
                                     as.table=FALSE,
                                     left=textGrob(outcome_label, rot = 90, vjust = 0.5, gp = gpar(col = "black", fontsize = textsize_margins)),
                                     bottom=textGrob("Percentile of Mean Temperature", hjust = 0.4, gp = gpar(col = "black", fontsize = textsize_margins)),
                                     top = NULL)
  
  ggsave(paste("Output/Stage_1/Plots_S1_overall", label, "%02d.jpg", sep="_"), list_plots_overall,
         width=210, height=297, units="mm", dpi=dpi_use)
  
  # SLICES PLOT #
  fun_create_plots3("slices", uselims$minslices, uselims$maxslices, list_crosspredictions)
  
  slices_plots <- get(paste("list_ggp_slices", label, sep="_"), envir=.GlobalEnv)
  
  list_plots_slices <- marrangeGrob(slices_plots[county_order_revers_by_population]                                    ,
                                    nrow=10,
                                    ncol=5,
                                    as.table=FALSE,
                                    left=textGrob(outcome_label, rot = 90, vjust = 0.5, gp = gpar(col = "black", fontsize = textsize_margins)),
                                    bottom=textGrob("Percentile of Mean Temperature", hjust = 0.4, gp = gpar(col = "black", fontsize = textsize_margins)),
                                    top = NULL)
  
  ggsave(paste("Output/Stage_1/Plots_S1_slices", label, "%02d.jpg", sep="_"), list_plots_slices,
         width=210, height=297, units="mm", dpi=dpi_use)
  
  # CONTOUR EFFECT PLOT #
  fun_create_plots3("contour", NA, NA, list_crosspredictions)
  
  contour_plots <- get(paste("list_ggp_contour", label, sep="_"), envir=.GlobalEnv)
  
  list_plots_contour <- marrangeGrob(contour_plots[county_order_revers_by_population]                                     ,
                                     nrow=8,
                                     ncol=4,
                                     as.table=FALSE,
                                     left=textGrob("Percentile of Mean Temperature", rot = 90, vjust = 0.5, gp = gpar(col = "black", fontsize = textsize_margins)),
                                     right=textGrob(outcome_label, rot = 270, vjust = 0.5, gp = gpar(col = "black", fontsize = textsize_margins)),
                                     bottom=textGrob("Lag in Days", hjust = 0.4, gp = gpar(col = "black", fontsize = textsize_margins)),
                                     top = NULL)
  ggsave(paste("Output/Stage_1/Plots_S1_contour", label, "%02d.jpg", sep="_"), list_plots_contour,
         width=210, height=297, units="mm", dpi=dpi_use)
  
  

  
  assign(paste("Plots_S1_overall", label, sep="_"), list_plots_overall, envir=.GlobalEnv)
  assign(paste("Plots_S1_contour", label, sep="_"), list_plots_contour, envir=.GlobalEnv)
  assign(paste("Plots_S1_slices", label, sep="_"), list_plots_slices, envir=.GlobalEnv)
  })

