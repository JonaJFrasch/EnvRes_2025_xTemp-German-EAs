
#----------------------------------------------------------------------------------------
# COUNTY SPECIFIC PLOTS
#----------------------------------------------------------------------------------------




# ------ OVERALL EFFECT ----------------------------------------------------------------------------------------

plot_overall_gg2 <- function(crosspredictions, title_predictor_axis, breaks_predictor_axis, labels_predictor_axis, title_outcome_axis, output_label, y_limits){
  
  plot_title <- paste("County:", crosspredictions$county_id)
  
  
  ### DATA ###
  
  if(crosspredictions$model.link[1]=="identity"){
    usedata_fit <- as.data.frame(crosspredictions$allfit) %>% dplyr::rename(estimate=1) %>% rownames_to_column(var="predictor") %>% mutate(predictor=as.numeric(predictor))
    usedata_high <- as.data.frame(crosspredictions$allhigh) %>% dplyr::rename(high=1) %>% rownames_to_column(var="predictor") %>% mutate(predictor=as.numeric(predictor))
    usedata_low <- as.data.frame(crosspredictions$alllow) %>% dplyr::rename(low=1) %>% rownames_to_column(var="predictor") %>% mutate(predictor=as.numeric(predictor))
    center <- 0
    
  } else {
    usedata_fit <- as.data.frame(crosspredictions$allRRfit) %>% dplyr::rename(estimate=1) %>% rownames_to_column(var="predictor") %>% mutate(predictor=as.numeric(predictor))
    usedata_high <- as.data.frame(crosspredictions$allRRhigh) %>% dplyr::rename(high=1) %>% rownames_to_column(var="predictor") %>% mutate(predictor=as.numeric(predictor))
    usedata_low <- as.data.frame(crosspredictions$allRRlow) %>% dplyr::rename(low=1) %>% rownames_to_column(var="predictor") %>% mutate(predictor=as.numeric(predictor))
    center <- 1
  }
  
  plotdata <- usedata_fit %>% left_join(usedata_high) %>% left_join(usedata_low) 
  
  
  ### PLOT ###
  plot_overall_out <- ggplot(data=plotdata, aes(x=predictor, y=estimate)) +
    geom_ribbon(aes(ymin=low, ymax=high), fill="black", alpha=.2) +
    geom_line(colour="black", linewidth=.75) +
    geom_hline(yintercept=center) +
    scale_x_continuous(breaks=breaks_predictor_axis,
                       labels=labels_predictor_axis,
                       name=title_predictor_axis) +
    scale_y_continuous(name=title_outcome_axis) +
    theme_classic() +
    theme(text = element_text(size = textsize, 
                              colour="black"),
          # axis.text.x=element_text(vjust=0.5, hjust=0),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          margin=unit(c(0,0,0,0), "mm")) +
    coord_cartesian(ylim=y_limits) +
    annotate("text", x = Inf, y = Inf, label = plot_title, vjust = 2, hjust = 1, size=textsize_note/.pt)
  
  # assign(paste("ggp_overall", output_label, sep="_"), plot_overall_out, envir=.GlobalEnv)
  print(plot_overall_out)
  return(plot_overall_out)
  
}


# ------ CONTOUR PLOTS ----------------------------------------------------------------------------------------

plot_contour_gg2 <- function(crosspredictions, title_predictor_axis, breaks_predictor_axis, labels_predictor_axis, title_outcome_axis, output_label, y_limits){
  
  plot_title <- paste("County:", crosspredictions$county_id)
  
  ### DATA ###
  
  if(crosspredictions$model.link[1]=="identity"){
    usedata <- crosspredictions$matfit
    center <- 0
  } else {
    usedata <- crosspredictions$matRRfit
    center <- 1
  }
  
  plotdata <- as.data.frame(usedata) %>%
    mutate(predictor=as.numeric(rownames(usedata))) %>%
    pivot_longer(cols=starts_with("lag"), names_to="lag", values_to="estimate") %>%
    mutate(lag=as.numeric(str_remove(lag, "lag")))
  
  
  ### SETUP ###
  
  # Scale
  max_distance_to_center <- max(abs(center-range(plotdata$estimate)))
  
  usedistance <- (ceiling((max_distance_to_center*1000) / 4.5) * 4.5)/1000
  
  ll <- center-usedistance
  ul <- center+usedistance
  
  # Scale-labels
  usebreaks <- seq(ll, ul, length.out=19)
  labelbreaks <- rep("", length(usebreaks))
  for(i in c(2,6,10,14,18)){
    labelbreaks[i] <- paste(round(usebreaks[i], digits=2))
  }
  usebreaks <- rev(usebreaks)
  labelbreaks <- labelbreaks
  
  # Colour-pallet
  upper_pallet <- brewer.pal(n=9, "RdPu")
  lower_pallet <- rev(brewer.pal(n=9, "Greens"))
  usepallet <- c(lower_pallet, upper_pallet)
  usepallet[c(length(lower_pallet), length(lower_pallet)+1)] <- "#FFFFFF"
  
  ### PLOT ###
  
  plot_contour_out <- ggplot(data=plotdata, aes(x=lag, y=predictor, z=estimate)) +
    geom_contour_filled(breaks=usebreaks, 
                        aes(fill = after_stat(level_mid))) +
    scale_fill_stepsn(colours=usepallet,
                      breaks=usebreaks,
                      name=title_outcome_axis,
                      limits=c(ll, ul),
                      labels=labelbreaks) +
    scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25, 28),
                       name="Lag in Days") +
    scale_y_continuous(breaks=breaks_predictor_axis,
                       labels=labels_predictor_axis) +
    geom_hline(yintercept = .75, colour="lightgrey") +
    theme_classic() +
    theme(text = element_text(size = textsize, 
                              colour="black"),
          legend.title = element_blank(),
          legend.justification="center",
          # legend.position = "bottom",
          # legend.title.position="top",
          # legend.justification="center",
          legend.key.width =  unit(7, 'pt'),
          legend.key.height = unit(12, 'pt'),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    annotate("text", x = Inf, y = Inf, label = plot_title, vjust = 2, hjust = 1, size=textsize_note/.pt)
  # guides(fill = guide_legend(override.aes = list(pattern = c(rep("none", length.out=9), rep("circle", length.out=10))))) 
  
  
  # assign(paste("ggp_contour", output_label, sep="_"), plot_contour_out, envir=.GlobalEnv)
  print(plot_contour_out)
  return(plot_contour_out)
  
}



# ------ SLICES PLOT ----------------------------------------------------------------------------------------

plot_slices_gg2 <- function(crosspredictions, title_predictor_axis, breaks_predictor_axis, labels_predictor_axis, title_outcome_axis, output_label, y_limits){
  
  plot_title <- paste("County:", crosspredictions$county_id)
  
  
  ### DATA ###
  
  if(crosspredictions$model.link[1]=="identity"){
    usedata_fit <- as.data.frame(crosspredictions$matfit) 
    usedata_high <- as.data.frame(crosspredictions$mathigh)
    usedata_low <- as.data.frame(crosspredictions$matlow) 
    center <- 0
    
  } else {
    usedata_fit <- as.data.frame(crosspredictions$matRRfit) 
    usedata_high <- as.data.frame(crosspredictions$matRRhigh) 
    usedata_low <- as.data.frame(crosspredictions$matRRlow) 
    center <- 1
  }
  
  
  usedata_fit <- usedata_fit %>% rownames_to_column(var="predictor") %>% 
    mutate(predictor=round(as.numeric(predictor), digits=3)) %>% 
    filter(predictor %in% c(0.01, 0.05, 0.95, 0.99)) %>% 
    pivot_longer(cols=contains("lag"), 
                 names_to="lag",
                 values_to="estimate") %>% 
    mutate(lag=as.numeric(str_remove(lag, "lag")),
           percentile=paste0("pctl", predictor*100)) #%>%
  # pivot_wider(id_cols=lag, names_from=predictor)
  
  usedata_high <- usedata_high %>% rownames_to_column(var="predictor") %>% 
    mutate(predictor=round(as.numeric(predictor), digits=3)) %>% 
    filter(predictor %in% c(0.01, 0.05, 0.95, 0.99)) %>% 
    pivot_longer(cols=contains("lag"), 
                 names_to="lag",
                 values_to="ul") %>% 
    mutate(lag=as.numeric(str_remove(lag, "lag")),
           percentile=paste0("pctl", predictor*100)) #%>%
  # pivot_wider(id_cols=lag, names_from=predictor)
  
  usedata_low <- usedata_low %>% rownames_to_column(var="predictor") %>% 
    mutate(predictor=round(as.numeric(predictor), digits=3)) %>% 
    filter(predictor %in% c(0.01, 0.05, 0.95, 0.99)) %>% 
    pivot_longer(cols=contains("lag"), 
                 names_to="lag",
                 values_to="ll") %>% 
    mutate(lag=as.numeric(str_remove(lag, "lag")),
           percentile=paste0("pctl", predictor*100)) 
  # pivot_wider(id_cols=lag, names_from=predictor)
  
  plot_title <- paste("County:", crosspredictions$county_id)
  
  
  plotdata <- usedata_fit %>% 
    left_join(usedata_high) %>% 
    left_join(usedata_low) %>%
    filter(percentile %in% c("pctl1", "pctl99"))
  
  
  ### PLOT ###
  plot_slices_out <-
    ggplot(data=plotdata, aes(x=lag)) +
    geom_ribbon(aes(ymin=ll, ymax=ul, fill=percentile), alpha=.2) +
    geom_line(aes(y=estimate, colour=percentile, linetype=percentile), linewidth=.75) +
    geom_hline(yintercept=center) +
    scale_x_continuous(breaks=c(0, 5, 10, 15, 20, 25, 28),
                       name="Lag in Days") +
    scale_y_continuous(name=title_outcome_axis) +
    scale_color_manual(name=title_predictor_axis,
                       labels=c("1st", "99th"),
                       values=c("#2171B5", "#D94801"),
                       aesthetics=c("color", "fill")) +
    scale_linetype_manual(name=title_predictor_axis,
                          labels=c("1st", "99th"),
                          values=c("dotted", "longdash")) +
    theme_classic() +
    theme(text = element_text(size = textsize, 
      colour="black"),
          legend.position = "none",
          legend.position.inside = c(1, .25),
          legend.direction="vertical",
          legend.justification = "right",
          legend.text.position = "left",
          legend.box.just="right",
          plot.title=element_text(hjust=.5, face="bold"),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) +
    coord_cartesian(ylim=y_limits) +
    annotate("text", x = Inf, y = Inf, label = plot_title, vjust = 2, hjust = 1, size=textsize_note/.pt)
  
  # assign(paste("ggp_slices", output_label, sep="_"), plot_slices_out, envir=.GlobalEnv)
  print(plot_slices_out)
  
  return(plot_slices_out)
}

#----------------------------------------------------------------------------------------
# FUNCTION TO CREATE ALL PLOTS
#----------------------------------------------------------------------------------------


fun_create_plots3 <- function(plottype, y_ll, y_ul, list_crosspredictions){
  
  list_plots_out <- llply(list_crosspredictions, function(crosspredictions){

    print(crosspredictions$label)

    ### PREDICTOR ###
    
    predictor <- predictor_label <- crosspredictions$predictor
    outcome <- outcome_label <- crosspredictions$outcome

    y_limits <- c(y_ll, y_ul)
    
    # SETTING PREDICOTR AXIS LABELS
    breaks_predictor_axis <- c(.01, 0.25, 0.50, 0.75, .99)
    labels_predictor_axis <- c("1st","25th", "50th", "75th", "99th")
    
    
    # SET UP PREDICTOR-AXIS TITLE
      title_predictor_axis <- "Percentile of Mean Temperature"
    
    # SET UP OUTCOME-AXIS TITLE
    if(crosspredictions$model.link[1]=="identity"){
      title_outcome_axis <- outcome_label
    } else {
      title_outcome_axis <- paste("RR", outcome_label)
    }
    
    
    ### OUTPUT ###
    
    # SET OUTPUT LABEL
      output_label <- crosspredictions$label
      
    
    ############
    ### PLOT ###
    
    call_plot <- call(paste0("plot_", plottype, "_gg2"),
                      crosspredictions, title_predictor_axis, breaks_predictor_axis, labels_predictor_axis, title_outcome_axis, output_label, y_limits)

    
    # DISPLAY PLOT
    plot_out <- eval(call_plot)
    
    return(plot_out)
    # plot_recorded <- recordPlot()
    # return(as.grob(plot_recorded))
    
  })
  
  assign(paste("list_ggp", plottype, list_crosspredictions[[1]]$label, sep="_"), list_plots_out, envir=.GlobalEnv)
  
}