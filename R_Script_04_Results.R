#	-------------------------------------------------------------------------------------------------------------------
# ----
#	----	ANALYSIS: EFFECTS OF EXTREME TEMPERATURE ON GERMAN EMERGENCY CARE
#	----
#	----  R_Script_01_Results
#	----
#	-------------------------------------------------------------------------------------------------------------------

#   Author: 					          Jona Frasch 
#   E-Mail: 					          j.frasch@uke.de 
#   Tel.: 						          +49 40 7410 52676

#	-------------------------------------------------------------------------------------------------------------------
#	----
#	----	BLOCK IV: RESULTS
#	----
#	-------------------------------------------------------------------------------------------------------------------

#	-------------------------------------------------------------------------------------------------------------------
# IV.A FINAL STUDY RESULSTS
# -------------------------------------------------------------------------------------------------------------------


###-----------------------------------------------###
###--------- RANDOM-EFFECTS META-ANALYSIS --------###


###--------- OVERALL EFFECT --------###
summary(meta_overall_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)
summary(meta_overall_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)$i2stat
summary(meta_overall_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)$corFixed
summary(meta_overall_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)$corRandom

###--------- HEAT EFFECT --------###
summary(meta_heat_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)
summary(meta_heat_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)$i2stat
summary(meta_heat_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)$corFixed
summary(meta_heat_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)$corRandom


###--------- COLD EFFECT --------###
summary(meta_cold_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)
summary(meta_cold_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)$i2stat
summary(meta_cold_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)$corFixed
summary(meta_cold_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final)$corRandom



# PLOT RESULTS OF THE RANDOM-EFFECTS META META ANALYSIS

layout(matrix(c(1,2,3,4),ncol=1),heights=c(1,5,5,5))
par(mar=c(0,0,0,0))
plot.new()
text(0.5,0.5,"RANDOM EFFECTS META ANALYSIS",cex=1.5,font=2)
par(mar=c(4, 4, 3, 2))
plot(cp.meta_overall_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final, 
     xlab="Percentile of Mean Temperature",
     ylab="RR Emergency Admission")
plot(cp.meta_heat_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final, 
     main="Heat Effect",
     xlab="Lag in days",
     ylab="RR Emergency Admission")
plot(cp.meta_cold_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final, 
     main="Cold-Effect",
     xlab="Lag in days",
     ylab="RR Emergency Admission")
dev.copy(jpeg,filename="Output/Plot_Meta-Analysis_Random-effects_final.jpg", width = 750, height = 1000, quality = 100);
dev.off() 

### PLOT FOR COMPARISON FIXED AND RANDOM EFFECTS META ANALYSIS ###

layout(matrix(c(1,2,3,4,5,6,7,8,2,9,4,10,6,11),ncol=2),heights=c(1,1,4,1,4,1,4))
par(mar=c(0,0,0,0))
plot.new()
text(0.5,0.5,"Fixed Effects Meta Analysis",cex=1.5,font=2)
plot.new()
text(0.5,0.5,"OVERALL EFFECT",cex=1,font=2)
par(mar=c(4,4,1,1))
plot(cp.meta_overall_fixed_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final, 
     xlab="Percentile of Mean Temperature",
     ylab="RR Emergency Admission")
par(mar=c(1,1,1,1))
plot.new()
text(0.5,0.5,"HEAT EFFECT",cex=1,font=2)
par(mar=c(4,4,1,1))
plot(cp.meta_heat_fixed_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final, 
     xlab="Lag in days",
     ylab="RR Emergency Admission")
par(mar=c(1,1,1,1))
plot.new()
text(0.5,0.5,"COLD EFFECT",cex=1,font=2)
par(mar=c(4,4,1,1))
plot(cp.meta_cold_fixed_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final, 
     xlab="Lag in days",
     ylab="RR Emergency Admission")
par(mar=c(1,1,1,1))
plot.new()
text(0.5,0.5,"Random Effects Meta Analysis",cex=1.5,font=2)
par(mar=c(4,4,1,1))
plot(cp.meta_overall_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final, 
     xlab="Percentile of Mean Temperature",
     ylab="RR Emergency Admission")
plot(cp.meta_heat_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final, 
     xlab="Lag in days",
     ylab="RR Emergency Admission")
plot(cp.meta_cold_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp_final, 
     xlab="Lag in days",
     ylab="RR Emergency Admission")
dev.copy(jpeg,filename="Output/Plot_Meta-Analysis_Compare-fixed-random_final.jpg", width = 750, height = 1000, quality = 100);
dev.off()


#	-------------------------------------------------------------------------------------------------------------------
# IV.B FOR COMPLETENESS: RESULTS FROM PRIVACY-FILTERED DATA
# -------------------------------------------------------------------------------------------------------------------

# SUMMARY OF THE META ANALYSIS
summary(meta_overall_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp)
summary(meta_heat_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp)
summary(meta_cold_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp)

# PLOT RESULTS OF THE META ANALYSIS

par(mar=c(0,0,0,0))
layout(matrix(c(1,2,3,4,5),ncol=1),heights=c(1,.5,5,5,5))
plot.new()
text(0.5,0.5,"RANDOM EFFECTS META ANALYSIS",cex=2,font=2)
plot.new()
text(0.5,0.5,"analysis based on privacy filtered data (not final study results, but reproducible)",cex=1,font=2)
par(mar=c(4, 4, 3, 2))
plot(cp.meta_overall_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp, 
     main="Overall Effect",
     xlab="Percentile of Mean Temperature",
     ylab="RR Emergency Admission")
plot(cp.meta_heat_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp, 
     main="Heat Effect",
     xlab="Lag in days",
     ylab="RR Emergency Admission")
plot(cp.meta_cold_random_N_Admissions_TMKp_nkl2_dfy8_28l_qp, 
     main="Cold Effect",
     xlab="Lag in days",
     ylab="RR Emergency Admission")
dev.copy(jpeg,filename="Output/Plot_Meta-Analysis_Compare-fixed-random_incomplete-data.jpg", width = 750, height = 1000, quality = 100);
dev.off()