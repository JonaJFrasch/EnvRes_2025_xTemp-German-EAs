# Study: Effects of Extreme Temperatures on German Emergency Care
The here provided script and data aim to reproduce an analysis on the effects of temperature and extreme temperature on emergency admissions to German hospitals. To reproduce the analysis, we provide a simulated dataset, but import and report results from analysison the complete data-set.

The study implements a two stage time-series design.
In the first stage, a distributed lag non-linear model is fit to data of each of Germanys counties spanning 10 years. 
Here, the effect of temperature is modeled using a natural cubic spline along both the space of the predictor and its lag. 
Covariates include the day of the week, an indicator for school holidays, as well as a natural cubic spline function with dfperyear degrees of freedom per year. In the final study, the number of knots along the space of the predictor and its lag, dfperyear, and the length of the lag period is varied. Here, we report only those of the main analysis. 
In the second stage, results from the first stage are aggregated using meta analysis. 

## Data & Variables:
* datain: County- & day-level observations of the number of emergency admissions and the mean daily temperature. The here provided data was "simulated" based on an original version of the data (excluding cells based on fewer than three emergency admissions) using the script "R_Script_XX_Addendum_Simulated-data.R" provided.
  * county - County-identifier
  * Date - Date of Admission
  * time - Number of Days since the beginning of the observation period
  * school_holiday - Indicator variable for school holidays
  * dow - Date of the week
  * TMKp - County-specific percentile of the daily average temperature
  * N_Admissions - Number of Emeregncy Admissions on a given day in a given county
* data_counties: County information for random-effects meta-analysis
  * county - County-identifier
  * AGS_uni - Official county key
  * county_name - County name
  * EWZ - population size
  * X - X-coordinate of geogrpahical center
  * Y - Y-coordinate of geogrpahical center
* data_coefs_vcovs_S1_final: Stage 1 results from the analysis on the complete, unfiltered dataset. 

## Scripts: 
* R_Script_01_Setup: Script imports data, defines lists required later in the analysis, and defines the parameters used for the parametrization of Stage 1 models (number and placemen of knots in the spline-functions, length of the lag period, etc)
* R_Script_02_Stage-1-DLNM: Stage 1 analysis with the model parametrizations defined previously (here, only one model is implemented)
* R_Script_03_Stage-2-Meta-Analysis: Stage 1 analysis with the model parametrizations defined previously (here, only one model is implemented)
* R_Script_04_Results: A Fixed and a random effects meta analysis based on the reduced Stage 1estimates of stage 1 results. Separate analysis for the overall effect, and for the lagged effect of temperature at the 1st and 99th percentile. Non-reduced meta-analysis would have to be uncommented
* R_Script_05_Exploratory-Analysis-County-Level: County-specific distribution of outcomes, visualization on seasonal trend, plots of Stage 1 analyses (county specific models)

## Output:
The results provided in folder Output comprise: 
* The county-specific distribution of the number of emergency admission with a fitted poisson- and negative-binomial distribution (folder Output/Distribution)
* County specific plots of stage 1 results: overall effect plot, contour plot, slices along the lag for extreme heat and cold
* Time series and fitted curve of the seasonal control with varyying degrees of freedom
* Plots of the second stage meta analysis
* Index of dispersion of N_Admissions by county, and by county and day of the week
