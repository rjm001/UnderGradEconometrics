capture log close _all

*===============================================================================
* Date: Feb 20, 2018
* By: Conor Foley
* 
* This code walks through the demonstration problems for Discussion Questions in
* week 9 of Econ 103 - Introduction to Econometrics, taught by Professor Rojas
* 
* Textbook is: Principles of Econometrics 4th Edition (Hill, Griffith, and Lim)
*
* Covered Problems: 7.2 and 7.14
*
* Note that this text will not appear in the log file, since it all comes before
* we initiated the log
*
* If you want to run this program, you will need the following .dta files to
* to be located in STATA's working directory:
* (1) fullmoon.dta and (2) fair4.dta
*
*===============================================================================

log using wk9_section_log, replace name(mainlog)

// Demonstration STATA code for week 9
// Principles of Econometrics 4th Edition
// Covered Problems: 7.2 and 7.14

set more off
clear all
use fullmoon.dta, clear

////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//////////////////////////////// Question 7.2 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

********************************************************************************
* Long story short: Compare the results of the regression below, with and
* without the fullmoon and newmoon variables. Conduct the joint hypothesis test
* for whether to include either fullmoon and/or newmoon in the regression.
********************************************************************************

reg case t holiday friday saturday fullmoon newmoon
test (fullmoon = 0) (newmoon = 0)

// Can also calculate the F statistic by calculating the "restricted" regression
// and using either the sse equation or the r2 equation.

// Store sse and r2 from the unrestricted regression. Also collect the degrees
// of freedom from the unrestricted regression
scalar sse_u = e(rss)
scalar r2_u = e(r2)
scalar df_u = e(df_r)

// Run "restricted" regresion where we impose that betas for fullmoon and 
// newmoon are zero (i.e. drop them from the regression)
reg case t holiday friday saturday

// Store sse and r2 from the restricted regression.
scalar sse_r = e(rss)
scalar r2_r = e(r2)

// Calculate the F-stat using the sse formula and the r2 formula
// Compare the results to the test command from earlier
scalar fstat_sse = ((sse_r-sse_u)/2)/(sse_u/df_u)
scalar fstat_r2 = ((1-r2_r)/(1-r2_u) - 1)*(df_u/2)

disp "F-statistic using SSE formula: " fstat_sse " (p-value = " Ftail(2,df_u,fstat_sse) ")"
disp "F-statistic using R2 formula: " fstat_r2


////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
/////////////////////////////// Question 7.14 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

clear all
use fair4.dta

********************************************************************************
*Part A: Consider the regression model
* 
* VOTE = beta1 + beta2*GROWTH + beta3*INFLATION + beta4*GOODNEWS 
*              + beta5*PERSON + beta6*DURATION + beta7*PARTY + beta8*WAR
* 
* Discuss the anticipated effect of the dummy variables PERSON and WAR
********************************************************************************

********************************************************************************
*Part B: The binary variable PARTY is somewhat different from the dummy
* variables we have considered. Write out the regression function E(VOTE) for
* the two values of PARTY. Discuss the effects of this specification.
********************************************************************************

********************************************************************************
*Part C: Use the data for the period 1916-2004 to estimate the proposed model.
* Discuss the estimation results. Are the signs as expected? Are the estimates
* statistically significant? How well does the model fit the data?
********************************************************************************

reg vote growth inflation goodnews person duration party war if year >= 1916 & year <=2004

********************************************************************************
*Part D: Predict the outcome of the 2008 election using the given 2008 data
* for values of explanatory variables. Based on the prediction, would you have
* picked the outcome of the election correctly?
********************************************************************************

predict voteHat, xb
list voteHat if year == 2008

********************************************************************************
*Part E: Construct a 95% confidence interval for the outcome of the 2008 
* election. 
********************************************************************************

// Generate the standard errors for voteHat: prediction (stdp) and forecast (stdf)
predict stdp, stdp
predict stdf, stdf

// Calculate t-critical value using inverse t function
scalar alpha = 0.05
scalar tval = invt(e(df_r),1-alpha/2)

// Calculate confidence intervals using stdp and stdf
gen cilow_stdp = voteHat - tval*stdp
gen cihigh_stdp = voteHat + tval*stdp
gen cilow_stdf = voteHat - tval*stdf
gen cihigh_stdf = voteHat + tval*stdf

// Display fitted value and confidence intervals
list voteHat cilow_stdp cihigh_stdp cilow_stdf cihigh_stdf if year == 2008

********************************************************************************
*Part F: Using data values of your choice (you must explain them), predict the
* outcome of the 2012 election.
********************************************************************************

// Add a new row to the data set (will be all blank values)
scalar newOb = _N+1
set obs `=newOb'

replace year = 2012 in `=newOb' // add value for year

// Set values for known variables
replace party = 1 			if year == 2012 // democrat in 2012
replace person = 1 			if year == 2012 // Obama re-election run in 2012
replace war = 0 			if year == 2012 // Not 1920, 1944, or 1948
replace duration = 0 		if year == 2012 // Same as 1984, 1996, 2004, etc.

// Set values for economic variables (open to interpretation)
replace goodnews = 4		if year == 2012 
replace growth = 2			if year == 2012
replace inflation = 1.5 	if year == 2012

// Re-run regression, now including the 2008 observation
qui reg vote growth inflation goodnews person duration party war if year >= 1916 & year <=2008

// Calculate fitted value and STDF
predict voteHat2, xb
predict stdf2, stdf

// Calculate new t-val (+1 to degrees of freedom from previous estimate)
scalar tval2 = invttail(e(df_r),alpha/2)

gen lb = voteHat2 - tval2*stdf2
gen ub = voteHat2 + tval2*stdf2

// Show the new fitted value and confidence interval
list lb voteHat2 ub if year == 2012

//Convert log file (smcl) to pdf
translate wk9_section_log.smcl "Week 9 TA Section STATA.pdf"

log close mainlog