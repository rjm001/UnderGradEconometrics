capture log close _all

*===============================================================================
* Date: Feb 23, 2018
* By: Conor Foley
* 
* This code walks through the demonstration problems for Discussion Questions in
* week 10 of Econ 103 - Introduction to Econometrics, taught by Professor Rojas
* 
* Textbook is: Principles of Econometrics 4th Edition (Hill, Griffith, and Lim)
*
* Covered Problems: 8.22
*
* Note that this text will not appear in the log file, since it all comes before
* we initiated the log
*
* If you want to run this program, you will need the following .dta files to
* to be located in STATA's working directory:
* (1) lasvegas
*
*===============================================================================

log using wk10_section_log, replace name(mainlog)

// Demonstration STATA code for week 10
// Principles of Econometrics 4th Edition
// Covered Problems: 8.22

set more off
clear all
use lasvegas.dta, clear

////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
/////////////////////////////// Question 8.22 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

********************************************************************************
*Part A: Estimate the linear probability model (regression) using the model
* explaining DELINQUENT as a function of the remaining variables. Use the White
* test with cross-product terms included to test for heteroskedasticity. Why did
* we include the cross-product terms?
********************************************************************************

// Run OLS
qui reg delinquent lvr ref insur rate amount credit term arm

// For the White test, we can use the command: estat imtest, white
// We are interested in the output before the table with a longer decomposition
estat imtest, white

********************************************************************************
*Part B: Use the estimates from (a) to estimate the error variances for each
* observation. How many of these estimates are at least one? How many are at
* most zero? How many are less than 0.01?
********************************************************************************

// Generate regression fitted values
predict yHat, xb

// Generate variance using yHat*(1-yHat)
// Note, this variance comes from our model, and is different from eHat^2
gen varHat = yHat*(1-yHat)

// Generate varHat_tab for easy presentation of the three conditions described
gen varHat_tab = "(3) other" // default value = "other"
replace varHat_tab = "(1) Less than zero" if varHat < 0
replace varHat_tab = "(4) Greater than 1" if varHat >= 1
replace varHat_tab = "(2) Between 0 and 0.01" if varHat <= 0.01 & varHat >= 0

// Tabulate results
tab varHat_tab

********************************************************************************
*Part C: Prepare a table containing estimates and standard errors from 
* estimating the linear probability model in each of the following ways:
*
* (i)   Least squares with conventional standard errors
* (ii)  Least squares with heteroskedasticity-robust standard errors.
* (iii) Generalized least squares omitting observations with variance less than 
*       0.01
* (iv)  Generalized least squares with variances less than 0.01 changed to 0.01
* (v)   Generalized least squares with variances less than 0.00001 changed to 
*       0.00001
*
* Discuss and compare the results.
********************************************************************************

// Construct weights for the GLS in 4-5
gen wgt_4 = 1/varHat
replace wgt_4 = 1/0.01 if varHat < 0.01
gen wgt_5 = 1/varHat
replace wgt_5 = 1/0.00001 if varHat < 0.00001

// Run the regressions separately, and store results using eststo
qui reg delinquent lvr ref insur rate amount credit term arm
eststo ols

qui reg delinquent lvr ref insur rate amount credit term arm, robust
eststo ols_RSE

qui reg delinquent lvr ref insur rate amount credit term arm [aweight = 1/varHat] if varHat > 0.01
eststo gls_omit

qui reg delinquent lvr ref insur rate amount credit term arm [aweight = wgt_4]
eststo gls_t01

qui reg delinquent lvr ref insur rate amount credit term arm [aweight = wgt_5]
eststo gls_t0001

// Prepare table for results from the 5 models
esttab, b(%7.4f) se(%7.4f) stats(F N) star compress mtitles
eststo clear

********************************************************************************
*Part D: Using the results from (iv), interpret each of the coefficients. 
* Mention whether the signs are reasonable and whether they are significantly
* different from zero.
********************************************************************************

reg delinquent lvr ref insur rate amount credit term arm [aweight = wgt_4]

********************************************************************************
*Appendix A - Construct test stat for White test "by hand"
********************************************************************************

// In Part A above, we just used estat imtest to calculate the test statistic
// for the White test of homoskedasticity of the error term. Here, I walk 
// through conducting the test by estimating a secondary regression and then
// using the N*R^2 formula given in the textbook in Section 8.2.2

// use the describe command to collect names of all original variables in the 
// dataset (i.e. lvr to delinquent)
qui describe lvr-delinquent, varlist
local origVars = r(varlist)
// make a local (excl) that holds name: delinquent
local excl delinquent
// generate a local that includes all the origVars, excluding delinquent. These 
// are the RHS varibles for our main regression
local meanRHS : list origVars -excl 

// Use the following loop to make variables for the square and interaction of 
// all the RHS variables from the main regression. A squared value is call var_2 
// and an interaction is called int_var1_var2, where var, var1, var2 are the 
// names of the original variables.
local varCount : word count `meanRHS'
forvalues x = 1/`=`varCount'' {
	forvalues y = `x'/`=`varCount'' {
		
		// if `x' == `y' then we have a squared term
		if `x' == `y' {
			
			// meanRHS is a list of variable names
			// use local xvar : word `x' of `meanRHS' to extract the xth name
			// of the list
			local xvar : word `x' of `meanRHS'
			gen `xvar'_2 = `xvar'^2
			
			label variable `xvar'_2 "`xvar' Squared"

		}
		// otherwise, we have an interaction term
		else {
			
			local xvar : word `x' of `meanRHS'
			local yvar : word `y' of `meanRHS'
			gen int_`xvar'_`yvar' = `xvar'*`yvar'
			
			label variable int_`xvar'_`yvar' "int `xvar' `yvar'"
			
		} // end if x == y ... else ...
	} // end forvalues y
} // end forvalues x
	
// Re-run regression of delinquent on original variables
qui reg delinquent `meanRHS' 

// Quietly calculate the white test value using estat imtest
qui estat imtest, white
scalar whiteTestVal_STATA = r(chi2)

// Generate residuals and square of residuals
predict eHat, residual
gen eHatSqr = eHat^2

// regress eHatSqr on level, interactions, and squares of all variables
// Given the names for the variables we built in the loop earlier, we can refer 
// to all these variables using the wildcard * to get all the squared (_2) and 
// interaction (int_) terms.
reg eHatSqr `meanRHS' *_2 int_*

// Note that ref_2, insur_2, and arm_2, along with arm, are all dropped due to 
// collinearity. This reduces our 44 RHS terms (not including the constant)
// down to 40 RHS terms. This matches the "model degrees of freedom" in STATA,
// stored as e(df_m)

// Calculate white test statistic as R^2 * N
scalar whiteTestVal_hand = e(r2)*e(N)

// Compare the "hand" calculated whiteTestVal to that given by STATA:
disp "Hand calculated White test statistic: " whiteTestVal_hand
disp "STATA-calculated White test statistic: " whiteTestVal_STATA

// Calculate the 99.5% critical value for the White test
scalar alpha = 0.005
disp "White Test " (1-alpha)*100 "% critical value: " invchi2(e(df_m),1-alpha)

// Can also use STATA's built in Breusch-Pagan test to run the White Test when
// using some extra options (i.e. feeding in the RHS variables and specifying
// the iid option)
qui reg delinquent `meanRHS' 
estat hettest `meanRHS' *_2 int_*, iid

********************************************************************************
*Appendix B - Weighted regression as OLS of transformed variables
********************************************************************************

// Another way to calculate the values from GLS (weighted regression) is to run
// an unweighted regression on a set of transformed variables.
// Specifically, whatever you assign as the aweight in the reg command we are
// multiplying both the left- and right-hand side variables by the square root
// of that variable and dividing by the square root of the average of that 
// variable, one observation at a time.
//
// Below, we walk through implementing the regression with "transformed" values 
// and compare the result to running the weighted regression.

// Generate the "transformed" variables by multiplying by sqrt(wgt_4) and
// dividing by sqrt(avg_wgt_4). We call these values var_adju. In addition, we
// try the variables without dividing through by sqrt(avg_wgt_4), which we will
// compare below. The variables without dividing by sqrt(avg_wgt_4) are called
// var_adju2
qui sum wgt_4
scalar avg_wgt_4 = r(mean)
foreach x of local origVars {
	gen `x'_adju = `x'*sqrt(wgt_4)/sqrt(avg_wgt_4)
	gen `x'_adju2 = `x'*sqrt(wgt_4)
}

// In addition, we need to calculate the "transformed" constant (which is no
// longer constant)
gen cnst_adju = sqrt(wgt_4)/sqrt(avg_wgt_4)
gen cnst_adju2 = sqrt(wgt_4)

// Construct a varlist of "adjusted" variables, and then remove 
// delinquent_adju from the list, giving us the RHS variables meanRHS_adju
qui desc *_adju, varlist
local adjuVars = r(varlist)
local excl delinquent_adju
local meanRHS_adju : list adjuVars -excl

// Do the same thing, but this time collecting the _adju2 variables
qui desc *_adju2, varlist
local adju2Vars = r(varlist)
local excl delinquent_adju2
local meanRHS_adju2 : list adju2Vars -excl

// Run OLS for delinquent_adju on the adjusted RHS values
// Be sure to not let STATA include a constant in the regression! We already 
// have the "transformed constant" included in the list meanRHS_adju
reg delinquent_adju `meanRHS_adju', noconstant

// Collect RMSE and SSE to compare to the _adju2 regression below
scalar rmse_adju = e(rmse)
scalar sse_adju = e(rss)

// Do the F-test that the all terms besides beta for cnst_adju are zero:
local excl cnst_adju
local meanRHS_adju_nocnst : list meanRHS_adju -excl
test `meanRHS_adju_nocnst'

// Compare the OLS of the x*sqrt(wgt_4)/sqrt(avg_wgt_4) values to weighted 
// regression with weights of wgt_4
reg delinquent `meanRHS' [aweight = wgt_4]

gen del_wt = delinquent*wgt_4

// Quirk (1) - the "F-test of the regression" differs because the "transformed"
// OLS also tests the cnst_adju term, while the weighted regression does not.
// Instead, compare the "F-test" reported by the weighted regression to the 
// test of the non-constant terms from the "transformed" OLS.
//
// Quirk (2) - the "Adjusted-R2" is different because of how STATA calculates
// the adjustment when the noconstant option is used. In STATA, adjusted R2 uses
// Adju R^2 = 1 - (SSE/(N-K))/(SST/(N-C)) where C = 1 if there is a constant in
// the regression and C=0 otherwise.

// Finally, lets look at what happens when we use OLS on the "adju2" variables
// i.e. x*sqrt(wgt_4) instead of x*sqrt(wgt_4)/sqrt(avg_wgt_4)
reg delinquent_adju2 `meanRHS_adju2', noconstant

// Collect the RMSE and SSE values
scalar rmse_adju2 = e(rmse)
scalar sse_adju2 = e(rss)

// The ratio of sse_adju2 to see_adju matches avg_wgt_4, and the ratio of
// rmse_adju2 to rmse_adju matches sqrt(avg_wgt_4)
disp "SSE ratio:   " sse_adju2/sse_adju "   v.s. Average of wgt_4: " avg_wgt_4
disp "RMSE ratio: " rmse_adju2/rmse_adju "   v.s. Sqrt of Average of wgt_4:  " sqrt(avg_wgt_4)

********************************************************************************
*Appendix C - Relationship between variance estimate and eHatSqr
********************************************************************************

gen val001 = 0.01
gen val000001 = 0.00001

label variable varHat "Estimated Variance"
label variable eHatSqr "Squared Error Term"

twoway (scatter varHat eHatSqr if delinquent == 0, msymbol(Oh) legend(label(1 "Delinquent = 0"))) ///
			(scatter varHat eHatSqr if delinquent == 1, msymbol(th) legend(label(2 "Delinquent = 1"))) ///
			(line val001 eHatSqr, legend(label(3 "VarHat = 0.01"))) ///
			(line val000001 eHatSqr, legend(label(4 "VarHat = 0.00001"))), ///
			xtitle("Squared Error Term") ytitle("Estimated Variance")

//Convert log file (smcl) to pdf
translate wk10_section_log.smcl "Week 10 TA Section STATA.pdf"

log close mainlog