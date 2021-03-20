capture log close _all

*===============================================================================
* Date: Feb 5, 2018
* By: Conor Foley
* 
* This code walks through the demonstration problems for Discussion Questions in
* week 7 of Econ 103 - Introduction to Econometrics, taught by Professor Rojas
* 
* Textbook is: Principles of Econometrics 4th Edition (Hill, Griffith, and Lim)
*
* Covered Problems: 6.14
*
* Note that this text will not appear in the log file, since it all comes before
* we initiated the log
*
* If you want to run this program, you will need the following .dta files to
* to be located in STATA's working directory:
* (1) hwage.dta
*
*===============================================================================

log using wk7_section_log, replace

// Demonstration STATA code for week 7
// Principles of Econometrics 4th Edition
// Covered Problems: 6.14

set more off
clear all
use hwage.dta, clear

////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
/////////////////////////////// Question 6.14 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

********************************************************************************
*Setup: In the context of a wage regression, use the RESET framework to conduct
* a model selection exercise.
*
* Parts (A) - (G)
********************************************************************************

********************************************************************************
*Part A: Estimate the model:
* 
* HW = beta1 + beta2*HE + beta3*HA+e
*
* What effects do changes in the level of education and age have on wages?
********************************************************************************

reg hw he ha

/* Discussion:

Based on the estimated coefficients, we would say:

beta2: An increase of 1 year in a husband's education would be expected to raise
       the husband's wage by 2.19 in 2006 dollars
	   
beta3: An increase of 1 year in a husband's age would be expected to raise the 
       husband's wage by 0.20 in 2006 dollars

*/
	 
********************************************************************************
*Part B: Does RESET suggest that the model in part (a) is adequate?
********************************************************************************

// To run the RESET test we:
// (1) calcuate the square and the cube of the fitted values
// (2) Run the auxillary regression:
//     hw = beta1 + beta2*he + beta3*ha + gamma1*hw_hat^2 + e
//     hw = beta1 + beta2*he + beta3*ha + delta1*hw_hat^2 + delta2*hw_hat^3 + e
//
// (3) Test (a) H0: gamma1 = 0, and (b) H0: delta1 = 0 and delta2 = 0
predict fithw_a, xb
gen fithw_a2 = fithw_a^2
gen fithw_a3 = fithw_a^3

reg hw he ha fithw_a2
// To test gamma1 = 0 can just look at the p-value for the fithw_a2 coefficient
qui reg hw he ha fithw_a2 fithw_a3
test (_b[fithw_a2]=0) (_b[fithw_a3]=0)

/* Discussion:

For both auxillary regressions, we are able to reject the null that the powers
of the fitted hw values have no effect in the regression. This suggests that the
model is mis-specified and we should explore adding in higher powers of the
RHS variables. We will do this in Part (C) below.

*/

// STATA comment:
// STATA also has a built-in function to do the RESET test using the 2nd, 3rd,
// and 4th powers of the fitted value. Compare the F-stats reported using
// estat ovtest following reg hw he ha, and the f-test for the coefficients on
// fithw_a2 fithw_a3 and fithw_a4 in the second regression below. The textbook
// authors argue that the 4th power test is not recommend because adding more
// polynomial terms reduces the power of the test (i.e. increases rate of
// failure to reject the null even when the null is false).
qui reg hw he ha
estat ovtest

gen fithw_a4 = fithw_a^4
qui reg hw he ha fithw_a2 fithw_a3 fithw_a4
test (_b[fithw_a2]=0) (_b[fithw_a3]=0) (_b[fithw_a4]=0)

********************************************************************************
*Part C: Add the variables HE^2 and HA^2 to the original equation and
* re-estimate it. Describe the effect that education and age have on wages in
* this newly estimated model.
********************************************************************************

reg hw he ha c.he#c.he c.ha#c.ha

disp "Education Tipping Point = beta2/(-2*beta4) = " _b[he]/(-2*_b[c.he#c.he])
margins, dydx(he) at(he=(12 16))

/* Discussion - Education:

The point estimate for beta2 (he) and beta4 (he^2) suggest that education has
an initially negative effect on wages, and then an positive effect with the 
marginal effect getting larger. We can see this because beta2 is negative and
beta4 is positive. As the calculation above suggests, the model estimates that
the marginal effect of education is positive for years 5 and upwards. If we look
at the distribution of he, this indicates that marginal effect is positive for
almost all individuals in the sample. In addition, comparing the marginal effect
at he=12 to the simple linear estimate shows an effect of a similar magnitude,
while at higher levels of education the marginal return is higher than in the
linear model.

*/

disp "Age Tipping Point = beta3/(-2*beta5) = " _b[ha]/(-2*_b[c.ha#c.ha])
margins, dydx(ha) at(ha=(30 35 40 45 50 55))

/* Discussion - Age:

The parabolic shape is flipped for age, with an initially positive relationship
(beta3 > 0) between age and wage declining and eventually turning negative 
(beta5 < 0). The relative magnitude of beta3 and beta5 is larger than that for
beta2 and beta4 (i.e. |beta3/beta5| > |beta2/beta4|) which lets us know that it
will take longer to reach the tipping point for age than it did for education. 
It turns out that the tipping point for age is around 48, or a little after
the mean age in the dataset. The relatively small beta5 also tells us that the 
marginal effect is changing more slowly than for education. The estimated 
marginal effect at 45 (approximately the mean age) is similar to that seen in 
the linear estimate from part (a).

*/

********************************************************************************
*Part D: Does RESET suggest that the model in pact (c) is adequate?
********************************************************************************

predict fithw_c, xb
gen fithw_c2 = fithw_c^2
gen fithw_c3 = fithw_c^3

qui reg hw he ha c.he#c.he c.ha#c.ha fithw_c2
test (_b[fithw_c2]=0)

qui reg hw he ha c.he#c.he c.ha#c.ha fithw_c2 fithw_c3
test (_b[fithw_c2]=0) (_b[fithw_c3]=0)

/* Discussion:

In both regressions, we fail to reject the null that the polynomials of the
fitted values have no effect. The RESET test suggests the model is adequate
with regards to including higher powers of the current set of RHS variables.

*/

********************************************************************************
*Part E: Reestimate the model in part (c) with the variable CIT included. What
* can you say about the level of wages in large cities relative to outside those
* cities?
********************************************************************************

reg hw he ha c.he#c.he c.ha#c.ha cit

/* Discussion:

The interpretation of the beta for cit is that wages are about $7.9 (2006$)
higher in large cities, on average, than are wages in other (non-large) cities.

*/

********************************************************************************
*Part F: Do you think CIT should be included in the equation?
********************************************************************************

/* Discussion:

Yes, I would recommend including CIT in the regression. First, the coefficient
for CIT is strongly significant. Second, its inclusion in the regression had
a noticeable effect on the estimated coefficients for the education and age,
with a particularly large effect for education. Third, including cit in the
regression lead to smaller standard errors for all the other RHS beta estimates,
indicating that it is adding significant new information to the regression.

We discuss this effect on the other beta estimates in more detail in part (g).

Below, we organize the beta estimates, standard errors, and t-statistics between 
the regressions with and without cit to make it easier to compare.

*/

// Run regressions and collect the beta estimates
//
// Note: the equations for standard errors and t-stats are somewhat complicated 
// matrix algebra equations. You do not need to worry about understanding what 
// is being done in those steps.
qui reg hw he ha c.he#c.he c.ha#c.ha
matrix beta_c = e(b)
matrix se_c = vecdiag(cholesky(diag(vecdiag(e(V)))))
matrix t_c = vecdiag(diag(beta_c)*inv(diag(se_c)))
qui reg hw he ha c.he#c.he c.ha#c.ha cit
matrix beta_f = e(b)
matrix se_f = vecdiag(cholesky(diag(vecdiag(e(V)))))
matrix t_f = vecdiag(diag(beta_f)*inv(diag(se_f)))

// Compile the beta and std error estimates into (2 x K) matricies, making sure 
// that estimates for the same variables are in the same column
matrix beta_compare = [ [beta_c, J(1,1,.)] \ [beta_f[1,1..4], beta_f[1,6], beta_f[1,5]] ]
matrix se_compare = [ [se_c, J(1,1,.)] \ [se_f[1,1..4], se_f[1,6], se_f[1,5]] ]
matrix t_compare = [ [t_c, J(1,1,.)] \ [t_f[1,1..4], t_f[1,6], t_f[1,5]] ]

// Add row and column names
local compareMats beta_compare se_compare t_compare
foreach x of local compareMats {
	matrix rownames `x' = "without cit" "with cit"
	matrix colnames `x' = "he" "ha" "heSqr" "haSqr" "cnst" "cit"
}

// Compare betas:
matrix list beta_compare
// Compare standard errors:
matrix list se_compare
// Compare t-stats:
matrix list t_compare

********************************************************************************
*Part G: For both the model estimated in part (c) and the model estimated in
* part (e), evaluate the following four derivatives:
*
* (i)  dHW/dHE for HE = 6 and HE = 15
* (ii) dHW/dHA for HA = 35 and HA = 50
*
* Does the omission of CIT lead to omitted-variable bias? Can you suggest why?
********************************************************************************

// The models in part (c) and part (f) both have the same formula for the 
// marginal effects:
//
// (i)  dHW/dHE = beta2 + 2*beta4*HE
// (ii) dHW/dHA = beta3 + 2*beta5*HA
//
// where beta2 is the term on HE, beta3 is the term on HA, beta4 is the term on
// HE^2, and beta5 is the term on HA^2
//
// The only difference between the marginal effects in (c) and (f) comes from
// the different estimates from beta2-beta5 that are calculated without or with
// the cit term.

// Calculate marginal effects for education (he)
matrix me_he = J(2,2,.)
matrix rownames me_he = "without cit" "with cit"
matrix colnames me_he = "he=6" "he=15"
matrix me_he[1,1] = beta_c[1,2-1]+2*beta_c[1,4-1]*6
matrix me_he[1,2] = beta_c[1,2-1]+2*beta_c[1,4-1]*15
matrix me_he[2,1] = beta_f[1,2-1]+2*beta_f[1,4-1]*6
matrix me_he[2,2] = beta_f[1,2-1]+2*beta_f[1,4-1]*15

// Calculate marginal effects for age (ha)
matrix me_ha = J(2,2,.)
matrix rownames me_ha = "without cit" "with cit"
matrix colnames me_ha = "ha=35" "he=50"
matrix me_ha[1,1] = beta_c[1,3-1]+2*beta_c[1,5-1]*35
matrix me_ha[1,2] = beta_c[1,3-1]+2*beta_c[1,5-1]*50
matrix me_ha[2,1] = beta_f[1,3-1]+2*beta_f[1,5-1]*35
matrix me_ha[2,2] = beta_f[1,3-1]+2*beta_f[1,5-1]*50

// View results:
//
// Marginal effects for HE
matrix list me_he
disp "HE marginal effect - with CIT ME is lower if HE < " (-1)*(beta_compare[1,1]-beta_compare[2,1])/(2*(beta_compare[1,3]-beta_compare[2,3]))
// Marginal effects for HA
matrix list me_ha
disp "HA marginal effect - with CIT ME is lower if HA < " (-1)*(beta_compare[1,2]-beta_compare[2,2])/(2*(beta_compare[1,4]-beta_compare[2,4]))

// Look at correlation between cit an other RHS variables (bottom row)
gen heSqr = he^2
gen haSqr = ha^2
corr he ha heSqr haSqr cit

/* Discussion:

The marginal effects fell for both education and age at the values specified.
We also show a calculation that says that for all levels of education in the
sample, the ME is lower in the CIT regression, while it is lower for almost all
ages in the sample (5% of observations have age > 57).

As we saw earlier when we compared the betas, the estimates for the linear terms
shifted down, while the estimates for the quadratic (X^2) terms shifted up. The
shift was larger for the linear term for both HE and HA, while coefficient
changes were larger for HE than for HA.

Given these changes, we would say that without including cit the estimates for 
HE and HA were biased UPWARDS and the estimates for HE^2 and HA^2 were biased
DOWNWARDS.

The presence of omitted variable bias tells us that (1) cit is correlated with
the RHS terms we're already using, and (2) cit is correlated the LHS term
OVER AND ABOVE its correlation with the RHS terms. In a more intuitive phrasing,
omitted variable bias occurs because cit incorporate additional information about
both a RHS variable AND and LHS variable that is not already featured in the
regression.

The simplest way to see that cit has the needed features is that (1) we can 
look at the correlation table for cit, and (2) the coefficient on cit in the
hw regression is strongly significant. The bias is larger for the HE terms
because the correlation between cit and HE and HE^2 is larger than is the 
correlation between cit and HA and HA^2.

*/

///////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\///////////////////////////////////////\\

/* Advanced Discussion:

If may seem odd that the heSqr and haSqr beta estimates shifted in the opposite
direction from the estimates on he and ha given that the correlations between
the linear and quadratic terms is of similar magnitude and has the same sign
(i.e. cit is positively correlated with he, ha, heSqr, and haSqr).

In general, if we have a collection of RHS terms X, a LHS term y, and a single
omitted variable z, then the bias for the beta for a single X variable, xk 
will be:

BIAS(xk) = beta(y,z; [X z])*beta(z,xk; X)

where beta(y,z; [X z]) is the beta estimate on z when we run 

reg y x1 x2 x3... z

and beta(z,xk; X) is the beta on xk when we run

reg z x1 x2 x3...

*/

// bias = change in estimate between the two regressions
matrix bias_direct = beta_compare[1,1..5] - beta_compare[2,1..5]
// grab beta on cit (i.e. beta(y,z; [X z])
scalar beta_hw_cit = beta_compare[2,6]
// calculate the beta for cit, i.e. beta(z, xk; X) for all xk
reg cit he ha c.he#c.he c.ha#c.ha
// Calculate the bias using beta(y,z; [X z])*beta(z,xk; X)
matrix bias_indirect = beta_hw_cit*e(b)

// Compare the change in the beta xk (bias_direct) to what we calculated using 
// the formula b(y,z;[X z])*b(z,xk;X) (bias_indirect). They match exactly!
matrix list bias_direct
matrix list bias_indirect

//Convert log file (smcl) to pdf
translate wk7_section_log.smcl "Week 7 TA Section STATA.pdf"

log close