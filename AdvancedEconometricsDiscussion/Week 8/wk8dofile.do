capture log close _all

*===============================================================================
* Date: Feb 9, 2018
* By: Conor Foley
* 
* This code walks through the demonstration problems for Discussion Questions in
* week 8 of Econ 103 - Introduction to Econometrics, taught by Professor Rojas
* 
* Textbook is: Principles of Econometrics 4th Edition (Hill, Griffith, and Lim)
*
* Covered Problems: 7.9
*
* Note that this text will not appear in the log file, since it all comes before
* we initiated the log
*
* If you want to run this program, you will need the following .dta files to
* to be located in STATA's working directory:
* (1) star.dta
*
*===============================================================================

log using wk8_section_log, replace name(main)

// Demonstration STATA code for week 8
// Principles of Econometrics 4th Edition
// Covered Problems: 7.9

set more off
clear all
use star.dta, clear

////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
//////////////////////////////// Question 7.9 \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
////////////////////////////////////////\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
	
********************************************************************************
*Setup: We consider data from an experiment where classrooms were divided into
* three groups: (1) small class (13-17 students), (2) regular-size classes (22-
* 25 students), and (3) regular-size classes with a teaching assistant. The data
* include test scores, along with some student, teacher, and school 
* characteristics.
*
* Parts (A) - (G)
********************************************************************************

********************************************************************************
*Part A: Calculate the average of TOTALSCORE for (i) students in regular-sized
* classrooms with full time teachers, but no aide (ii) stduents in regular-sized
* cassrooms with full time teachers, and an aide, and (iii) students in small
* classrooms. What do you observe about test scores in these three types of
* learning environments?
********************************************************************************

// Each of the three class types is associated with its own dummy variable. 
// Despite the name, regular and aide are muutally exclusive (i.e. if
// regular = 1, then aide must be 0.) Together, each observation has a 1 for
// at least one of small, regular, and aide.

sum totalscore if small == 1
sum totalscore if regular == 1
sum totalscore if aide == 1

// Looking at these simple, UNCONDITIONAL, averages, the small classrooms appear
// to have higher average scores than regular-size classes, while there is not
// much difference between regular classes with and without an aide.

********************************************************************************
*Part B: Estimate the regression model 
*
*    TOTALSCORE_i = beta1 + beta2*SMALL_i + beta3*AIDE_i + e_i
*
* where AIDE in a indicator variable equaling one for classes taught by a
* teacher and an aide, and zero otherwise. What is the relation of the estimated
* coefficients from this regression to the sample means in part (a)? Test the
* significance of beta3 at the 5% level of significance.
********************************************************************************

reg totalscore small aide

/* Discussion:

When we have only non-interacted dummy variables in the regression, the constant 
in the regression refers to the average for the excluded group. In this case,
the excluded group is the non-small, non-aide classes - the "regular" group.
Compare the beta estimate for the constant to the mean totalscore for group 2
(regular-sized classes).

The beta on the dummy variable tells us the gap between the average for the 
dummy group (e.g. small or aide) and the excluded group. In addition, if we
wanted to know the average difference between the two dummy groups, small and 
aide, we would take the difference between the beta estimates for small and aide
respectively. Compare the difference in means we calcuated in Part (A) to the
beta estimates here.

Testing the null that beta3 = 0 is the same as asking if the average test score
differs between the "regular" group and the "aide" group. The STATA output 
already gave us the information we need for this test. The p-value of 0.892 
means we fail to reject the null that beta3 = 0.

*/

********************************************************************************
*Part C: To the regression in (b) add the additional explanatory variable 
* TCHEXPER. Is this variable statistically significant? Does its addition to the
* model affect the estimates of beta2 and beta3?
********************************************************************************

reg totalscore small aide tchexper

/* Discussion:

Now that we have non-dummy variables in the regression, the interpretation of
the betas for the dummies (small and aide) changes slightly. Now, we should 
think of these as telling us the change in the average test score CONDITIONAL on
teacher experience. The beta estimates changed a little bit, falling 0.9 points
for aide and rising 0.2 points for small. The standard errors and t-statistics
also changed, but only slightly. This all suggests that teacher experience is 
only weakly correlated with the class types.

What do we mean by the average difference CONDITIONAL on tecaher experience? 
Imagine if we lined up all the classes and only took the difference between 
small, regular, and aide classes where the teacher had the same experience. 
Then, after taking these experience-by-experience differences, we average over
the gaps. This is in essense what OLS is doing: we pick a slope for tchexper 
that works well across all three types, and given this slope we adjust the 
average test score in each group to best fit the data. Mechanically, suppose we
fix the level of tchexper at some number X and want to find the expected
difference in student scores between a small class and a regular class. Then
we have:

score_hat(small = 1, tchexper = X) - score_hat(reg = 1, tchexper = X) = 
beta1 + beta2 + beta4*X - (beta1 + beta4*X) = beta2

While the beta estimates for small and aide changed only slightly, the beta
for tchexper is significant (t = 8.78) and the R2 of the regression has increased
from 0.007 to 0.02, a modest improvement. The improved fit of the regression
also helps to lower the standard errors of our estimates.

While the betas on the dummy variables have changed only slightly, the estimate
for the constant term is very different. The constant is no longer measuring
the average for the "regular" group. Rather, the constant ensures that the
regression line will go through the average of totalscore and tchexper among
regular-group classes. That is, if score_reg and tchexper_reg are the values we
get for the mean when we put in the command "sum totalscore if reg == 1" and 
"sum tchexper if reg == 1", then the constant in the regression ensure that

score_hat(reg=1, tchexper = tchexper_reg) = 
score_reg = beta1 + (tchexper_reg)*beta4

Similarly, the regression line when small = 1 or when aide = 1 will go through
(score_small, tchexper_small) and (score_aide, tchexper_aide). 

Also - a minor note: the sample size changed because there are 20 observations
that lack data on tchexper. It turns out that all of these observations are also
for classrooms with teacher aides. However, if we re-run the regression in
part (b) using only the data points that have tchexper data, there is little
change in our estimates.

*/

********************************************************************************
*Part D: To the regression in (c) add the additional explanatory variables BOY,
* FREELUNCH, and WHITE_ASIAN. Are any of these variables statistically
* significant? Does their addition to the model affect the estimates of beta2 and
* beta3?
********************************************************************************

reg totalscore small aide tchexper boy freelunch white_asian

/* Discussion:

As before, including the new variables has only a slight change on the estimates
of the coefficients for small and aide. AIDE jumps to a positive level again
but the new estimate is well within the confidence bounds of the estimates in
parts (b) and (c). The standard errors for small and aide continue to decline,
in part reflecting an increase in R2 from 0.02 to 0.10.

The estimates for boy, freelunch, and white_asian are all significant, with the
t-statistics for a null of zero all having large absolute values.

*/

********************************************************************************
*Part E: To the regression in (d) add the additional explanatory variables
* TCHWHITE, TCHMASTERS, SCHURBAN, and SCHRURAL. Are any of these variables
* statistically significant? Does their addition to the model affect the
* estimates of beta2 and beta3?
********************************************************************************

reg totalscore small aide tchexper boy freelunch white_asian tchwhite tchmaster schurban schrural

/* Discussion:

The new variables are modestly significant, with t-statistics in the range of
(absolute value) 1.74 to 2.74. This lowest p-value is for tchmaster, with a 
p-value of 0.078, meaning we fail to the beta on tchmaster = 0 at 5%, but do 
reject at 10%. The schurban coefficient is next, with a p-value of 0.044 (reject
at 5%, fail to reject at 1%) and the other two variables have p-values below
0.01.

The beta2 and beta3 estimates again move slightly. This time, however, the
standard errors increased slightly. This partly reflects the reduced degrees of
freedom together with the marginal increase in R2 (from 0.102 to 0.106).


*/

********************************************************************************
*Part F: Discuss the importance of parts (c), (d), and (e) to our estimation of 
* the "treatment" effects in part (b).
********************************************************************************

/* Discussion:

The evidence presented above reflects that there is limited omitted variable
bias in our estimates. We can see this in the fact that including additional
RHS terms does little to move our point estimates for small and aide. The lack
of omitted variable bias despite the explanatory power of these RHS terms means
that there is little correlation between these RHS terms and the treatment - i.e.
assignment of a student/teacher to a classroom type. This is expected given the
experimental nature of the research design.

Overall, in all our regression, we find that small classes have higher test 
scores on the order of 13-14 points and regular-size classes with aides have no
significant improvement relative to a regular-size class without an aide.

*/

********************************************************************************
*Part G: Add to the models in (b) through (e) indicator variables for each
* school: SCHOOL_j = 1 if student is in school j, and = 0 otherwise. Test the
* joint significance of these school "fixed effects". Does the inclusion of
* these fixed effect indicator variables substantially alter the estimates of
* beta2 and beta3?
********************************************************************************

/* Discussion:

As we saw before, including additional RHS terms did little to change our beta
estimates. It turns out that including all the school fixed effects modestly
raises the point estimates for small and aide, but these adjustments are still
well within the confidence intervals for the regressions without fixed effects.
This once again aligns with our expectation that omitted variable bias will not
be a major concern given the experimental design.

How does the fixed effect change our interpretation of the betas on small and 
aide? As we mentioned previously, when we have additional RHS variables, we can 
think of our betas as telling us the effect of class type CONDITIONAL on the 
other RHS terms. By adding fixed effects, we've included ANYTHING THAT SHIFTS THE
SCHOOL-WIDE AVERAGE as the thing we're conditioning on. In other words, when we
have a school fixed-effect, all the other betas can only help to explain within-
school variation, since by definition the fixed effect estimates will make sure
we hit the school-wide average in each school.

A couple semi-technical notes about fixed effects. First, recall the 
dummy-variable trap, which tells us that we cannot include a dummy for every 
schid value because every observations has a schid. STATA automatically drops 
the smallest value for schid (112038) and includes a dummy for each of the 
remaining 78 schools. Given this, the interpretation of the dummy on a given
fixed effect would be whether students at a school have different average test 
scores (conditional on other RHS variables) than the "base school" (i.e. the 
school that didn't get a dummy variable, or school 112038).

Second, the full set of school id dummies will be collinear with any other 
school-wide characteristic, such as the urban or rural status of the school. The
indicator for urban, for example, is just the simple sum of the indicators for
all urban schools. This shows the sense in which including all the fixed effects
picks up any school-wide characteristic that might affect scores. For any school
characteristic (that is constant over time) that we can imagine, the fixed 
effect will pick this up.

How does the "urba school" effec get picked up by the school fixed effects? 
While it may be true that urban schools systematically differ from non-urban 
schools, this information is also picked up by the school fixed effect. For 
example, if all urban schools do worse, then all urban schools will have a lower 
fixed effect than an otherwise similar school. However, not all urban schools 
will have the SAME fixed effects, since there may be other school-specific 
characteristics that differentiate one urban school from another.

In this experiment, we can have school-wide fixed effects because the treatment 
is done on a classroom-by-classroom basis so we still have within-school 
variation to use to estimate the effect.

*/

quietly log off main

local stubsToLoop b c d e
local varsToLoop small aide

local varGroup_b small aide
local varGroup_c `varGroup_b' tchexper
local varGroup_d `varGroup_c' boy freelunch white_asian
local varGroup_e `varGroup_d' tchwhite tchmasters schurban schrural

local setColnames b c d e bFE cFE dFE eFE
local setRownames beta stdErr t p y_r2 FE_fstat

matrix store_aide = J(6,8,.)
matrix rownames store_aide = `setRownames'
matrix colnames store_aide = `setColnames'

matrix store_small = J(6,8,.)
matrix rownames store_small = `setRownames'
matrix colnames store_small = `setColnames'

scalar count = 0

// This Loop: for each group of RHS variables (b, c, d, and e), run a regression
// with and without school fixed effects. Store summary statistics for betas on
// small and aide, as well as regression-wide summary stats. For the FE regression,
// also calculate the joint significance of call FE terms. Finally, in group E,
// drop schurban and schrural from the regression with FEs due to colinearity.
foreach x of local stubsToLoop {
	scalar count = count + 1
	
	// Regression using group x (one of b, c, d, or e)
	qui reg totalscore `varGroup_`x''
	
	foreach var of local varsToLoop {
	
		matrix store_`var'[1,count] = _b[`var']
		matrix store_`var'[2,count] = _se[`var']
		matrix store_`var'[3,count] = _b[`var']/_se[`var']
		matrix store_`var'[4,count] = 2*ttail(e(df_r),abs(store_`var'[3,count]))
		matrix store_`var'[5,count] = e(r2)
		
	}
	
	if strmatch("`x'","e") {
		// schurban and schrural are each co-linear with the full set of school 
		// dummies, so we cannot include all of them together in a regression
		//
		// 2 options: let STATA drop terms automatically (it will probably drop
		// a couple school dummies), OR tell STATA which variables not to use.
		//
		// This IF statement implements option 2 by removing schurban and
		// schrural from the list of variables to use
		local dropSchDummies schurban schrural
		local varGroup_e : list varGroup_e -dropSchDummies
	}
	
	// Regression using group x (one of b, c, d, or e) AND school fixed effects
	qui reg totalscore `varGroup_`x'' i.schid
	
	// Contrast is useful for running joint hypothesis tests when we have many
	// dummy/interaction terms. Use it here to do F-test of all dummies based on
	// the schid variable
	qui contrast schid 
	matrix tmp_fstat = r(F)
	
	foreach var of local varsToLoop {
	
		matrix store_`var'[1,count+4] = _b[`var']
		matrix store_`var'[2,count+4] = _se[`var']
		matrix store_`var'[3,count+4] = _b[`var']/_se[`var']
		matrix store_`var'[4,count+4] = 2*ttail(e(df_r),abs(store_`var'[3,count+4]))
		matrix store_`var'[5,count+4] = e(r2)
		matrix store_`var'[6,count+4] = tmp_fstat[1,1]
		
	}	
	
}


quietly log on main
// OLS estimates for aide in regressions b-e, with and without school FEs
matrix list store_aide, format(%9.4f)

// OLS estimates for small in regressions b-e, with and without school FEs
matrix list store_small, format(%9.4f)


//Convert log file (smcl) to pdf
translate wk8_section_log.smcl "Week 8 TA Section STATA.pdf"

log close main