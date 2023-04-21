clear all
global alessandro "C:\Users\magia\OneDrive - Università Commerciale Luigi Bocconi\AA Second Semester\Microeconometrics\Microeconometrics_Stata\Problem Set\PS 4\Alessandro"
global lukas "C:\Users\lukas\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 4\Lukas"
global anna "C:\Users\anna\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 4\Anna"
global final "C:\Users\magia\OneDrive - Università Commerciale Luigi Bocconi\AA Second Semester\Microeconometrics\Microeconometrics_Stata\Problem Set\PS 4\final"

cd "$final"

use "DTA\pset_4.dta", clear

bysort v_id: egen year_num = rank(year)
xtset v_id year_num

***************************
**# A #1
***************************
{
gen TREATED = (num_PSINPRES1980 == 1 | num_PSINPRES1980 == 2)
gen POST_TREATED = POST*TREATED 
}

***************************
**# B #2
***************************
{
local elections "1993 1996 2000"

local cond_1993 "if (ele1v_post92 == 1992 | ele1v_post92 == 1993)"
local cond_1996 "if (ele1v_post92 == 1994 | ele1v_post92 == 1995 | ele1v_post92 == 1996)"
local cond_2000 "if (ele1v_post92 == 1997 | ele1v_post92 == 1998 | ele1v_post92 == 1999 | ele1v_post92 == 2000)"	

foreach ele in `elections' {
		
	gen Y_C_`ele'=HEALTH_CENTER_VL `cond_`ele'' & TREATED==0
	gen Y_T_`ele'=HEALTH_CENTER_VL `cond_`ele'' & TREATED==1
				
	preserve 

	collapse Y_C_`ele' Y_T_`ele', by(year)
		
	twoway  (line Y_T_`ele' year) (line Y_C_`ele' year, lpattern(dash)),  ///
	xline(`ele', lcolor(green) lpattern(solid)) ///
	title(Outcome trends) ytitle(Health Center Percentage)  yscale(titlegap(*10))  /// 
	graphregion(color(white)) xlabel(1986 1990 1993 1996 2000 2003) ///
	legend(order(1 "Treatment" 2 "Control") size(medsmall) nobox region(lstyle(none)    lcolor(white)))  
	graph rename Y_`ele', replace 
	
	restore 

}

graph combine Y_1993 Y_1996 Y_2000, graphregion(color(white)) row(1)
graph export "Tables\Graph_1.png", replace width(2000) height(500)

/*
To check whether the parallel trend assumption is justified, we should verify whether the outcome of the control and of the treated groups follow the same trends until the year when we expect to see an effect. More precisely, in this context, we expect that INPRES program affects public-good provision the year of the first post-1992 election. Since the data are collected every three/four years (following census rules), we expect to see the first change in the effects in 1993, 1996 and 2000,  respectively for each group of villages. Therefore, we expect to see a parallel trend until the last census wave before the relevant election. Namely, we expect a parallel trend until 1990, 1993 and 1996 for the three groups respectively. 

From a graphical ispection, this requirement is surely justified for the 1992-93 and the  1994-1996 groups. However, we observe an increase in health center provosion among treated for the 1997-2000 group between 1993 and 1996, while the change in the control group seems to be slightly smaller. This could possibly violate the parallel trend assumption, but from this graph we cannot tell if this trend difference is statistically significant.
*/

}

***************************
**# C #3
***************************
{
	
* g=1, t=1
sum HEALTH_CENTER_VL if TREATED==1 & POST==1
scalar AVG_Y_1_1 = r(mean)
* g=1, t=0
sum HEALTH_CENTER_VL if TREATED==1 & POST==0
scalar AVG_Y_1_0 = r(mean)
* g=0, t=1
sum HEALTH_CENTER_VL if TREATED==0 & POST==1
scalar AVG_Y_0_1 = r(mean)
* g=0, t=1
sum HEALTH_CENTER_VL if TREATED==0 & POST==0
scalar AVG_Y_0_0 = r(mean)
*

matrix Table_1 = [AVG_Y_1_1, AVG_Y_0_1, (AVG_Y_1_1 - AVG_Y_0_1)\ ///
                  AVG_Y_1_0, AVG_Y_0_0, (AVG_Y_1_0 - AVG_Y_0_0)\ ///
				  (AVG_Y_1_1-AVG_Y_1_0), (AVG_Y_0_1-AVG_Y_0_0), ((AVG_Y_1_1-AVG_Y_1_0)-(AVG_Y_0_1-AVG_Y_0_0))]
				  
matrix rownames Table_1 = "Post=1" "Post=0" "Difference 1"
matrix list Table_1
esttab matrix(Table_1) using "Tables\Table_1.tex", title("Table 1") collabels("Treated=1" "Treated=0" "Difference 2")  replace

/*
From the table, we see that having at least one school constructed during the period of the INPRES program increases the probability of having a health center in the village after 1992 by 1.2 percentage points. 
*/

*We re-estimate the table for each village group separately: 

local elections "1993 1996 2000"

local cond_1993 "if (ele1v_post92 == 1992 | ele1v_post92 == 1993)"
local cond_1996 "if (ele1v_post92 == 1994 | ele1v_post92 == 1995 | ele1v_post92 == 1996)"
local cond_2000 "if (ele1v_post92 == 1997 | ele1v_post92 == 1998 | ele1v_post92 == 1999 | ele1v_post92 == 2000)"

foreach ele in `elections' {
	
preserve
	
keep `cond_`ele''

* g=1, t=1
sum HEALTH_CENTER_VL if TREATED==1 & POST==1
scalar AVG_Y_1_1 = r(mean)
* g=1, t=0
sum HEALTH_CENTER_VL if TREATED==1 & POST==0
scalar AVG_Y_1_0 = r(mean)
* g=0, t=1
sum HEALTH_CENTER_VL if TREATED==0 & POST==1
scalar AVG_Y_0_1 = r(mean)
* g=0, t=1
sum HEALTH_CENTER_VL if TREATED==0 & POST==0
scalar AVG_Y_0_0 = r(mean)
*

matrix Table_1_`ele' = [AVG_Y_1_1, AVG_Y_0_1, (AVG_Y_1_1 - AVG_Y_0_1)\ ///
                  AVG_Y_1_0, AVG_Y_0_0, (AVG_Y_1_0 - AVG_Y_0_0)\ ///
				  (AVG_Y_1_1-AVG_Y_1_0), (AVG_Y_0_1-AVG_Y_0_0), ((AVG_Y_1_1-AVG_Y_1_0)-(AVG_Y_0_1-AVG_Y_0_0))]

matrix rownames Table_1_`ele' = "Post=1" "Post=0" "Difference 1"
matrix list Table_1_`ele'
esttab matrix(Table_1_`ele') using "Tables\Table_1.tex", title("Table 1") collabels("Treated=1" "Treated=0" "Difference 2")  append


restore

}



/*
We see that the effect of school construction is a rise in probability of having a public health center in the village after 1992 by about 0.79 percentage points in the 1992-93 group of villages, by 2.22 in the  1994-96 group and by 1.70 in the 1997-2000 group. The low effect on the first group of vllages, compared to the other two, can be explained by the fact that very few cohorts affected by the school construction program could run in elections of 1992-93, and thus the effect of the program is likely to be lower in size right after 1992 (a phenomenon noticed by Martinez and Bravo as well).
*/
}

***************************
**# D #4
***************************
{
	
*(i)
reg HEALTH_CENTER_VL POST_TREATED POST TREATED, vce(cluster idkab_num)
outreg2 using "Tables\Table_2.tex", replace tex lab title("Table 2") ctitle("Pooled OLS") br bdec(4) addtext(Year FEs, NO, Village FEs, NO, Cluster, YES)

*We obtain the same result of point C (i.e. the effect is an increase in probability by about 1.22 percentage points).

*(ii)
xtreg HEALTH_CENTER_VL POST_TREATED POST i.year_num, fe i(v_id) vce(cluster idkab_num)
outreg2 using "Tables\Table_2.tex", append tex lab drop(i.year_num) title("Table 2") ctitle("xtreg") br bdec(4) addtext(Year FEs, YES, Village FEs, YES, Cluster, YES)

/*We cannot include TREATED because of multicollinearity. Treatment status does not vary over time and including village fixed effects eliminates all time constant variation in village specific variables. Hence, including TREATED and village FE leads to perfect multicollinearity.
*/

*(iii)
areg HEALTH_CENTER_VL POST_TREATED POST i.year_num, absorb(v_id) vce(cluster idkab_num)
outreg2 using "Tables\Table_2.tex", append tex lab drop(i.year_num) title("Table 2") ctitle("areg") br bdec(4) addtext(Year FEs, YES, Village  FEs, YES, Cluster, YES)

*We obtain the same results as those found in the FE model estimated with xtreg.

/*It is worth noticing that, by using a within-group estimator, we obtain an effect on the probability of having a public health center of about 1.67 percentage points, which slightly differs from that found with a pooled regression of about 1.22 percentage points. This difference can be explained by considering the different assumptions which the two methods require to provide a consistent estimator. Indeed, the pooled OLS, like the RE model,
needs to assume that the time-invariant unobserved variables are uncorrelated with the regressors (in this case, they are post_treated,
treated and post) in each period (Woolridge, 2010). Conversely, the FE model, which we estimate through a within-group estimator, allows for 
this correlation to exist (even though it still requires strict exogeneity). Therefore, the difference in the two estimates suggests that a certain degree of correlation exists between the unobserved time-invariant variables and the regressors. 
*/

*As required, in all the regressions we use clustered standard errors at the district level to avoid biased SE.

}

***************************
**# E #5
***************************
{
*Note that from now we use all the possibile specification for fe model.
*Since the regression require many FEs, we employ a within-group model (we follow Wooldridge(2010), who argues that FE is generally more efficient than FD, under the assumption of no serial correlation in the errors). 
	
gen INTENSITY = num_PSINPRES1980
gen POST_INTENSITY = POST*INTENSITY

local year_FEs = "i.year_num"
local village_FEs = "i.v_id"

*using the standard xtreg command
xtreg HEALTH_CENTER_VL POST_INTENSITY POST `year_FEs', fe i(v_id) vce(cluster idkab_num)

*equivalently with the areg command
areg HEALTH_CENTER_VL POST_INTENSITY POST `year_FEs', absorb(v_id) vce(cluster idkab_num)

*alternatively with the reghdfe command
reghdfe HEALTH_CENTER_VL POST_INTENSITY POST, absorb(v_id year) vce(cluster idkab_num)


/*
The regression just implemented differs from that seen in points d.ii and d.iii because the treatment is not discrete, but rather we distinguish the effect of having one or two INPRES schools built in the village. Hence, we only expect that results are the same if treatment intensity does not matter. Yet, making the plausible assumption that higher treatment intensity (i.e. two INPRES schools built) is associated with larger treatment effects, we expect different results compared to the discrete case. 

Results: Both the xtreg and areg regressions show that having one INPRES school built in the village increases the probability of having a health center after 1992 by 1.20 percentage points on average. Therefore, villages with two schools built are 2.40 percentage points more likely to have a health center after 1992 than those villages not affected by INPRES program (and 1.20 percentage points more likely than those where just one school is built).
*/

*exploring heterogeneity in intensity
xtreg HEALTH_CENTER_VL i1.POST#i(1/2)INTENSITY POST `year_FEs', fe i(v_id) vce(cluster idkab_num)

*Allowing the treatment effects to differ by intensity level, we see that the implicit assumption of linear treatment effects is plausible. The effect of having two INPRES schools built is approximately twice the size of having just one INPRES school built. However, the treatment effect does not seem to be exactly linear. The results suggest slightly decreasing returns to scale. 
}

***************************
**# F #6
***************************
{


tab year, gen(years)
rename years1 y_1986
rename years2 y_1990
rename years3 y_1993
rename years4 y_1996
rename years5 y_2000
rename years6 y_2003

local census_years "1990 1993 1996 2000 2003"

foreach v in `census_years' {
	gen INTENSITY_`v' = INTENSITY*y_`v'
	local temp = "INTENSITY_`v'"
	local intensities `intensities' `temp'
}

local year_FEs = "i.year_num"
local village_FEs = "i.v_id"

local elections "1993 1996 2000"
local cond_1993 "if (ele1v_post92 == 1992 | ele1v_post92 == 1993)"
local cond_1996 "if (ele1v_post92 == 1994 | ele1v_post92 == 1995 | ele1v_post92 == 1996)"
local cond_2000 "if (ele1v_post92 == 1997 | ele1v_post92 == 1998 | ele1v_post92 == 1999 | ele1v_post92 == 2000)"	

foreach ele in `elections' {
	
areg HEALTH_CENTER_VL `intensities' `year_FEs' `cond_`ele'', absorb(v_id) vce(cluster idkab_num)

reghdfe HEALTH_CENTER_VL `intensities' `cond_`ele'', absorb(v_id year) vce(cluster idkab_num)

}

/*
To assess whether there is a significant effect of the treatment variable during years before the treatment, we should check whether the coefficient of INTENSITY has a significant impact on outcome (i.e., public good provision) in the census years before the first election after 1992 (i.e., the first election affected by the INPRES program). 

For the first group of villages, we check wheteher the coefficient associated with INTENSITY is significant in 1990. Clearly, as we can see from the high p-value, this is not the case (i.e., the estimated coefficient is not significantly different from zero). Moreover, effect sizes are very small.

Similarly, for the second group of villages, both the coefficients for INTENSITY effect in 1990 and 1993 are very small and not statistically significant. Yet, it should be noted that the sample size in the first and second group is lower than in the third group which makes it very difficult to achieve statistical significance for very small effects. Based on that, also effects after the respective treatment years are not statistically significant. However, effect sizes are considerably larger. That is why we do not only comment on statistical significance but also on effect size.

Finally, in the third group of villages, the effect for 1990 and 1993 are all very small not statistically different from zero. However, the effect in 1996 is 0.66 percentage points and statistically significant at the 5 percent level. Although the effect is smaller in size than in the years after, this is reason for doubt. Yet, Martinez-Bravo (2017) only estimates the election year and does not know the election year precisely. So, it could be that some villages in the third group had elections already before the expected date and thus also have earlier treatment effects. 

Generally, these results argue in favor of an absence of pre-treatment trends, thus supporting the parallel trend assumption. However, the effect in 1996 for the third group indicates that the pre-trends are partly different in the third group which is a threat to the identifying assumption of parallel trends. 
*/
}

***************************
**# G #7
***************************
{
*Since there are different gaps between the years, we can't compute the one-year lagged variable. So we create a year-rank variable (year_num generated at the beginning of the do file) to simulate time trend within village


local year_FEs = "i.year_num"
local village_FEs = "i.v_id"

*using the standard xtreg command
xtreg HEALTH_CENTER_VL l.HEALTH_CENTER_VL POST_INTENSITY POST `year_FEs', fe i(v_id) vce(cluster idkab_num)

*equivalently with the areg command
areg HEALTH_CENTER_VL l.HEALTH_CENTER_VL POST_INTENSITY POST `year_FEs', absorb(v_id) vce(cluster idkab_num)

*alternatively with the reghdfe command
reghdfe HEALTH_CENTER_VL l.HEALTH_CENTER_VL POST_INTENSITY POST, absorb(v_id year) vce(cluster idkab_num)

/*
In a model like this one, which includes lagged outcome as a regressor, the Nickell Bias may arise. 
Here, the bias is due to the fact that the elements of the model are demeaned and this cause the residuals of 
the transformed (demeaned) model to be correlated with one of the regressor (i.e., the demeaned lagged outcome). 

Analytically, this can be explained by considering that, in the trasformed model, one of the regressors is given
by the difference between outcome in t-1 and mean outcome, which depends on the outcome of periods from 1 to t-1. 
In the same regression, the transformed residuals are given by the difference between the residuals at time t and
their mean, which depends on residuals from time 1 to t. Since, for each period s between 1 and t-1, y_s is correlated
with residuals_s, the demeaned lagged outcome and the demeaned residuals are clearly correlated. 

Sometimes, the context and the model specification require the introduction of lagged outcome as a regressor, as in this case,
where it is intuitive (and also showed by the significant estimator in the regression above), that past presence of a health center 
in the village is correlated with the chances of having a health center in the village after the first election post-1992. 
In these cases, some solutions can be implemented to tackle the risk on biased estimates. 
One possibility is to estimate two separate regressions, one with only FE, the other with only lagged outcome, in order to have
bounds such that the real effect can be considered to be in between.
Another possibility is to instrument lagged outcome with further lags of the dependent variable, which are far enough in time 
so that they do not suffer from correlation with present or almost-present values of the residuals.
*/
}

***************************
**# H #8
***************************
{

local year_FEs = "i.year"
local village_FEs = "i.v_id"


xtreg HEALTH_CENTER_VL POST_INTENSITY POST  i(2/6).year_num#(c.num_bank_pre i1.pedati_pre i1.dum_otrocop_pre) `year_FEs', fe i(v_id) vce(cluster idkab_num)

areg HEALTH_CENTER_VL POST_INTENSITY POST  i(2/6).year_num#(c.num_bank_pre i1.pedati_pre i1.dum_otrocop_pre) `year_FEs', absorb(v_id) vce(cluster idkab_num)

reghdfe HEALTH_CENTER_VL POST_INTENSITY POST  i(2/6).year_num#(c.num_bank_pre i1.pedati_pre i1.dum_otrocop_pre), absorb(v_id year) vce(cluster idkab_num)


******* Now create a table to compare results *******

reghdfe HEALTH_CENTER_VL POST_INTENSITY POST, absorb(v_id year) vce(cluster idkab_num)
outreg2 using "Tables\Table_Comparison.tex", replace tex lab nocons keep(POST_INTENSITY) title("Point H: Comparisons between Diff-in-Diff") ctitle("DiD with Intensities") br bdec(4) addtext(Village FEs, YES, Year FEs, YES, Cluster, YES, Lagged Variable, NO, Baseline Controls, NO)

reghdfe HEALTH_CENTER_VL POST_INTENSITY POST  i(2/6).year_num#(c.num_bank_pre i1.pedati_pre i1.dum_otrocop_pre), absorb(v_id year) vce(cluster idkab_num)
outreg2 using "Tables\Table_Comparison.tex", append tex lab nocons keep(POST_INTENSITY) title("Point H: Comparisons between Diff-in-Diff") ctitle("Base Controls") br bdec(4) addtext(Village FEs, YES, Year FEs, YES, Cluster, YES, Lagged Variable, NO, Baseline Controls, YES)

reghdfe HEALTH_CENTER_VL l.HEALTH_CENTER_VL POST_INTENSITY POST, absorb(v_id year) vce(cluster idkab_num)
outreg2 using "Tables\Table_Comparison.tex", append tex lab nocons keep(POST_INTENSITY) title("Point H: Comparisons between Diff-in-Diff") ctitle("Lagged Variable") br bdec(4) addtext(Village FEs, YES, Year FEs, YES, Cluster, YES, Lagged Variable, YES, Baseline Controls, YES)


	
/*We see that results are robust to the inclusion of unbalanced baseline controls.
Indeed, we see that the average increase in probability of public good provision 
after 1992 amounts to about 1.21 percentage points for each INPRES school built, a result consistent with that found in point E. This result is in line with that by Martinez and Bravo. Moreover, we see that only the presence of village cooperatives, interacted with years, has an estimated effect on the outcome which significantly differs from zero, even though it's size is low.

In the previous point we try to verify whether our results are robust to the inclusion of health center presence in the village in the previous period as a regressor. Indeed, it is highly likely that, while keeping the number of INPRES school built constant, the probability of having a health center in the village after 1992 is higher when a health center was already present in the previous period. 
Formally, we verify this through the regression in point G and, indeed, we see that lagged outcome has a positive significant impact. Not surprisingly, by including lagged outcome, we see that this regressor explains part of the effect previously linked to the INPRES program. As a result, now the realisation of a INPRES school raises the probability of having a public health center afer 1992 by about 0.73%, rather than by about 1.20% as 
in point E. However, it is worth noticing that the effect of INPRES program is still positive and statistically significant.
*/

/*NOTE: as highlighted by the recent literature on DiD, since the covariates considered change over time, we 
should make additional assumptions, which imply requiring that the unbalanced covariates (i.e., in this case, the availability 
of a horse-drawn cart in the village, the presence of village cooperatives, and the number of banks) display no significant pre 
treatment trends before treatment. In the present setting, this assumption is likely to be violeted. 
To tackle the issue, a solution would be to implement a doubly-robust DiD estimator.*/

}

***************************
**# I #9
***************************
{
	
gen PLACEBO_POST = (year>=1990)
gen PLACEBO_POST_INTENSITY = PLACEBO_POST*INTENSITY

*Since there are just two periods, we do not add year fe becase of collinearity

xtreg HEALTH_CENTER_VL PLACEBO_POST_INTENSITY PLACEBO_POST if year < 1993, fe i(v_id) vce(cluster idkab_num)

/*
The results show an estimator for the fake treatment variable which is not statistically significant different from zero, arguing in favor of the validity of general estimation strategy. This robustness check further strenghtens the parallel trend assumption and the assumption of absence on any omitted time-varying variable. Indeed, were the results being due to an omitted time-varying variable, we would have likely found a significant effect of INPRES program also before the first round of election before 1992. Conversely, since this is not the case, we can say that the probability that results are due to the presence of a time-varying omitted variable is low.
*/
}

***************************
**# J #10
***************************
{
/*
As Theorem 1 of de Chaisemartin and D'haultfoeuille (2020) shows, the two-way fixed effects (TWFE) estimator is misleading even if the parallel trend assumption holds. This is the case because the TWFE estimator is equal to the expectation of a weighted average of the treated cells' average treatment effects. However, some of these weights can be negative for some groups and periods. So, given negative weights, negative group-time treatment effects could enter the TWFE estimator positively (or positive group-time treatment effects could enter negatively). This applies to settings where treatment effects are heterogeneous across groups or over time which is very plausible in the Martinez-Bravo (2017) setting. 

Hence, a priori we cannot say that the TWFE estimates that we have computed before can be interpreted as the ATE.
*/

}

***************************
**# L #11
***************************
{
*estimating TWFE model again and saving beta coefficient
xtset v_id year
areg HEALTH_CENTER_VL POST_TREATED POST i.year, absorb(v_id) vce(cluster idkab_num)
scalar beta_fe=_b[POST_TREATED]

*computing TWFE weights with twowayfeweights command
twowayfeweights HEALTH_CENTER_VL v_id year POST_TREATED, type(feTR) controls(POST) path("DTA\weights.dta")
ereturn list
scalar sigma_fe1 = e(lb_se_te)
*save sigma_fe computed by the twowayfeweights command
di sigma_fe1

*compute sigma_fe manually 
preserve
use "DTA\weights.dta", clear
drop if weight==0
tabstat weight, stats(sd mean sum count) save
return list
scalar weights_num = r(StatTotal)[4,1]
scalar sd_weights = r(StatTotal)[1,1]*weights_num
scalar sigma_fe2 = abs(beta_fe)/sd_weights
di sigma_fe2
restore
*note that the manual computation yields the same result.

*The ratio between (i) the absolute value of the two-way fixed effects estimate computed in point (D) and (ii) the standard deviations of the weights associated to each (village, year) cell within the Martinez-Bravo (2017) setting is 0.06796258.
}

***************************
**# M #12
***************************
{
/*
The TWFE estimator is a weighted sum of 15,583 ATTs, of which 0 receive negative weights. Hence, the TWFE estimator and the "actual" ATT cannot be of opposite signs. Moreover, the estimator of the simga_fe (the ratio computed in L) is 0.068. This means that, even if negative weights were found, it would require a very large heterogeneity in group-time treatment effects to have opposite signs between the TWFE estimator and the ATT. 

Hence, treatment effect heterogeneity is less of a concern and we are confident about the sign of the TWFE estimator.

Furthermore, even if treatment effects are perfectly homogeneous among the treated groups, it must be noted that TWFE estimators does also not identify the ATE per se. They capture merely the ATT. Yet, in the given setting, ATT and ATE are arguably very similar. Martinez-Bravo (2017) argues convincingly that village elections and their interaction with school construction intensity are quasi-random. Therefore, we can assume that there is no selection bias; implying that, if the TWFE estimator identifies the ATT, it is also similar to the ATE. Still, it could be the case that untreated villages would have had different treatment effects when treated than treated villages which would establish a difference between ATT and ATE.

It is practically impossible that village-year treatment effects are perfectly homogenous. Additionally, exploring the weights computed in (L), we see that not all village-year treatment effects receive the same weight in the TWFE estimator. Meanwhile the ATT, as defined by de Chaisemartin and D'haultfoeuille (2020), would equally weight all village-year treatment effects across all treated. Hence, given some treatment effect heterogeneity, the TWFE estimator cannot equal the ATT exactly because of its different weighting structure. 

Overall, given no negative weights and and a large sigma_fe, we can conclude that the TWFE estimator has the correct sign, is robust and plausibly is very close to the ATT and therefore also close the ATE. 
*/
}
