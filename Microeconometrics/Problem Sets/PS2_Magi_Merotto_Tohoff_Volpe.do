clear all
global alessandro "C:\Users\magia\OneDrive - Università Commerciale Luigi Bocconi\AA Second Semester\Microeconometrics\Microeconometrics_Stata\Problem Set\PS 2\Alessandro"
global danilo "/Users/danilovolpe/Desktop/Micrometrics/PS2"
global lukas "C:\Users\lukas\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 2\Lukas"
global anna "C:\Users\anna\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 2\Anna"

cd "$alessandro"

**********************************************************************
***************************** Exercise 1 *****************************
**********************************************************************
{
********************************************************
********************** Question 1 **********************
********************************************************

***********************************
**************** A ****************
***********************************

use "DTA\pset_2_q_1.dta"

preserve

egen educ_mean=mean(Education), by(birthdate)
duplicates drop birthdate, force

* Figure 1
twoway connected educ_mean birthdate if inrange(birthdate, 1930, 1939.75), ///
sort (birthdate) ytitle("Years of completed education", size(medsmall) height(5) orient(vertical)) ///
ylab(11.8(0.2)13) xtitle("Year of birth", size(medsmall) height(7)) ///
lcolor(black) graphregion(fcolor(white)) plotregion(style(outline)) mcolor(black) msymbol(square) ///
mlabel(birthqtr) mlabposition(6)  mlcolor(black) mlabcolor(black) ///
caption("FIGURE I" "Years of Education and Season of Birth" "1980 Census""Note. Quarter of Birth is listed below each observation.", size(vsmall) height(10) position(6))
graph export "Tables\Figure1.png", replace
putexcel set "Tables\Figures.xls", sheet("Figure1") modify
putexcel A1 = picture("Tables\Figure1.png")


* Figure 2
twoway connected educ_mean birthdate if inrange(birthdate, 1940, 1949.75), ///
sort (birthdate) ytitle("Years of completed education", size(medsmall) height(5) orient(vertical)) ///
ylab(12.6(0.2)13.6) xtitle("Year of birth", size(medsmall) height(7)) ///
lcolor(black) graphregion(fcolor(white)) plotregion(style(outline)) mcolor(black) msymbol(square) ///
mlabel(birthqtr) mlabposition(6)  mlcolor(black) mlabcolor(black) ///
caption("FIGURE II" "Years of Education and Season of Birth" "1980 Census""Note. Quarter of Birth is listed below each observation.", size(vsmall) height(10) position(6))
graph export "Tables\Figure2.png", replace
putexcel set "Tables\Figures.xls", sheet("Figure2") modify
putexcel A1 = picture("Tables\Figure2.png")



* Figure 3
twoway connected educ_mean birthdate if inrange(birthdate, 1950, 1959.75), ///
sort (birthdate) ytitle("Years of completed education", size(medsmall) height(5) orient(vertical)) ///
ylab(11.8(0.2)13.4) xtitle("Year of birth", size(medsmall) height(7)) ///
lcolor(black) graphregion(fcolor(white)) plotregion(style(outline)) mcolor(black) msymbol(square) ///
mlabel(birthqtr) mlabposition(6)  mlcolor(black) mlabcolor(black) ///
caption("FIGURE III" "Years of Education and Season of Birth" "1980 Census""Note. Quarter of Birth is listed below each observation.", size(vsmall) height(10) position(6))
graph export "Tables\Figure3.png", replace
putexcel set "Tables\Figures.xls", sheet("Figure3") modify
putexcel A1 = picture("Tables\Figure3.png")

restore

*The exogeneity of the instrument is debatable. Indeed, literature has highlighted reasons to doubt the fact that this instrument is randomly assigned. For instance, Bound et al. (1995) and Bound and Jaeger (1996) have found that the probability of borning during spring is higher for children from high-income families and this holds true also controlling for racial and regional disparities.
*More in general, we can arguably maintain that a child's birth season may likely be affected by environmental circumstances or by parents' will. This, in turn, suggests a link between children's birth season and some of the socio-economic factors which relate to the characteristics of their family and/or of the context where they grow up. All these elements may impact children's socio-economic conditions and, eventually, health, later in life.
*Therefore, there are reasons to suspect that the exogeneity of the instrument is at least debatable.


***********************************
**************** B ****************
***********************************

*Angrist and Krueger (1991) and the subsequent academic debate on the validity of the season of birth as an instrument in wage equations can be used to discuss the validity of the exclusion constraint.
* Indeed, some of the issues that arise in Angrist and Krueger's (1991) framework are likely to exist in a health status model as well. 
*Bound and Jaeger (1996) review a large body of evidence suggesting that a child's birth date is linked to personality, mental and physical health, behavior disorders, and school achievement, all of which can have an impact on health outcomes.
*All of this, together with the concerns stated concerning the parental background link in the exogeneity debate, leads us to doubt the notion that the quarter of birth has an effect on subsequent health outcomes solely through the educational channel. As a result, we doubt that the exclusion rule applies in this case.

* Potential violations of the exogeneity assumption and the exclusion constraint are emphasized in critiques edited by Bound et al. (1995) and Bound and Jaeger (1995) in Angrist and Krueger's (1991) setting. 
*The argument previously presented regarding the potential lack of randomness in the quarter of birth allocation constitutes a likely violation of exogeneity in Angrist and Krueger's (1991) paradigm as well, because the instrument used is the same (seasonality of birth).

*Furthermore, as previously stated, the potential correlation between the instrument and other variables other than education that were omitted from the model (personality, mental and physical health, behavior disorders, school performance, and so on) could lead to a violation of the exclusion restriction assumption, as these variables could represent alternative channels through which a child's quarter of birth influences his or her subsequent earnings.

***********************************
**************** C ****************
***********************************

*We expect to notice a positive bias in the OLS estimates compared to the real causal parameter, and the TSLS estimates should be a better estimate (at least locally) if the necessary assumptions are met (see discussion above).
*Indeed, we can reasonably expect a number of factors, such as parental income and social background, parents' health, and preferences toward children's life outcomes (e.g. willingness to invest in children's future in terms of education and physical/mental wellbeing), to have a positive impact on a child's health and educational attainment.
*Pre-existing medical issues are also a significant role in determining the strength of the association, as reverse causation is a possibility (higher levels of health leading to more and better schooling).
*Because a simple OLS coefficient from a regression of health status on education captures all of these components, we anticipate to discover a higher OLS estimate than the IV parameter, which should solely represent the effect of exogenous variation in education on subsequent health outcomes.
*However, there are two things to consider. First, as previously stated, the validity of instrumental variable analysis is based on the instrument's randomization and exclusion restriction assumptions (as well as the strength of the first stage, which we can test anyway). Our expectations concerning the OLS-IV discrepancy may not materialize or even be reversed if such unstable assumptions are not checked.
*Second, it's important to remember that IV estimations reflect a local treatment effect, that is, a treatment impact that is conditional on the level of the variables of those units whose treatment takeup is changed by the instrument (see the subsequent discussion on compliers).
*On the other hand, OLS estimates capture a relationship that applies to the entire sample under investigation. As a result, if the population to which IV estimates relate has a local treatment impact that is substantially greater than the average treatment effect for the entire population, OLS estimates may end up being lower than this bigger-than-average local treatment effect, while being favorably biased.
*All persons whose educational attainment was influenced by the quarter of their birth are included in the current population of "compliers," which is also the relevant group for which IV estimates are useful.
*In particular, these are the individuals who intended to leave school as soon as legally possible, so that being born in the first quarter of a year rather than in the last one had an effect (through the mechanism described by Angrist and Krueger (1991)) on the total amount of schooling they ended up receiving. 
*IV estimations, on the other hand, would not be useful for people who would have gone beyond the minimal school requirements regardless, since the season in which they were born (the instrument) had no bearing on this.


********************************************************
********************** Question 2 **********************
********************************************************

***********************************
**************** A ****************
***********************************

use "DTA\pset_2_q_2_and_3.dta", clear

sum Healthy, mean
scalar mu_y=r(mean)

sum Education, mean
scalar mu_x=r(mean)

***********************************
**************** B ****************
***********************************

qui tab birthqtr, gen(Quarter)
qui tab region, gen(Region)

local Controls "Central Married Region*"
local Birth_Year_FEs "i.birthyear"


/* Alternative for creating dummy years: We write in this section three different alternatives to create the local for year of birth. The problem with dummy years is that, due to multicollinearity with region controls, we lost one of the dummy year. So we use i.birthyear to set as a baseline year the first one and than we estimate a model without a constant. In this way we will not lose any region and we will only lose the baseline year



*** Alternative 1 ***
levelsof birthyear, local(Birth_Year) 
local Birth_Year_FEs
foreach year_num in `Birth_Year' {

	gen d_`year_num'=0
	replace d_`year_num'=1 if birthyear==`year_num'
	local Birth_Year_FEs `Birth_Year_FEs' d_`year_num'
}
sum `Birth_Year_FEs'


*** Alternative 2 ***
qui tab region, gen(birthyear)
local Birth_Year_FEs "birthyear*"

*** Alternative 3 ***
local Birth_Year_FEs "ibn.birthyear"

reg Healthy Education `Controls' `Birth_Year_FEs', nocons
reg Healthy Education `Controls' ibn.birthyear, nocons
reg Healthy Education `Controls' i.birthyear, nocons

reg Healthy Education `Controls' `Birth_Year_FEs'
reg Healthy Education `Controls' ibn.birthyear
reg Healthy Education `Controls' i.birthyear

*/


label var Education "Years of education"

***************************************
**************** C - D ****************
***************************************

reg Healthy Education
outreg2 using "Tables\TABLE_Q_2.xls", replace excel lab keep(Education) nocons title("TABLE_Q_2 - Dep. variable (y): Healthy") ctitle("OLS") br bdec(4) addtext(Controls, NO, Year of birth FEs, NO) addstat("Mean y", mu_y, "Mean x", mu_x)

reg Healthy Education `Controls', nocons
outreg2 using "Tables\TABLE_Q_2.xls", append excel lab keep(Education) title("TABLE_Q_2 - Dep. variable (y): Healthy") ctitle("OLS") br bdec(4) addtext(Controls, YES, Year of birth FEs, NO) addstat("Mean y", mu_y, "Mean x", mu_x)

reg Healthy Education `Controls' `Birth_Year_FEs', nocons
outreg2 using "Tables\TABLE_Q_2.xls", append excel lab keep(Education) title("TABLE_Q_2 - Dep. variable (y): Healthy") ctitle("OLS") br bdec(4) addtext(Controls, YES, Year of birth FEs, YES) addstat("Mean y", mu_y, "Mean x", mu_x)

***************************************
**************** E - F ****************
***************************************

ivreg2 Healthy (Education=Quarter1 Quarter2 Quarter3), first
outreg2 using "Tables\TABLE_Q_2.xls", append excel lab keep(Education) nocons title("TABLE_Q_2 - Dep. variable (y): Healthy") ctitle("IV") br bdec(4) addtext(Controls, NO, Year of birth FEs, NO) addstat("F-statistic IVs", e(widstat))

ivreg2 Healthy `Controls' (Education=Quarter1 Quarter2 Quarter3), first nocons
outreg2 using "Tables\TABLE_Q_2.xls", append excel lab keep(Education) title("TABLE_Q_2 - Dep. variable (y): Healthy") ctitle("IV") br bdec(4) addtext(Controls, YES, Year of birth FEs, NO) addstat("F-statistic IVs", e(widstat))

ivreg2 Healthy `Controls' `Birth_Year_FEs' (Education=Quarter1 Quarter2 Quarter3), nocons first
outreg2 using "Tables\TABLE_Q_2.xls", append excel lab keep(Education) title("TABLE_Q_2 - Dep. variable (y): Healthy") ctitle("IV") br bdec(4) addtext(Controls, YES, Year of birth FEs, YES) addstat("F-statistic IVs", e(widstat))


********************************************************
********************** Question 3 **********************
********************************************************

	


***********************************
**************** A ****************
***********************************

reg Healthy Education `Controls' `Birth_Year_FEs', nocons
outreg2 using "Tables\TABLE_Q_3.xls", replace excel lab keep(Education) nocons title("TABLE_Q_3") ctitle("OLS") br bdec(4) addtext(Controls, YES, Year of birth FEs, YES)

***********************************
**************** B ****************
***********************************

ivreg2 Healthy `Controls' `Birth_Year_FEs' (Education=Quarter1 Quarter2 Quarter3), nocons first savefirst
return list
ereturn list
scalar F_stat = e(widstat)
est restore _ivreg2_Education
outreg2 using "Tables\TABLE_Q_3.xls", excel append keep(Quarter*) nocons title("TABLE_Q_3") ctitle("IV First Stage") br bdec(4) addtext(Controls, YES, Year of birth FEs, YES) addstat("F-statistic IVs", F_stat)

***********************************
**************** C ****************
***********************************

reg Healthy Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs' if Education!=., nocons
outreg2 using "Tables\TABLE_Q_3.xls", append excel lab keep(Quarter*) nocons title("TABLE_Q_3") ctitle("Reduced Form") br bdec(4) addtext(Controls, YES, Year of birth FEs, YES)

*Looking only at the results from 2.e we cannot say anything about the signs of the coefficient. Indeed the 2SLS coefficient is the result of the product between the reduced form coefficient end second stage regression coefficient. Anyway from the figure we can see that there is a negative relation beetween the QOB and Education, this, additional to the fact that from 2.e the IV coefficient is positive, lead us to think that the reduced form coefficient are negative. Morover, from the figure we can see that there are little differences between fourth and third quarter, this will lead us to think that the coefficient of reduced form will be near to 0.

*Yes! They are in line with the expecation.

***********************************
**************** D ****************
***********************************

ivreg2 Healthy `Controls' `Birth_Year_FEs' (Education=Quarter1 Quarter2 Quarter3), first nocons
outreg2 using "Tables\TABLE_Q_3.xls", excel append keep(Education) nocons title("TABLE_Q_3") ctitle("IV Second Stage") br bdec(4) addtext(Controls, YES, Year of birth FEs, YES)

***********************************
**************** E ****************
***********************************

*Bound et al. (1995) draw attention to the finite sample bias of IV estimates in case of finite sample when the R^2 of the first stage approaces 0 (ie. week correlation between endogenous and instrument). The finite sample bias of IV estimates is due to a slightly endogeneity of the instruments which create a bias that is amplified by the weakness of the instrument. Following the paper, we can assess the weakness of the instrument using the F-test on the excluded instuments which is above 60 and which allow us to accept the alternative hypotesis of joint significance. Furthermore, given the rule of thumb of 10 as a treshold of F-statistic, we can exclude the possibility of weakness of instruments

***********************************
************** F - G **************
***********************************

use "DTA\pset_2_q_2_and_3.dta", clear

qui tab birthqtr, gen(Quarter)
qui tab region, gen(Region)

local Controls "Central Married Region*"
local Birth_Year_FEs "ib(last).birthyear"

local State_FEs "ib(last).bpl"
local Year_Quarter_FEs "b(last).birthyear#b(last).birthqtr"
local State_Quarter_FEs "b(last).bpl#b(last).birthqtr"

ivreg2 Healthy `Controls' `Birth_Year_FEs' `State_FEs' (Education= `State_Quarter_FEs'), nocons

*We redefine the locals to tell stata to use last year last quarter as baseline. In the following regression last year dummy and all the quarters are dropped because of multicollinearity
ivreg2 Healthy `Controls' `Birth_Year_FEs' (Education= `Year_Quarter_FEs'), nocons
scalar F_Test_YQ = e(widstat)

*Since Wyoming is the last state in the list (which is a numerical varible), adding b(last) is telling stata to use it as baseline
ivreg2 Healthy `Controls' `Birth_Year_FEs' `State_FEs' (Education= `State_Quarter_FEs'), nocons
scalar F_Test_SQ = e(widstat)

***********************************
**************** H ****************
***********************************

display F_Test_SQ 
*3.4905826

display F_Test_YQ
*8.0722064

*The F-Test of the two previous regression is below 10 (the rule of thumb previously stated). This might be problematic in case of correlation between outcome and instrument. Indeed, week instruments will inflate the bias of IV even if we use large sample.
*In this specific case, the F-Test is low because of the presence of many instruments, which, in the case mentioned, could be problematic

}

**********************************************************************
***************************** Exercise 2 *****************************
**********************************************************************
{
clear all
global alessandro "C:\Users\magia\OneDrive - Università Commerciale Luigi Bocconi\AA Second Semester\Microeconometrics\Microeconometrics_Stata\Problem Set\PS 2\Alessandro"
global danilo "/Users/danilovolpe/Desktop/Micrometrics/PS2"
global lukas "C:\Users\lukas\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 2\Lukas"
global anna "C:\Users\anna\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 2\Anna"

cd "$alessandro"
set matsize 2000

********************************************************
********************** Question 1 **********************
********************************************************

***********************************
**************** A ****************
***********************************


*First, notice that the instrument used by Autor et al. (2013) follows into the definition of a Bartik-like instrument (see Goldsmith-Pinkham, Sorkin, and Swift (2020), page 2590). Then, as argued by Goldsmith-Pinkham, Sorkin, and Swift (2020), its consistency is assured when two conditions are satisfied. We state these conditions in terms of employment shares and such formulation makes sense when applied to Autor et al. (2013), as argued by Goldsmith-Pinkham, Sorkin, and Swift (2020, appendix) in A.4.
*The first necessary set of conditions is formally stated by assumption 1, page 2597 of Goldsmith-Pinkham, Sorkin, and Swift (2020) and it requires relevance of the instrument. In the context of Author et al. (2013), this assumption can be intuitively phrased by requiring that there must be a non-US high-income market and time period such that the realized imports from China to that high-income market have predictive power to the contemporaneous change in Chinese import exposure per worker in a US region, conditional on controls. In addition to this, it is also necessary that the weights on which the Bartik instrument is built, namely the lagged shares of the industry-region-time specific employment on the total employment of that industry and period, do not weigh the place and time-specific covariances in a way such that they exactly cancel out. 
*The second assumption, which is necessary for consistency, is the exclusion restriction. Namely, it requires that the realized imports from China to other high-income countries are uncorrelated with the structural error terms, conditional on the controls. Uncorrelation must hold for all industries that have nonzero weight. 

***********************************
**************** B ****************
***********************************

*If we assume that import competition has an effect that is heterogeneous over the place and/or time, some additional assumptions are necessary. Indeed, as reported by Goldsmith-Pinkham, Sorkin, and Swift (2020), the Bartik instrument is the combination of unordered instruments (to see this in the example proposed by the authors, notice that when the share of an industry increases, it can raise the predicted growth rates in some locations and decrease them in others). Therefore, allowing for unrestricted heterogeneity can produce estimates difficult to interpret. Thus, assumptions are necessary, which impose a restricted form of linear heterogeneity. 
*Goldsmith-Pinkham, Sorkin, and Swift (2020) do so first by expanding the model to include location-specific coefficients. In addition and considering Autor et al (2013) setting, it would be necessary to assume that there is a linear relation between location-time-specific changes in US imports from China and location-specific change in Chinese import exposure per worker. This assumption also implies that there are constant linear effects within a given location. 
*Following this, it is necessary to assume a sort of monotonicity. Namely, it is assumed that for each industry, the location-industry-specific first stage coefficients have all weakly the same sign. Moreover, it must be assumed that, given location-specific controls, the product of time location-specific changes in US imports from China, residuals and the location-specific coefficient is null in expectation. 
*Given these assumptions, Goldsmith-Pinkham, Sorkin, and Swift (2020) show that IV estimates are interpretable under heterogeneous effects. 

********************************************************
********************** Question 2 **********************
********************************************************

***** CREATING DATASET, RESHAPING AND COMPUTING BARTIK-STYLE INSTRUMENT ********
{
/*** Autor, Dorn, Hanson DATA **/
insheet using "DTA/ADHdata_AKM.csv", clear //Autor et al Data
gen year = 1990 + (t2=="TRUE")*10
drop t2

/*** Borusyak, Hull, Jaravel: SHARES DATA **/ 
merge 1:m czone year using "DTA/Lshares.dta", gen(merge_shares) 
/*** Borusyak, Hull, Jaravel: SHOCKS DATA **/
merge m:1 sic87dd year using "DTA/shocks.dta", gen(merge_shocks)

*labelling (and renaming) variables 
rename ind_share share_emp_ind_bhj_ 
rename g g_
label var sic87dd "standard industrial classification of 1987"
label var g_ "growth of imports from China to other high-income countries"
label var share_emp_ind_bhj_ "industry-location employment share"
label var d_sh_empl_mfg "percentage point change in manufacturing employment rate"

*defining the Bartik-style instrument (1/2): product of industry growth rates of high-income countries and industry-location employment shares
gen z_ = share_emp_ind_bhj_ * g_

*dropping unnecessary variables before reshaping
drop g_emp_ind-g_importsUSA

*reshaping to CZ and year dataset with industries in columns
reshape wide share_emp_ind_bhj_ g_ z_, i(czone year) j(sic87dd) 

*(2/2) defining the Bartik-style instrument: sum of inner products
egen z = rowtotal(z_*) 
}

***** PREPARATION FOR COMPUTATION OF ROTEMBERG WEIGHTS *************************
{
*defining a bunch of locals needed later (I think some locals look unnecessary, but they make it easier to copy the code to other settings)
local controls reg_* l_sh_popedu_c l_sh_popfborn l_sh_empl_f l_sh_routine33 l_task_outsource l_shind_manuf_cbp t2
local weight weight 
local y d_sh_empl_mfg 
local x shock
local z z
local time_var year 
local cluster_var czone 
levelsof `time_var', local(years) 

*local including all industry-location employment shares
local ind_stub share_emp_ind_bhj_ 
*local including all industry growth rates of high-income countries
local growth_stub g_ 

/** g_2141 and g_3761 = 0 for all years **/ //probably we drop variables without any variation
drop g_2141 `ind_stub'2141
drop g_3761 `ind_stub'3761

*creating exclusive industry-location employment shares variables for each yera (1990 and 2000)
*creating a variable indicating the largest industry growth over all industries for every communiting zone
forvalues t = 1990(10)2000 {
	foreach var of varlist `ind_stub'* {
		gen t`t'_`var' = (year == `t') * `var' //
		}
	foreach var of varlist `growth_stub'* {
		gen t`t'_`var'b = `var' if year == `t' 
		egen t`t'_`var' = max(t`t'_`var'b), by(czone) //finding the largest g for each CZ and year
		drop t`t'_`var'b 
		}
	}


*creating dummy variables for each of the nine divisions and each of the 2 time periods
*dropping the base level
tab division, gen(reg_) 
drop reg_1
tab year, gen(t) 
drop t1

*dropping observations without specified CZ (zero observations dropped here)
drop if czone == .

*computing the Bartik-style instruments using each single industry (many instruments)
*computing first stage and saving the F-test
*computing the reduced form and saving the coefficients
foreach var of varlist `ind_stub'* {
	if regexm("`var'", "`ind_stub'(.*)") { //I do not see why this regexm-loop is necessary here? Anyways
		local ind = regexs(1) 
		}
	tempvar temp
	qui gen `temp' = `var' * `growth_stub'`ind' // these are the Bartik-style instruments using each single industry 
	qui regress `x' `temp' `controls' [aweight=`weight'], cluster(czone) //first stage
	local pi_`ind' = _b[`temp'] // and we save the first stage coefficient 
	qui test `temp'
	local F_`ind' = r(F) // we save the first stage F-test
	qui regress `y' `temp' `controls' [aweight=`weight'], cluster(czone) //this is the reduced form
	local gamma_`ind' = _b[`temp'] //saving the reduced form coefficient
	drop `temp'
	}
	
*computing the confidence intervals of the just-identified beta for the five most influential industries
*note: weak instrument robust confidence interval using the method from Chernozhukhov and Hansen (2008) over a range from -10 to 10
foreach var of varlist `ind_stub'3571 `ind_stub'3944 `ind_stub'3651 `ind_stub'3661 `ind_stub'3577 {
	if regexm("`var'", "`ind_stub'(.*)") {
		local ind = regexs(1) 
		}
	tempvar temp
	qui gen `temp' = `var' * `growth_stub'`ind'
	ch_weak, p(.05) beta_range(-10(.1)10)   y(`y') x(`x') z(`temp') weight(`weight') controls(`controls') cluster(czone)
	disp r(beta_min) ,  r(beta_max)
	local ci_min_`ind' =string( r(beta_min), "%9.2f")
	local ci_max_`ind' = string( r(beta_max), "%9.2f")
	disp "`ind', `beta_`ind'', `t_`ind'', [`ci_min_`ind'', `ci_max_`ind'']"
	drop `temp'
	}

*computing the employment population and the standard deviation of industry-location employment shares for each CZ and year
preserve
keep `ind_stub'* czone year `weight'
reshape long `ind_stub', i(czone year) j(ind)
gen `ind_stub'pop = `ind_stub'*`weight'
collapse (sd) `ind_stub'sd = `ind_stub' (rawsum) `ind_stub'pop `weight' [aweight = `weight'], by(ind year)
tempfile tmp
save "DTA/mergefile1.dta", replace
restore
}

***** COMPUTING ROTEMBERG WEIGHTS BASED ON GOLDSMITH-PINKHAM ET AL *************
{
*computing the Rotemberg weights 
{ //copying the bunch of locals again that I can easily re-run this bit
local controls reg_* l_sh_popedu_c l_sh_popfborn l_sh_empl_f l_sh_routine33 l_task_outsource l_shind_manuf_cbp t2
local weight weight 
local y d_sh_empl_mfg 
local x shock
local z z
local time_var year 
local cluster_var czone 
levelsof `time_var', local(years) 
*local including all industry-location employment shares
local ind_stub share_emp_ind_bhj_ 
*local including all industry growth rates of high-income countries
local growth_stub g_ 
}

bartik_weight, z(t*_`ind_stub'*) weightstub(t*_`growth_stub'*) x(`x') y(`y') controls(`controls'  ) weight_var(`weight')

*saving the results of the bartik_weight command in matrices 
mat beta = r(beta)
mat alpha = r(alpha)
mat gamma = r(gam)
mat pi = r(pi)
mat G = r(G)
qui desc t*_`ind_stub'*, varlist
local varlist = r(varlist)

*copying the estimated parameters from the bartik_weight command in an empty dataframe
*this is also where alpha1 comes from, which we must plot in exercise 2.2.a
clear
svmat beta
svmat alpha
svmat gamma
svmat pi
svmat G
}

********************************************************************************
***** 2.2.a. Plotting the Rotemberg weights ************************************
********************************************************************************
{
*Plot the distribution of Rotemberg weights associated to Autor et al. (2013) in a parametric and a non-parametric manner
*we chose do not specify the bandwidth because we are satisfied with the default option
label var alpha1 "Rotemberg weights"
kdensity alpha1, kernel(epanechnikov) normal 
graph export "Tables/exercise2_2a_figure.pdf", replace
}

********************************************************************************
***** 2.2.b. REPLICATION PARTS OF TABLE A1 *************************************
********************************************************************************
{
*preparing variables
gen ind = ""
gen year = ""
local t = 1
foreach var in `varlist' {
	if regexm("`var'", "t(.*)_`ind_stub'(.*)") {
		qui replace year = regexs(1) if _n == `t'
		qui replace ind = regexs(2) if _n == `t'
		}
	local t = `t' + 1
	}

destring ind, replace
destring year, replace
merge 1:1 ind year using "DTA/mergefile1.dta"
gen beta2 = alpha1 * beta1
gen indshare2 = alpha1 * (`ind_stub'pop/`weight')
gen G2 = alpha1 * G1
collapse (sum) alpha1 beta2 indshare2 G2 (mean) G1 , by(ind)
gen agg_beta = beta2 / alpha1
gen agg_indshare = indshare2 / alpha1
gen agg_g = G2 / alpha1
rename ind sic
merge 1:1 sic using "DTA/sic_code_desc"
rename sic ind
keep if _merge == 3
gen ind_name = subinstr(description, "Not Elsewhere Classified", "NEC", .)
replace ind_name = subinstr(ind_name, ", Except Dolls and Bicycles", "", .)

***** Panel A: Weighted Betas by alpha weights *********************************
gen positive_weight = alpha1 > 0
gen agg_beta_weight = agg_beta * alpha1
preserve
collapse (sum) agg_beta_weight alpha1 (mean) agg_beta, by(positive_weight)
egen total_agg_beta = total(agg_beta_weight)
gen share = agg_beta_weight / total_agg_beta
gsort -positive_weight
local agg_beta_pos = string(agg_beta_weight[1], "%9.3f")
local agg_beta_neg = string(agg_beta_weight[2], "%9.3f")
local agg_beta_pos2 = string(agg_beta[1], "%9.3f")
local agg_beta_neg2 = string(agg_beta[2], "%9.3f")
local agg_beta_pos_share = string(share[1], "%9.3f")
local agg_beta_neg_share = string(share[2], "%9.3f")
restore

***** Panel  B: Top 5 Rotemberg Weight Inudstries ******************************
egen agg_beta_weight_sum = sum(abs(agg_beta_weight))
gen share_beta_absweight = abs(agg_beta_weight) / agg_beta_weight_sum

foreach ind in 3571 3944 3651 3661 3577 {
	qui sum alpha1 if ind == `ind'
    local alpha_`ind' = string(r(mean), "%9.3f")
	qui sum agg_g if ind == `ind'	
	local g_`ind' = string(r(mean), "%9.3f")
	qui sum agg_beta if ind == `ind'	
	local beta_`ind' = string(r(mean), "%9.3f")
	qui sum agg_indshare if ind == `ind'	
	local share_`ind' = string(r(mean)*100, "%9.3f")
	tempvar temp
	qui gen `temp' = ind == `ind'
	gsort -`temp'
	local ind_name_`ind' = ind_name[1]
	drop `temp'
	qui sum share_beta if ind == `ind'
    local share_beta_absweight_`ind' = string(r(mean), "%9.3f")
	}

***** Writing to tex file ******************************************************
capture file close fh
file open fh using "Tables/exercise2_2b_table.tex", write replace
file write fh "\toprule" _n

/** Panel A **/
file write fh "\multicolumn{3}{l}{\textbf{Panel A: Estimates of $\beta_{k}$ for positive and negative weights} }\\" _n
file write fh  " & $\alpha$-weighted Sum & Share of overall $\beta$ & Mean  \\ \cmidrule(lr){2-4}" _n
file write fh  " Negative & `agg_beta_neg' & `agg_beta_neg_share' &`agg_beta_neg2' \\" _n
file write fh  " Positive & `agg_beta_pos' & `agg_beta_pos_share' & `agg_beta_pos2' \\" _n

/** Panel B **/
file write fh "\multicolumn{6}{l}{\textbf{Panel B: Top 5 Rotemberg weight industries} }\\" _n
file write fh  " & $\hat{\alpha}_{k}$ & \$g_{k}$ & $\hat{\beta}_{k}$ & 95 \% CI & Ind Share & Share of overall $\beta$  \\ \cmidrule(lr){2-7}" _n
foreach ind in 3571 3944 3651 3661 3577 {
	if `ci_min_`ind'' != -10 & `ci_max_`ind'' != 10 {
		file write fh  "`ind_name_`ind'' & `alpha_`ind'' & `g_`ind'' & `beta_`ind'' & (`ci_min_`ind'',`ci_max_`ind'')  & `share_`ind'' & `share_beta_absweight_`ind'' \\ " _n
		}
	else  {
		file write fh  "`ind_name_`ind'' & `alpha_`ind'' & `g_`ind'' & `beta_`ind'' & \multicolumn{1}{c}{N/A}  & `share_`ind'' & `share_beta_absweight_`ind'' \\ " _n
		}
	}
file write fh  "\bottomrule" _n
file close fh
}

********************************************************************************
***** 2.2.c. REPLICATION OF FIGURE A2 AND A3 ***********************************
********************************************************************************
{
*preparing variables
gen omega = alpha1*agg_beta
total omega
mat b = e(b)
local b = b[1,1]
gen label_var = ind 
gen beta_lab = string(agg_beta, "%9.3f")
gen abs_alpha = abs(alpha1) 
gen agg_beta_pos = agg_beta if positive_weight == 1
gen agg_beta_neg = agg_beta if positive_weight == 0
gen F = .
levelsof ind, local(industries)
foreach ind in `industries' {
	capture replace F = `F_`ind'' if ind == `ind'	
	}

*replicating A2
graph twoway ///
	(scatter agg_beta_pos F if F >= 5 [aweight=abs_alpha], msymbol(Oh) mcolor(navy)) ///
	(scatter agg_beta_neg F if F >= 5 [aweight=abs_alpha], msymbol(Dh) mcolor(gold)), ///
	legend(label(1 "Positive Weights") label( 2 "Negative Weights")) ///
	yline(`b', lcolor(black) lpattern(dash)) xtitle("First stage F-statistic")  ///
	ytitle("{&beta}{subscript:k} estimate")
graph export "Tables/exercise2_2c_figureA2.pdf", replace

*replicating A3
gsort -alpha1
graph twoway ///
	(scatter F alpha1 if _n <= 5, mcolor(navy) mlabel(ind_name) msize(0.5) mlabsize(2)) ///
	(scatter F alpha1 if _n > 5, mcolor(navy) msize(0.5) ), ///
	xtitle("Rotemberg Weight") ytitle("First stage F-statistic") ///
	yline(10, lcolor(black) lpattern(dash)) legend(off)
graph export "Tables/exercise2_2c_figureA3.pdf", replace

*Taking figure A2 and A3 into account, we see that there is some heterogeneity in treatment effects underlying the overall estimate. However, the point estimates' dispersion among those industries with F-statistics above 5 and among those industries with higher Rotemberg weight is both relatively small. Moreover, the point estimates with negative Rotemberg weights are close to the overall Bartik point estimator or, if not so, have low Rotemberg weights. Hence, it is unlikely that there are negative weights on location-specific parameters as Goldsmith-Pinkham, Sorkin, and Swift also write in their paper's appendix. Although the evidence against negative weights is not as strong as in the second empirical example of Goldsmith-Pinkham et al., the 'Immigrant Enclave', the patterns of heterogeneity are definitely less concerning compared to their first empirical example, the Canonical Setting'. 

*As it is unlikely that some underlying effects receive negative weights, we can still intepret the overall Bartik point estimator as a LATE. Hence, in this setting, we can be quite confident that the overall Bartik estimate has a LATE-like interpretation as a weighted average of treatment effects.
}



********************************************************
********************** Question 3 **********************
********************************************************

*The estimates presented in section V result from an IV estimator which is obtained by exploiting the product structure of the endogenous variable and thus, according to Goldsmith-Pinkham, Sorkin, and Swift (2020), they can be deemed as resulting from a Bartik-like instrument. Therefore, the assumptions necessary for their consistency are the same stated in point 1.a (we assume homogeneous impact of imports). 
*Both for Autor et al. (2013) and Autor et al. (2020), the first assumption, relevance, is quite general and, thus, highly plausible. Moreover, it can easily be tested through an F-test applied to the first stage regression. We consider the exclusion assumption as more tricky to be justified.
*In Autor et al. (2020), strict exogeneity of the instrument requires that industry-level growth of Chinese exports to other high-income markets is uncorrelated with the residuals of the initial endogenous regression. This amounts to require that the variability in the exposure to Chinese import in other high-income countries is exogenous with respect to the fraction of the variability in US presidential election vote share over 2000-2008 which is left unexplained by the growth of Chinese import penetration in the US, conditional on controls.
*The plausibility of this assumption may be debated, especially when considering the estimates presented in section V which result from regressions with few controls. Indeed, there may exist a relation between Chinese import growth in other high-income countries and the voting decisions in US presidential elections which goes beyond the effect of the change of US exposure to Chinese import. We will elaborate more on this topic with an example, but first let us mention some observations on the plausibility of the assumptions needed for consistency of Autor et al. (2013)'s estimates (see question 1).
*As already said, also fo Autor et al. (2013), the most interesting point is assumption 2. In this context, strict exogeneity practically requires that the realized imports from China to other high-income markets are uncorrelated with the variability in the change in the manufactural employment share of the working-age population which is left unexplained by the change in Chinese import exposure per worker, for a given region and period, conditional on controls. Also in this case alternative channels which link Chinese imports in other markets to change in US counties manufacturing employment beyond what is explained by changes in local exposure to Chinese import per worker may exist (see, for instance, the effect of the share of educated workers in the region, A5 appendix Goldsmith-Pinkham, Sorkin, and Swift (2020)). 
*However, we argue that the amount of factors that can impact the dependent variable, in the 2013's paper, is lower compared to the dependent variable in Autor et al (2020). As an example, suppose that in a US county, say Philadelphia, the penetration of Chinese import stays constant over time, as well as the manufactural employment share. Assume also that Philadelphia county is a huge importer of, let's say, cars from France. In the meantime, a variation in the share of Chinese imports to a high-income country different from the US, say the UK, encourages the development of certain industries in the Uk with the result that the government would like to decrease the price of the cars that UK imports from France. Then, the UK can make various attempts for modifying public opinion in Philadelphia in order to increase support to the right-wing party, which is more likely to introduce protectionist policies (which would cut Philadelphia's demand for French cars and, eventually, decrease their price). This may well happen without any impact, at least in the short run, to Chinese import penetration or share of manufactural employment in Philadelphia.
*Overall, this suggests that with Autor et al (2020)'s dependent variable a stronger justification is needed to claim consistency of the IV estimates, since there may be more space for confounding factors, compared to the Autor et al (2013) outcome variable. It is worth noticing that these justifications are more plausible when controls are introduced. 
}
