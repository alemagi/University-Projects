clear all
set more off
version 17
*Adjust your directory in this global
global lukas "C:\Users\tohof\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 1\Final"
global anna "C:\Users\Anna\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 1\Final"
global alessandro "C:\Users\magia\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 1\Final"
global danilo ""
*Create in your working directory a folder called "Tables"
*And choose your working directory here
*cd "$lukas"
cd "$anna"
*cd "$alessandro"
*cd "$danilo"

*ssc install balancetable


********************************************************************************
***************** Exercise 1 ***************************************************
********************************************************************************
{
***************** Point A ******************************************************
use "Data\jtrain2.dta", clear
*Adjust labels for nicer latex output
label var re74 "real earn. '74"
label var re75 "real earn. '75"
label var re78 "real earn. '78"
label var educ "years of educ."
label var black "black"
label var hisp "hispanic"
label var nodegree "no highschool"
label var train "trained"

balancetable train educ black hisp re74 re75 nodegree using "Tables\Table_1.tex", ///
	vce(robust) pval ctitles("Control" "Trained" "Difference") replace ///
	varlabels 

*the same again to be appended in exercise 2
balancetable train educ black hisp re74 re75 nodegree using "Tables\Table_1b.tex", ///
	vce(robust) pval ctitles("Control" "Trained" "Difference") replace ///
	varlabels 

*Only two differences are significantly different from zero: the dummy for being Hispanic and that for no degree. We are not surprised by these imbalances. Indeed, even though originally the treatment was randomly allocated, the dataset has then been restricted, since some variables, as pre-treatment earning, were available only for some individuals (as explained by Dehejia and Wahba (1999)). Thus, since the sample has been restricted and some units were dropped not at random, imbalances were expected.


***** Alternative ******
*Here we insert an alternative to solve Exercise 1 without using the package Balancetable

/*
*Here we get Mean and SD of the two groups
tabstat educ black hisp nodegree re74 re75, statistics(mean sd) by(train) save
return list
matrix table1 = (r(Stat1)\r(Stat2))' //We save the results in a Matrix

*Show The results
matrix list table1

*Now we add statistics from ttest
estpost ttest educ black hisp nodegree re74 re75, by(train)

*Adding the results to Matrix
matrix table1 = [table1,(e(b)\e(se))']
matrix rownames table1 = educ black hisp nodegree re74 re75

*Show The results
matrix list table1

*Now we export the matrix into latex
esttab matrix(table1) using Table_PointA.tex, title("Table 1")  collabels("Mean Control" "Sd. Control" "Mean Treated" "Sd. Treated" "Mean Difference" "Sd. Mean Difference")  replace
*/


***************** Point B ******************************************************

regress re78 train
return list //Show the results: This is a matrix
matrix A  =  r(table)   //Saving results matrix
matrix list A 
scalar b = A[1,1] //indexing the elements of interest
scalar list b
scalar stde = A[2,1]
scalar list stde

*The coefficient indicates that the train status is associated with 1,794$ higher earnings in 1978. The result is statistically different from zero at all conventional significance levels.

***************** Point C ******************************************************

regress re78 train //Command for the classic regression
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo first_reg, addscalars(N_Control N_Control N_Treated N_Treated)

regress re78 train age educ black hisp
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo sec_reg, addscalars(N_Control N_Control N_Treated N_Treated)

regress re78 train age educ black hisp re74 re75
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo third_reg, addscalars(N_Control N_Control N_Treated N_Treated)
*Exporting
esttab first_reg sec_reg third_reg using "Tables\Table_2a.tex", ///
	scalars(N_Control N_Treated) replace r2 ///
	label nogaps title("Regressions - Exercise 01") ///
	se  starlevels(* 0.10 ** 0.05 *** 0.01)

	
*The results are not so much sensitive to the introduction of covariates. When moving from the first to the second regression, the estimated effect of training on '78 earnings decreases a bit, but it remains significant and estimated standard errors are almost the same. When moving from the second to the third OLS regression, the estimated treated effect almost does not change, and similarly the estimated standard errors. Also the coefficients estimated for the other covariates do not change, except for the dummy for being Hispanic, which, by the way, was not significant. 

***************** Point D ******************************************************

regress re78 train age educ black hisp re74 re75
dfbeta train, stub(influence_train) 

egen rank = rank(influence_train)

su rank, meanonly 
local massimo = r(max)

drop if rank == 3 | rank == 5 | rank==10 
drop if rank == `massimo'-3 | rank == `massimo'-5 | rank == `massimo'-10

regress re78 train age educ black hisp re74 re75
eststo reg_D

esttab first_reg sec_reg third_reg reg_D using "Tables\Table_2b.tex", ///
	replace	label nogaps r2 title("Regressions - Exercise 1: Sensitivity checks") ///
	se  starlevels(* 0.10 ** 0.05 *** 0.01)
	
*The results change between the two regressions, even though not a lot. The estimated effect of training decreases by about 100$, ceteris paribus. The estimated effects of the other covariates do not change a lot, exept from the dummy "hispanic" which changes sign (which, however, is not significant). The estimated standard errors do not change significantly.
}

********************************************************************************
***************** Exercise 2 ***************************************************
********************************************************************************
{
***************** Point A ******************************************************
use "Data\jtrain3.dta", clear
*Adjust labels for nicer latex output
label var re74 "real earn. '74"
label var re75 "real earn. '75"
label var re78 "real earn. '78"
label var educ "years of educ."
label var black "black"
label var hisp "hispanic"
label var train "trained"

balancetable train educ black hisp re74 re75 using "Tables\Table_1b.tex", ///
	vce(robust) pval ctitles("Control" "Trained" "Difference") append ///
	varlabels prefoot("\\" "\\" "\hline")

***************** Point B ******************************************************

set seed 88888
gen temp=uniform()
qui sum temp, d
gen treated=1 if temp<=r(p50)
replace treated=0 if treated==.
drop temp
*alternatively gen treated=rbinomial(1,0.5) or round(runiform())

***************** Point C ******************************************************

*ssc install randtreat

randtreat, generate (treated_2) setseed(5)

pwcorr treated treated_2, sig

*When looking at the output of pwcorr, you can see that the variable "treated" (which randomly assigns half of the observations to a fake treatment group and the other half to a pretend control group) and the newly formed treatment variable fake "treated 2" have a low correlation. Indeed, the correlation coefficient r is 0.003, indicating a minor but substantial positive association between the two variables. The high p-value (0.8771) indicates that the two variables do not have a statistically significant association.


***************** Point D ******************************************************

balancetable treated educ black hisp re74 re75 using "Tables\Table_1b.tex", ///
	vce(robust) pval ctitles("Control" "Treated" "Difference") append ///
	varlabels prefoot("\\" "\\" "\hline")

*When compared to earlier points, the addition of the "treatment" variable renders all of the variables statistically insignificant. This verifies our suspicions about the randomization process's varied nature.

***************** Point E ******************************************************

qui reg re78 treated, r
tabstat treated if e(sample)==1, statistics(count) by(treated) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo first_reg2, addscalars(N_Control N_Control N_Treated N_Treated)

qui reg re78 treated age educ black hisp, r
tabstat treated if e(sample)==1, statistics(count) by(treated) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo sec_reg2, addscalars(N_Control N_Control N_Treated N_Treated)

qui reg re78 treated age educ black hisp re74 re75, r
tabstat treated if e(sample)==1, statistics(count) by(treated) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo third_reg2, addscalars(N_Control N_Control N_Treated N_Treated)

esttab first_reg sec_reg  using "Tables\Table_PointC.tex", title("TABLE 2: Question 2E") scalars(N_Control N_Treated) replace

esttab first_reg sec_reg third_reg first_reg2 sec_reg2 third_reg2 using "Tables\Table_2c.tex", ///
	scalars(N_Control N_Treated) replace r2 ///
	label nogaps title("Regressions - Exercise 02") ///
	se  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	mgroups("jtrain2" "jtrain3", pattern(1 0 0 1 0 0)) ///
	order(train treated)

*In comparison to what we learned in question 1, the scenario has changed. Indeed, looking at table 2,the placebo variable presented earlier ("treated" instead of "train") is not statistically significant, as expected.
}

********************************************************************************
***************** Exercise 3 ***************************************************
********************************************************************************
{
***************** Point A ******************************************************
*First, I run the regressions and save the number of controlled and treated units

regress re78 train
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo first_reg3, addscalars(N_Control N_Control N_Treated N_Treated)

regress re78 train age educ black hisp
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo sec_reg3, addscalars(N_Control N_Control N_Treated N_Treated)

regress re78 train age educ black hisp re74 re75
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo third_reg3, addscalars(N_Control N_Control N_Treated N_Treated)

*Now I export the results using esttab and .tex extension
esttab first_reg sec_reg third_reg first_reg2 sec_reg2 third_reg2 first_reg3 sec_reg3 third_reg3 ///
using "Tables\Table_2d.tex", ///
	scalars(N_Control N_Treated) replace ///
	label nogaps title("Regressions - Exercise 03 (a)") ///
	se  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	mgroups("jtrain2" "jtrain3" "jtrain3", pattern(1 0 0 1 0 0 1 0 0)) ///
	order(train treated)

*First, we compare the three newly obtained regressions with those obtained from a different dataset in question 1. We notice the results differ a lot. 

*Indeed, the first and second regressions show a negative significant estimator of the training effect on earnings (i.e., ceteris paribus training decreases earnings by about 15,000 $ and 8,500 $ respectively), even though the magnitude of the effect halves (in absolute value) when adding the covariates. Such paradoxical effect of training, which here is found to have a negative impact on earnings, is similar to that found by Angrist and Pischke (2008) in doing the same regression. The authors explain such difference with selection bias, due to the fact that the dataset that they were, and we are, using, is broad and likely to be quite different compared to the treated group. 

*In the third regression, the estimated effect of training becomes positive (i.e., training raises earnings of about 200US$) but insignificant. Similarly, the estimated effects of the other covariates change, even though many of them are found to be significant. 

*Overall, the model is not robust to changes in the specification.

***************** Point B **********************************
*the logistic regression
logit train age educ black hisp re74 re75, vce(rob)

predict yhat

*I construct the common support by creating a dummy which equals one for obesrvations which satisfy the overlap assumption

gen phat_treated = yhat if train == 1
gen phat_controlled = yhat if train == 0

sum  phat_treated
scalar lb_support = r(min)

sum  phat_controlled
scalar ub_support = r(max)

gen common_support = (yhat>lb_support & yhat<ub_support)

***************** Point C *********************************

*we repeat point A but now with the dataset restricted to the common support

regress re78 train if common_support == 1
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo fourth_reg3, addscalars(N_Control N_Control N_Treated N_Treated)

regress re78 train age educ black hisp if common_support == 1
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo fifth_reg3, addscalars(N_Control N_Control N_Treated N_Treated)

regress re78 train age educ black hisp re74 re75 if common_support == 1
tabstat train if e(sample)==1, statistics(count) by(train) save
return list
scalar N_Control = r(Stat1)[1,1]
scalar N_Treated = r(Stat2)[1,1]
eststo sixth_reg3, addscalars(N_Control N_Control N_Treated N_Treated)


esttab first_reg sec_reg third_reg first_reg2 sec_reg2 third_reg2 ///
	first_reg3 sec_reg3 third_reg3 fourth_reg3 fifth_reg3 sixth_reg3 ///
	using "Tables\Table_2e.tex", r2 ///
	scalars(N_Control N_Treated) replace ///
	label nogaps title("Regressions") ///
	se  starlevels(* 0.10 ** 0.05 *** 0.01) ///
	mgroups("Ex.1" "Ex.2" "Ex.3" "Ex.3 - CS", pattern(1 0 0 1 0 0 1 0 0 1 0 0)) ///
	mtitles ("earn 78'" "earn 78'" "earn 78'" "earn 78'" "earn 78'" "earn 78'" "earn 78'" "earn 78'" "earn 78'" "earn 78'" "earn 78'" "earn 78'" ) ///
	order(train treated) compress

*When the regression is restricted to the common support, the results do change. In the first and second regressions, the estimated impact of training on earnings is still negative, but of lower magnitude than before (i.e., about 5,800 $ and 7,500 $ respectively). In the third regression the estimated effect is positive and higher (i.e., ceteris paribus, being in the training group increases 1978 earnings by about 1,000 US$). Looking at the estimated effect of the other covariates on'78  earnings, the second regression has estimated effects that differ both in terms of sign and of magnitude with respect to the same regression run on the whole sample. Conversely, the third regression on the whole sample and that on the common support display similar estimated effects of the other covariates. 

*Overall, even though the difference between the estimators just obtained and those of question 1 is still considerable, it is also clear that the regression on the common support does a better job compared to the regression on the entire dataset jtrain3, in order to estimate an effect of training closer to the one found in point 1 with the "experimental" data. Therefore, there are reasons to maintain that restricting the regression to the common support can be useful, especially with a very broad dataset as jtrain3, which may give a control group quite different from the treated units. Indeed, disregarding the observations outside the common support can be useful in order to assure a greater degree of similarity among the individuals since only those cells where there are at least both some treated and some controlled units are considered in the regression. However, it is also worth noticing, as underlined by Angrist and Pischke (2008), that the estimated standard errors of these last three regressions have not been adjusted for the sampling variance in the estimates of the propensity score.

***************** Point D ******************************************************	
* produce graph of common support *

*net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
set scheme cleanplots

*graph set window fontface "LM Roman 10"

twoway ///
(histogram yhat if train==1, fraction fcolor(white) lcolor(dkgreen)) ///
(histogram yhat if train==0, fraction yaxis(2) fcolor(white) lcolor(gold)), ///
title("Histogram of Propensity Score", size(medsmall) margin(b=4)) ///
xtitle("Propensity Score", size(small)) ytitle("Fraction of Treatment", size(small))  ytitle("Fraction of Control", size(small) axis(2)) ///
xscale(titlegap(*10)) yscale(titlegap(*10)) yscale(titlegap(*10) axis(2)) ///
ylabel(, labsize(small) angle(horizontal)) ylabel(, labsize(small) angle(horizontal) axis(2))  xlabel(, labsize(small) angle(horizontal)) ///
legend(label(1 "Treatment") label(2 "Control") size(medsmall) nobox region(lstyle(none) lcolor(white))) ///
graphregion(color(white)) bgcolor(white)

graph export Hist_PS.png, as(png) name("Graph") replace

*NB: In order to have a more clear hinstogram, given the difference between the control and the treatment group, we decide to use both the vertical axis on the left and right hand side.

*From the histogram, it stands out that a considerable amount of the controlled units have a propensity score close to zero. This makes sense considering that the control group comes from a dataset, jtrain3, which is broad and not selected in an extremely detailed way. Thus, we expect it to differ from the treated units. As a result, the regressions on the common support have a much lower (about half) number of controlled observations.
 
}

********************************************************************************
***************** Exercise 4 ***************************************************
********************************************************************************
{
***************** Point A ******************************************************
use "Data\jtrain3.dta", clear

*findit pscore: click on st0026 2 to install the updated version of the ado file

* Adapt dataset to the paper
gen age2 = age^2
label var age2 "age squared"
gen educ2 = educ^2
label var educ2 "education squared"
gen RE74 = re74*1000
label var RE74 "'74 earnings, in $"
gen RE75 = re75*1000
label var RE75 "'75 earnings, in $"
gen RE78 = re78*1000
label var RE78 "'78 earnings, in $"
gen RE742 = RE74^2
label var RE742 "'74 earnings squared"
gen RE752 = RE75^2
label var RE752 "'75 earnings squared"
gen T = train
label var T "treatment"
gen blackU74 = unem74*black
label var blackU74 "interact. black unempl. '74"

pscore T age age2 educ educ2 marr black hisp RE74 RE75 RE742 RE752 blackU74, ///
		pscore(mypscore) blockid(myblock) comsup numblo(5) level(0.005) logit

***************** Point B ******************************************************

* Yes, we find the same values as Becker and Ichino (2002) present in page 370.
* The authors used the consup option to construct common support. This means that steps 2-7 in their alogorithm are restricted to common support.

***************** Point C ******************************************************

tabstat mypscore, by(T)
tabstat mypscore if comsup == 1, by(T) 

* Before imposing the common support condition, the average pscore is 0.0232 in the control group and 0.6874 in the treatment group. 
* After imposing the common support condition, the average pscore is 0.0499 in the control group and 0.6874 in the treatment group. 

***************** Point D ******************************************************

* Estimating ATT with propensity score and nearest neighbor matching
set seed 1221
attnd RE78 T age age2 educ educ2 marr black hisp RE74 RE75 RE742 RE752 blackU74, ///
		comsup reps(100) dots logit

* Estimating ATT with propensity score and the radius matching method
attr RE78 T age age2 educ educ2 marr black hisp RE74 RE75 RE742 RE752 blackU74, ///
		comsup boot reps(100) dots logit radius(0.05)
		
* The results are not consistent across the two models. Using the radius matching method (r=0.05), we obtain an ATT of -6.221$, which is neither consistent with the experimental findings nor with the nearest neighbor matching method. Presumably, this finding can be explained by the fact that the control group has very low propensity scores on average. A radius of 0.05 therefore includes much more control units than the nearest neighbor method; also those control units which are not sufficiently eual to the treated units. In fact, we see that the number of control units is 1.157. This means that almost haf of the control units very used as controls, although we know that only a small portion of them constitutes a suitable counterfactual.

***************** Point E ******************************************************

* Certainly, Smith and Todd (2005s) have a point when claiming that "propensity score matching does not represent a "magic bullet" that solves the selection problem in every context". Moreover, Smith and Todd (2005a) criticise that propensity score matching is less reliable when (1) the set of observed variables related to program participation and labor market outcomes is small, (2) when treatment and comparison groups are drawn from different geographical areas and (3) the dependent variable is not measured in the same way for both groups. This citicism is based on findings from a range of other high quality publications. Hence, also here they certainly have a point. Finally, we agree that the use of every econometric method must be context dependent. No econometric method is a one-fits-all solution, also not propensity score matching. 

* Yet, Dehejia and Wahba never claimed that their method can be easily generalized. Instead, they emphasized that a convincing specification of the propensity score matching method is pivotal. In fact, they indirectly support the claim of Smith and Todd that propensity score matching is no "magic bullet". By selecting data- and context-specific spezifications of propensity score matching, they show that a hasty and generalizing implementation of propensity score matching (which they acuse Smith and Todd of) leads to non-neglectable changes in estimates. This clearly is a major limitation for the use of propensity score matching because most researchers arguably lack the experience and deep understanding of propensity score matching which the leading authors in the field have. 

* In conclusion, we can learn from the debate of Smith and Todd and Dehejia that propensity score matching has its disadvantages and pitfalls. Although we would not go that far and agree with Smith and Todd that propensity score matching does not solve the selection problem in the NSW data, we are not fully convinced that the approach taken by Dehejia and Wahba satisfies the highest scientific standards. The critique of Smith and Todd generally appears to be justified. However, this does not render propensity score matching unuseful in general but rather in a very specific context. Hence, we still believe that propensity score matching is, used in the right setting with correct and convincing specification, a powerful tool. Yet, it might not be as powerful as some readers of Dehejia and Wahba study might have hoped. 

* As showed in point 3, a simple OLS regression can produce an estimated effect of training close to the one obtained from the experimental data (question 1). This is especially true when we follow Angrist and Pischke (2008) procedure and use propensity score as an initial screening method (question 3.c). However, it is worth noticing a few drawbacks of this approach. First, the difference between the estimates decreases from point 3.a to 3.c, but it does not disappear. More specifically, the best OLS estimate is that training increases '78 earnings by about 1,040$, ceteris paribus, compared to the experimental estimated treatment effect of about 1,680$. Moreover, the OLS regressions, even when restricted to the common support, display high estimated standard errors, which lead the estimated treatment effect of the third regression to be insignificant. This issue makes sense when linked to the observations made by Angrist and Pischke (2008) and reported in point 3.c, about the estimated standard errors which are not corrected for the variability in the estimation of the pscore. Of course, it may be the case that, by selecting a better control group using prior information, as suggested by Angrist and Pischke (2008), this issue could be tackled and OLS regression could give even better estimates. 

* Moreover, Angrist and Pschke (2008, p.86) rightly acknowledgethat that "there are many details to be filled in when implementing propensity score mtaching, such as how to model the score and how to do inference". As many of these "details" are not yet standardized and as it thus requires a lot experience and deep understanding of propensity score matching to obtain valid results, we agree that propensity score mtaching should be used with caution. Of course, when researchers have reasonable good information on the mechaisms and covariates determining treatment assignment propensity scores can be very useful. Finally, it must be noted that Angrist and Piscke wrote an "Empiricist's Companion". Hence, given their target group, it is convincing that they recommend the more reliable and standardized procedure of OLS regressions.

* Overall, what we can conclude from the present analysis is that the OLS regression has produced estimates more different from the experimental results, compared to some specifications which use pscore matching. However, OLS regression has the advantage of being somehow a more standardized and less error-prone procedure. When controlling for the right covariates, it provides a useful comparative benchmark and, in many cases, it certainly does a reasonably good job of eliminating selection bias.
}

********************************************************************************
***************** Exercise 5 ***************************************************
********************************************************************************
{
***************** Point A ******************************************************
*Following Athey and Imbens (2017), the condition for Neyman's inference to be unbiased, when the treatment effect is heterogeneous, is that we assume that the sample is randomly drawn from an infinite super population.

***************** Point B ******************************************************
*Fisher's method builds on the idea of considering potential outcomes as given and exploiting randomization in the assignment of treatment to make inference. The classic sharp null hypothesis is that of lack of effect of the treatment. Under the null, it is possible to infer all the missing potential outcomes. 

*To test whether the null should or should not be rejected, the first point is choosing a suitable statistic. The statistic first mentioned by Athey and Imbens (2017) is the difference in mean outcome between the treated and the control group. After having chosen the statistic, it is necessary to compute its value from the observed outcomes. Then, the treatment should be re-assigned many times and the statistic should be computed again and again. Finally, it is necessary to compute the share of the rounds which leads to a statistic more extreme than the one observed and, by doing so, compute the p-value that the difference in means is higher than that observed.

*We only use the mean-difference statistics instead of the difference in means of the ranks by treatment status, because we neither have many 0 nor a large tail.

use "Data\jtrain2.dta", clear
set seed 4442

mat M=(.,.,.,.)
forvalue i=1(1)1000 {
	cap drop select sample1 rank 
	gen select = runiform()
	egen rank = rank(select)
	gen sample1 = 0
	qui replace sample1 = 1 if rank<=185
	qui replace sample1 = 3 if rank>425
	qui tabstat re78, statistics(mean count) by(sample1) save
	qui return list
	qui matrix M = (M \ `i' ,r(Stat2)[1,1]-r(Stat1)[1,1],r(Stat2)[2,1], r(Stat1)[2,1])
}
matrix list M

svmat M
rename M2 mean_diff
gen more_extreme = 0
replace more_extreme = 1 if abs(mean_diff) >  1.794
tabstat more_extreme, statistics(sum) save
return list
display r(StatTotal)[1,1]/1000
* p-value with 10,000 repetitions is 0.0055
* p-value with 1,000 reps and seed: 12 is 0.0044

*Therefore, we have found a pvalue close to that of Athey and Imbens (2017), even though not the same. Maybe this can be explained by the fact that Athey and Imbens do not drop 20 observations randomly as we do. They might drop only (certain) oberservations from the control group. It is not really clear from their paper. Moreover, the (small) difference in results could also be explained by different random seeds or different number of repetitions. Because more extreme mean differences than 1.79 are very rare, we only "converge slowly" towards the "true" p-value. Hence, even with 10,000 repetitions our results are sensitive with respect to different random seeds. 

* We also tried an alternative path as an additional consistency check. To do so we used the function permute which is an intergrated STATA function

/*
permute train ((r(Stat2)[1,1])-(r(Stat1)[1,1])), reps(10000) rseed(1234): tabstat re78, statistics(mean) by(train) save
*p-value: 0.0044
*/


***************** Point C ******************************************************
*We think that the fact that the LaLonde sample is stratified should have been considered by Athey and Imbens while doing the Fisherian inference. For instance, suppose that in 1978 and for the population considered by the study, earnings were on average higher in one of the cities considered compared to the others. Then, if re-assignment of the treatment ends up treating more people from this city than in the original assignment, this could lead to more extreme values of the estimated mean difference and, in turn, to higher chances of rejecting the null hypothesis (i.e., higher chances of Type I error). Therefore, it would have been more accurate to keep the number of treatment and control units at the city level the same while randomly re-assigning the treatment. 
}