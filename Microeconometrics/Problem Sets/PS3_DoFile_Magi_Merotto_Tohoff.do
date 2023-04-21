clear all
global alessandro "C:\Users\magia\OneDrive - Università Commerciale Luigi Bocconi\AA Second Semester\Microeconometrics\Microeconometrics_Stata\Problem Set\PS 3\Alessandro"
global lukas "C:\Users\tohof\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 3\Final"
global anna "C:\Users\anna\OneDrive - Università Commerciale Luigi Bocconi\Problem Set\PS 3\Anna"
global final "C:\Users\magia\OneDrive - Università Commerciale Luigi Bocconi\AA Second Semester\Microeconometrics\Microeconometrics_Stata\Problem Set\PS 3\Final"

cd "$final"
*net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
use "DTA\pset_3.dta", clear

*********************************
*************** A ***************
*********************************
{
set scheme cleanplots
rdplot T X, graph_options(title("T-X Discontinuity", size(medsmall) margin(b=4)) legend(off) xtitle("Running Variable", size(small)) ytitle("Treatment Variable", size(small)) xscale(titlegap(*5)) yscale(titlegap(*-25)) ylabel(, labsize(small) angle(horizontal))  xlabel(, labsize(small) angle(horizontal))) 
*graph export "Tables\Graph_TX.png", as(png) name("Graph") replace

*From the graph it's clear that the design is a Sharp RD since there is a sharp jump from 0 to 1. There are no treated municipalities below the threshold while every municipalitiy above the treshold is treated.

*Here we check formally that there are no T=0 above the cut-off
gen check = 0
replace check = 1 if (X>=0)
tab check T
drop check

}

*********************************
*************** B ***************
*********************************
{
preserve

local covariates "hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"


cap drop variable_name
cap drop lables
gen variable_name = ""
gen lables = ""
local row = 1
foreach cov in `covariates' {
	
	replace variable_name = "`cov'" in `row'
	replace lables = "`: variable label `cov''" in `row'
	local ++row
	
}

local num: list sizeof covariates
mat Table_1 = J(`num',4,.)
mat list Table_1 
local row = 1
foreach cov in `covariates' {
    qui rdrobust `cov' X
	mat Table_1[`row',1] = round(e(h_l),.001)
	mat Table_1[`row',2] = round(e(tau_cl),.001)
	mat Table_1[`row',3] = round(e(pv_cl),.001)
	mat Table_1[`row',4] = round(e(N_h_l)+e(N_h_r),.001)
	local ++row
}
mat rownames Table_1 = `covariates'
mat colnames Table_1 = "MSE_Optimal_Bandwidth" "RD_Estimator" "P_val" "Effective_Number"
mat list Table_1 
svmat Table_1, names( col )
keep lables Effective_Number MSE_Optimal_Bandwidth P_val RD_Estimator
drop in 10/2629
dataout, save(Table1.tex) tex replace dec(2)

restore
*NOTE: We have choosen the conventional insted of robust p-value because the problem set was not asking for them. With Robust we would replace e(pv_cl) with e(pv_rb)
}

*********************************
*************** C ***************
*********************************
{
local covariates "hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk"

foreach cov in `covariates' {
	
rdplot `cov' X, graph_options(title("`: variable label `cov''", size(medsmall) margin(b=4)) legend(off))
graph rename `cov', replace
}

graph combine hischshr1520m i89 vshr_islam1994 partycount lpop1994 merkezi merkezp subbuyuk buyuk
graph export "Tables\Graph_1.pdf", replace
}

*********************************
*************** D ***************
*********************************
{


rdrobust Y X
scalar h_left = -e(h_l)
scalar h_right = e(h_r)

twoway ///
(histogram X if  X < 0 & X >= h_left , freq color(red) bin(25)) ///
(histogram X if X >= 0 & X <= h_right, freq color(blue) bin(25)), ///
graphregion(color(white)) xtitle("Islamic vote share 1994", size(small)) ytitle("", size(small))  ///
xscale(titlegap(*10)) yscale(titlegap(*10)) ///
ylabel(, labsize(small) angle(horizontal))  xlabel(, labsize(small) angle(horizontal)) xline(0, noextend lcolor(green) lpattern(solid)) legend(off)
graph rename x_histogram, replace


local h_l = h_left
local h_r = h_right
rddensity X, plot plot_range(`h_l' `h_r')
graph rename density_plot, replace


*For illustrative reasons we have placed the two graphics on top of each other and not side by side
*A side by side graph would result from: - graph combine density_plot x_histogram, col(2) iscale(0.5) -
graph combine density_plot x_histogram, col(1) iscale(0.5)
graph export "Tables\Graph_2.pdf", replace
}


*********************************
*************** E ***************
*********************************
{
rddensity X

*This command compute the McCrary density test. We estimate the density of running variable near the cutoff and we test the the continuity. In our case we fail to reject H0 (continuity) with a p-value of 0.1634 so we can say that there is no evidence of manipulation. Hence, the density of X around the cutoff is sufficiently continuous. 
}

*********************************
*************** F ***************
*********************************
{
local cutoff "-10 -5 5 10"

matrix define R = J(4,4,.)
local k = 1
foreach x in `cutoff' {

	if `x' > 0 {
		local condition = "if X >= 0"
	}
	else if `x' < 0 {
		local condition = "if X < 0"
	}
	else {
		local condition = ""
	}
	
	rdrobust Y X `condition', c(`x')

	matrix R[`k', 1] = e(h_l)
	matrix R[`k', 2] = e(tau_cl)
	matrix R[`k', 3] = e(pv_cl)
	matrix R[`k', 4] = e(N_h_l)+e(N_h_r)

	
	local k = `k' + 1
}

mat rownames R = `cutoff'
mat colnames R = "MSE_Optimal_Bandwidth" "RD_Estimator" "P_val" "Effective_Number"

matrix list R

*Here we test for discontinuity of Y in different thresholds using placebo cut-off. To avoid contamination from the true cut-off we use only the treatment group to test the placebos above the 0 and then the control group otherwise. We find no statistically significant discontinuity at neither -10, -5, 5 nor 10. 
}

*********************************
*************** G ***************
*********************************
{
rdplot Y X, binselect(es) scale(2.225 2.225) ///
	graph_options(ytitle("Outcome") xtitle("Running Variable") legend(off))
	
*We scaled the bins with the same multiplier of 2.225, based on a trial and error approach, to achieve 40 (almost) evenly-spaced bins. Alternatively, we could write - rdplot Y X, binselect(es) nbins(24 16) - , which would also be based on trial and error. Moreover, we could achieve evenly spaced bins throughout (same bin size left and right of the cutoff) by specifying nbins(20 20).
}

*********************************
*************** H ***************
*********************************
{
*uniform kernel
rdrobust Y X, p(1) kernel(uni) 
*uniform triangular
rdrobust Y X, p(1) kernel(triangular) 

*Note: p(1) is the default. Still, we specified it to be sure to get results based on local linear polynomials

*Electing a mayor from an Islamic party has indeed a significant effect on the educational attainment of women. The RD estimate shows a 3.0-3.2 percentage point treatment effect on female high school education. The effect is for both kernel choices significant at the 5 percent level and the different kernel choices do not change the results significantly. 
}

*********************************
*************** I ***************
*********************************
{
*defining variables for polynomials of order 4
gen X_2 = X^2
gen X_3 = X^3
gen X_4 = X^4

*defining triangular weights globally
gen glob_weights = .
replace glob_weights = (1 - abs(X/100)) if X < 0 
replace glob_weights = (1 - abs(X/100)) if X >= 0 

*estimating the intercept of the local polynomial regression of order 4 on the left of the cut-off
reg Y X X_2 X_3 X_4  [aw = glob_weights] if X < 0
matrix coef_left = e(b)
scalar intercept_left = coef_left[1, 5]	

*estimating the intercept of the local polynomial regression of order 4	on the right of the cut-off
reg Y X X_2 X_3 X_4  [aw = glob_weights] if X >= 0 
matrix coef_right = e(b)
scalar intercept_right = coef_right[1, 5]

*taking the difference of the intercepts
scalar difference = intercept_right - intercept_left
display difference

*checking if we yield the same result as with rdrobust
rdrobust Y X, p(4) kernel(triangular) h(100 100) 
*Note: the estimator is the same
}

*********************************
*************** J ***************
*********************************
{
*getting and storing the optimal bandwidth
rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
ereturn list
scalar h_l=-e(h_l)
scalar h_r=e(h_r)

*defining triangular weights locally
cap gen loc_weights = .
replace loc_weights = (1 - abs(X/h_l)) if X < 0 & X >= h_l
replace loc_weights = (1 - abs(X/h_r)) if X >= 0 & X <= h_r

*estimating the local polynomial regression of order 4	on the left of the cut-off
reg Y X  [aw = loc_weights] if X < 0 & X >= h_l
matrix coef_left = e(b)
scalar intercept_left = coef_left[1, 2]

*estimating the local polynomial regression of order 4	on the right of the cut-off
reg Y X [aw = loc_weights] if X >= 0 & X <= h_r
matrix coef_right = e(b)
scalar intercept_right = coef_right[1, 2]

*taking the difference of the intercepts
scalar difference = intercept_right - intercept_left
display difference

*checking if we yield the same result as with rdrobust
rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
ereturn list 
scalar difference_1 = e(tau_cl) - difference
display difference_1
*1.625e-08

*Note: the estimator is almost the same

*We get results close to those obained in point (h). 
*Notice that to obtain this result, we have had to build weights according to the triangular kernel formula. Indeed, differently from the uniform kernel and from the OLS, the triangular kernel constructs local polynomial estimators using non-uniform weights, which are assigned  to observations according to their distance from the cutoff (i.e. higher weight to observations closer to X=zero). These particular features of the triangular kernel function may also explain the slight difference in our results compared to what obtained in point (h).

*Notice however that the difference is really small in magnitude (was it not the case, we should be worried that there is an underlying issue in our specification, or excessive noise in the data).  
}

*********************************
*************** K ***************
*********************************
{
*getting and storing the optimal bandwidth
rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
ereturn list
local opt_i=e(h_l)

*looping over alternative bandwidth and writing to a matrix
matrix define R = J(5, 6, .)

local col = 1
forvalues i = 0.5(0.25)1.5 {
local bw = round(`opt_i'*`i',.0001)
rdrobust Y X, h(`bw')
matrix R[`col', 1] = `bw'
matrix R[`col', 2] = e(tau_cl)
matrix R[`col', 3] = e(se_tau_cl)
matrix R[`col', 4] = R[`col', 2] - invnormal(0.975) * R[`col', 3]
matrix R[`col', 5] = R[`col', 2] + invnormal(0.975) * R[`col', 3]
matrix R[`col', 6] = `i'
local col = `col'+1
} 

*plotting results
preserve
svmat R
graph twoway (rcap R4 R5 R1, lcolor(navy)) (scatter R2 R1, mcolor(cranberry)), ytitle("RD Treatment Effect") xtitle("Bandwidth") yscale(range(-5 10)) legend(off) 
graph export "Tables\Graph_3.pdf", replace
restore

*As we can see from the graph, the results are quite robust to changes in the bandwitch. Indeed, for all the different bandwitch choices, the RD estimated treatment effect is between 1.80 and 3.02, approximately. The smaller estimate is obtained for bandwith 0.5*opt_i, the higher for the bandwith opt_i (where opt_i = 17.23 approximately). 
*It is worth noticing that the narrowest bandwiths not only result in a smaller treatment effect estimates, compared to the other bandwiths, but also in larger confidence intervals, not significantly different from zero. While the latter can be explained by a smaller number of observations given a smaller bandwidth, the former could indicate that narrowing of the bandwidth might lead to (slightly) underestimating the effect. However, estimates are very similar and robust throughout.
*A final point which is worth noticing is that we could have used also e(se_tau_rb) for robust standard errors and we would have obtained the same treatment effect estimates, but they would not have been significantly different from zero.
*Indeed, in that scenario we would obtain:

rdrobust Y X, p(1) kernel(triangular) bwselect(mserd)
ereturn list
local opt_i=e(h_l)

*looping over alternative bandwidth and writing to a matrix
matrix define R = J(5, 6, .)

local col = 1
forvalues i = 0.5(0.25)1.5 {
local bw = round(`opt_i'*`i',.0001)
rdrobust Y X, h(`bw')
matrix R[`col', 1] = `bw'
matrix R[`col', 2] = e(tau_cl)
matrix R[`col', 3] = e(se_tau_rb)
matrix R[`col', 4] = R[`col', 2] - invnormal(0.975) * R[`col', 3]
matrix R[`col', 5] = R[`col', 2] + invnormal(0.975) * R[`col', 3]
matrix R[`col', 6] = `i'
local col = `col'+1
} 

*plotting results
preserve
svmat R
graph twoway (rcap R4 R5 R1, lcolor(navy)) (scatter R2 R1, mcolor(cranberry)), ytitle("RD Treatment Effect, robust SE") xtitle("Bandwidth") yscale(range(-5 10)) legend(off) 
*graph export "Tables\Graph_3_1.png", replace
restore

}

*********************************
*************** L ***************
*********************************
{
*As a first graphical inspection, we show the jump of the outcome variable at the new cutoff (x = 0). We follow the steps of question 1:

set scheme cleanplots
rdplot T x, graph_options(title("T-x Discontinuity", size(medsmall) margin(b=4)) legend(off) xtitle("Running Variable", size(small)) ytitle("Treatment Variable", size(small)) xscale(titlegap(5)) yscale(titlegap(-25)) ylabel(, labsize(small) angle(horizontal))  xlabel(, labsize(small) angle(horizontal))) 

*We see that the dependent variable does indeed jump and that, should we use the x variable as a running variable, we should implement a fuzzy RD. Indeed, from the graph we see that the probability of treatment change discontinuously after the cutoff, but not from 0 to 1. 
*However, we also note that the values of observations start to increase already before the cutoff, and keep doing so even after it, without a proper jump. Moreover, the discontinuity that appears from the graph is really small.
*We then try yo estimate the treatment effect by using rdrobust:

rdrobust T x

*and we get not significantly treatment effects. Thus, our suspicious are confirmed, the outcome does not show a significant discontinuity at the cutoff defined using x. Therefore, x cannot be used as an alternative running variable.

*Note: By investigating the relationship between X and x, we guess that x is based on X plus some "noise". 
gen z = X-x
sum z
kdensity z

*We further guess that the noise is approximately normally distributed with mean zero and standard deviation of 5. We suspect however that this noise is not random, but that it affects differently the observations closer to the cutoff and, thus, it does not allow to use x as an alternative running variable. 

}
