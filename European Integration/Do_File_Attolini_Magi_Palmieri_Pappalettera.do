clear all
global alessandro "C:\Users\magia\OneDrive - Università Commerciale Luigi Bocconi\AA Second Semester\European Integration\Final"
global marvin "C:\Users\Marvi\OneDrive - Università Commerciale Luigi Bocconi\Documenti\Bocconi\Magistrale - ESS\Year I\Semester II\Economics of European Integration\Group Project"

*cd "$alessandro"
cd "$alessandro"

************please install the following package to reproduce the graphs************
*net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
set scheme cleanplots
graph set window fontface "LM Roman 10"

*net from http://www.stata.com
*net cd users
*net cd vwiggins
*net install grc1leg


*********************************
*********************************
**# FIRST PART
*********************************
*********************************

use "DTA/EEI_TH_2022.dta", clear
sort id_n year
order id_n year

local all_variables "K sales L W M real_K real_sales real_M real_VA"
local variables "K sales L W M"
local real_variables "real_K real_sales real_M real_VA"

*preliminary step
*data cleaning: replace all observations = 0 with a missing value (note: there are no negative observations, only observations = 0)
foreach var in `all_variables' {
        replace `var'=. if  `var'<=0
        }
	
	
	
	
*********************************
**## PROBLEM 1
*********************************

*---------------------------*
**### A and B
*---------------------------*

foreach years in 2008 2017 {

foreach sec in 13 29 {

tabstat `variables' if country == "Italy" & year == `years' & sector == `sec', statistics(count mean sd min p25 p50 p75 max) save
return list
matrix sumstat_`sec'_`years' = r(StatTotal)'

qui sum id_n if country == "Italy" & year == `years' & sector == `sec'
mat sumstat_`sec'_`years' = J(1,8,.)\sumstat_`sec'_`years'\[r(N),.,.,.,.,.,.,.]

}

frmttable, statmat(sumstat_13_`years') sdec(0,2,2,0,0) ///
		   ctitles("", "", "N. of Observations", "Mean" ,"Standard Deviation", "Min", "p25", "Median", "p75", "Max") ///
             rtitles("NACE 13", "" \ "", "Capital" \ "", "Revenues" \ "", "N. of Employees" \ "", "Cost of Employees" \ "", "Materials" \ "", "N. of Firms") ///
             title("Descriptive Statistics for Italian Firms in `years'")

frmttable, statmat(sumstat_29_`years') sdec(0,2,2,0,0) store(table_1) ///
             rtitles("NACE 29", "" \ "", "Capital" \ "", "Revenues" \ "", "N. of Employees" \ "", "Cost of Employees" \ "", "Materials" \ "", "N. of Firms") append		 
			 
outreg using "Tables/descriptive_stats_Italy_`years'.tex", replace ///
	replay(table_1) tex note("") fragment plain

}




*********************************
**## PROBLEM 2
*********************************

foreach var in `real_variables' L {
        gen ln_`var'=ln(`var')
        }

label variable ln_L "ln(labor)"
label variable ln_real_K "ln(capital)"


*---------------------------*
**### A
*---------------------------*

eststo clear

*Levinsohn-Petrin - VA

eststo: xi: levpet ln_real_VA if sector==13, free(ln_L i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
sca lp_b_13 = r(table)[1,1]
predict TFP_LP_13, omega
gen ln_TFP_LP_13=ln(TFP_LP_13)

eststo: xi: levpet ln_real_VA if sector==29, free(ln_L i.year) proxy(ln_real_M) capital(ln_real_K) reps(50) level(99)
sca lp_b_29 = r(table)[1,1]
predict TFP_LP_29, omega
gen ln_TFP_LP_29=ln(TFP_LP_29)

*Exporting in LaTex format
esttab using "Tables/table_problem_II.tex", replace label se star(* 0.10 ** 0.05 *** 0.01) keep(ln_L ln_real_K) mtitle("NACE 13" "NACE 29") b(%12.2f)

*Showing the result in STATA
esttab, label se star(* 0.10 ** 0.05 *** 0.01) keep(ln_L ln_real_K) mtitle("NACE 13" "NACE 29")


eststo clear

*Wooldridge - VA
eststo: xi: prodest ln_real_VA if sector==13, met(wrdg) free(ln_L) proxy(ln_real_M) state(ln_real_K) va
predict ln_TFP_WRDG_13, residuals
gen TFP_WRDG_13 = exp(ln_TFP_WRDG_13)

eststo: xi: prodest ln_real_VA if sector==29, met(wrdg) free(ln_L) proxy(ln_real_M) state(ln_real_K) va
predict ln_TFP_WRDG_29, residuals
gen TFP_WRDG_29 = exp(ln_TFP_WRDG_29)

*Exporting in LaTex format
esttab using "Tables/table_problem_II.tex", append label se star(* 0.10 ** 0.05 *** 0.01) keep(ln_L ln_real_K) mtitle("NACE 13" "NACE 29") b(%12.2f)

*Showing the result in STATA
esttab, label se star(* 0.10 ** 0.05 *** 0.01) keep(ln_L ln_real_K) mtitle("NACE 13" "NACE 29")

eststo clear


*OLS regression - VA
eststo: xi: reg ln_real_VA ln_L ln_real_K i.country i.year if sector==13
sca ols_b_13 = r(table)[1,1]
predict ln_TFP_OLS_13, residuals
gen TFP_OLS_13= exp(ln_TFP_OLS_13)

scalar bias_13 = ols_b_13 - lp_b_13
estadd sca bias_13

eststo: xi: reg ln_real_VA ln_L ln_real_K i.country i.year if sector==29
sca ols_b_29 = r(table)[1,1]
predict ln_TFP_OLS_29, residuals
gen TFP_OLS_29= exp(ln_TFP_OLS_29)

scalar bias_29 = ols_b_29 - lp_b_29
estadd sca bias_29

*Exporting in LaTex format
esttab using "Tables/table_problem_II.tex", append label se star(* 0.10 ** 0.05 *** 0.01) keep(ln_L ln_real_K) scalar(Bias) mtitle("NACE 13" "NACE 29") b(%12.2f)

*Showing the result in STATA
esttab, label se star(* 0.10 ** 0.05 *** 0.01) keep(ln_L ln_real_K) scalars(bias_13 bias_29) mtitle("NACE 13" "NACE 29")




*********************************
**## PROBLEM 4
*********************************

* Here we create a table for the distribution of TFP (OLS) for NACE 13 and 29, with and w/o outliers
tabstat TFP_OLS_13, statistics(mean min p1 p5 p50 p95 p99 max) save
return list
matrix TFP_OLS_descriptive_13 = r(StatTotal)'

qui sum TFP_OLS_13, d
replace TFP_OLS_13=. if !inrange(TFP_OLS_13,r(p1),r(p99))
tabstat TFP_OLS_13, statistics(mean min p1 p5 p50 p95 p99 max) save
return list
matrix TFP_OLS_clean_descriptive_13 = r(StatTotal)'


tabstat TFP_OLS_29, statistics(mean min p1 p5 p50 p95 p99 max) save
return list
matrix TFP_OLS_descriptive_29 = r(StatTotal)'

qui sum TFP_OLS_29, d
replace TFP_OLS_29=. if !inrange(TFP_OLS_29,r(p1),r(p99))
tabstat TFP_OLS_29, statistics(mean min p1 p5 p50 p95 p99 max) save
return list
matrix TFP_OLS_clean_descriptive_29 = r(StatTotal)'

frmttable, statmat(TFP_OLS_descriptive_13) sdec(3,3,3,3,3,3,3,3) ///
             rtitles("TFP 13") title("Distribution of Total Factor Productivity (OLS)")
frmttable, statmat(TFP_OLS_clean_descriptive_13) sdec(3,3,3,3,3,3,3,3) ///
             rtitles("TFP 13 clean") append
frmttable, statmat(TFP_OLS_descriptive_29) sdec(3,3,3,3,3,3,3,3) ///
             rtitles("TFP 29") append
frmttable, statmat(TFP_OLS_clean_descriptive_29) sdec(3,3,3,3,3,3,3,3) ///
             rtitles("TFP 29 clean") append
outreg using "Tables/TFP_OLS_descriptive.tex", replace ///
			 replay(table_tfp) tex note("") fragment plain



*---------------------------*
**### A
*---------------------------*


sum TFP_LP_13, d
replace TFP_LP_13=. if !inrange(TFP_LP_13,r(p1),r(p99))
sum TFP_WRDG_13, d
replace TFP_WRDG_13=. if !inrange(TFP_WRDG_13,r(p1),r(p99))
sum TFP_LP_29, d
replace TFP_LP_29=. if !inrange(TFP_LP_29,r(p1),r(p99))
sum TFP_WRDG_29,d 
replace TFP_WRDG_29=. if !inrange(TFP_WRDG_29, r(p1),r(p99))

save "DTA/EEI_TH_2022_clean.dta", replace

drop ln_TFP_LP_* ln_TFP_WRDG_*
gen ln_TFP_LP_13 = ln(TFP_LP_13)
gen ln_TFP_LP_29 = ln(TFP_LP_29)
gen ln_TFP_WRDG_13 = ln(TFP_WRDG_13)
gen ln_TFP_WRDG_29 = ln(TFP_WRDG_29)

twoway (kdensity TFP_LP_13, lw(medthick) color(maroon)) (kdensity TFP_WRDG_13, lw(medthick) color(edkblue)), title("NACE 13") xtitle("Total Factor Productivity") xscale(titlegap(*15)) name(TFP_13, replace) legend(pos(6) col(3) lab(1 "Levinsohn-Petrin") lab(2 "Wooldridge"))
twoway (kdensity TFP_LP_29, lw(medthick) color(maroon)) (kdensity TFP_WRDG_29, lw(medthick) color(edkblue)), title("NACE 29") xtitle("Total Factor Productivity") xscale(titlegap(*15)) name(TFP_29, replace) legend(pos(6) col(3) lab(1 "Levinsohn-Petrin") lab(2 "Wooldridge"))
grc1leg TFP_13 TFP_29, rows(1) altshrink
graph export "figures/TFP_P4_A.eps", replace

twoway (kdensity ln_TFP_LP_13, lw(medthick) color(maroon)) (kdensity ln_TFP_WRDG_13, lw(medthick) color(edkblue)), title("NACE 13") xtitle("Total Factor Productivity (ln)") xscale(titlegap(*15)) name(TFP_13_ln, replace)legend(pos(6) col(3) lab(1 "Levinsohn-Petrin") lab(2 "Wooldridge"))
twoway (kdensity ln_TFP_LP_29, lw(medthick) color(maroon)) (kdensity ln_TFP_WRDG_29, lw(medthick) color(edkblue)), title("NACE 29") xtitle("Total Factor Productivity (ln)") xscale(titlegap(*15)) name(TFP_29_ln, replace) legend(pos(6) col(3) lab(1 "Levinsohn-Petrin") lab(2 "Wooldridge"))
grc1leg TFP_13_ln TFP_29_ln, rows(1) altshrink
graph export "figures/TFP_ln_P4_A.eps", replace


*---------------------------*
**### B
*---------------------------*

twoway (kdensity ln_TFP_LP_13 if country=="Italy", lw(medthick) color(maroon)) (kdensity ln_TFP_LP_29 if country=="Italy", lw(medthick) color(maroon) lp(dash)) (kdensity ln_TFP_WRDG_13 if country=="Italy", lw(medthick) color(edkblue)) (kdensity ln_TFP_WRDG_29 if country=="Italy", lw(medthick) color(edkblue)  lp(dash)), title("Italy") xtitle("") xscale(titlegap(*15)) name(TFP_ln_Italy, replace) legend(pos(6) col(2) lab(1 "Levinsohn-Petrin, NACE 13") lab(2 "Levinsohn-Petrin, NACE 29") lab(3 "Wooldridge, NACE 13") lab(4 "Wooldridge, NACE 29") size(tiny))

twoway (kdensity ln_TFP_LP_13 if country=="Spain", lw(medthick) color(maroon)) (kdensity ln_TFP_LP_29 if country=="Spain", lw(medthick) color(maroon) lp(dash)) (kdensity ln_TFP_WRDG_13 if country=="Spain", lw(medthick) color(edkblue)) (kdensity ln_TFP_WRDG_29 if country=="Spain", lw(medthick) color(edkblue)  lp(dash)), title("Spain") xtitle("") xscale(titlegap(*15)) name(TFP_ln_Spain, replace) legend(pos(6) col(2) lab(1 "Levinsohn-Petrin, NACE 13") lab(2 "Levinsohn-Petrin, NACE 29") lab(3 "Wooldridge, NACE 13") lab(4 "Wooldridge, NACE 29") size(tiny))

twoway (kdensity ln_TFP_LP_13 if country=="France", lw(medthick) color(maroon)) (kdensity ln_TFP_LP_29 if country=="France", lw(medthick) color(maroon) lp(dash)) (kdensity ln_TFP_WRDG_13 if country=="France", lw(medthick) color(edkblue)) (kdensity ln_TFP_WRDG_29 if country=="France", lw(medthick) color(edkblue)  lp(dash)), title("France") xtitle("Total Factor Productivity (ln)") xscale(titlegap(*15)) name(TFP_ln_France, replace) legend(pos(6) col(2) lab(1 "Levinsohn-Petrin, NACE 13") lab(2 "Levinsohn-Petrin, NACE 29") lab(3 "Wooldridge, NACE 13") lab(4 "Wooldridge, NACE 29") size(tiny))

grc1leg TFP_ln_Italy TFP_ln_Spain TFP_ln_France, rows(3) iscale(*2) altshrink
graph export "figures/TFP_countries_ln_P4_B.eps", replace

twoway (kdensity TFP_LP_13 if country=="Italy", lw(medthick) color(maroon)) (kdensity TFP_LP_29 if country=="Italy", lw(medthick) color(maroon) lp(dash)) (kdensity TFP_WRDG_13 if country=="Italy", lw(medthick) color(edkblue)) (kdensity TFP_WRDG_29 if country=="Italy", lw(medthick) color(edkblue)  lp(dash)), title("Italy") xtitle("") xscale(titlegap(*15)) name(TFP_Italy, replace) legend(pos(6) col(2) lab(1 "Levinsohn-Petrin, NACE 13") lab(2 "Levinsohn-Petrin, NACE 29") lab(3 "Wooldridge, NACE 13") lab(4 "Wooldridge, NACE 29") size(tiny))

twoway (kdensity TFP_LP_13 if country=="Spain", lw(medthick) color(maroon)) (kdensity TFP_LP_29 if country=="Spain", lw(medthick) color(maroon) lp(dash)) (kdensity TFP_WRDG_13 if country=="Spain", lw(medthick) color(edkblue)) (kdensity TFP_WRDG_29 if country=="Spain", lw(medthick) color(edkblue)  lp(dash)), title("Spain") xtitle("") xscale(titlegap(*15)) name(TFP_Spain, replace) legend(pos(6) col(2) lab(1 "Levinsohn-Petrin, NACE 13") lab(2 "Levinsohn-Petrin, NACE 29") lab(3 "Wooldridge, NACE 13") lab(4 "Wooldridge, NACE 29") size(tiny))

twoway (kdensity TFP_LP_13 if country=="France", lw(medthick) color(maroon)) (kdensity TFP_LP_29 if country=="France", lw(medthick) color(maroon) lp(dash)) (kdensity TFP_WRDG_13 if country=="France", lw(medthick) color(edkblue)) (kdensity TFP_WRDG_29 if country=="France", lw(medthick) color(edkblue)  lp(dash)), title("France") xtitle("Total Factor Productivity") xscale(titlegap(*15)) name(TFP_France, replace) legend(pos(6) col(2) lab(1 "Levinsohn-Petrin, NACE 13") lab(2 "Levinsohn-Petrin, NACE 29") lab(3 "Wooldridge, NACE 13") lab(4 "Wooldridge, NACE 29") size(tiny))

grc1leg TFP_Italy TFP_Spain TFP_France, rows(3) iscale(*2) altshrink
graph export "figures/TFP_countries_P4_B.eps", replace

*---------------------------*
**### C
*---------------------------*

twoway (kdensity ln_TFP_LP_29 if country=="Italy" & year==2001, lw(medthick) color(maroon)) (kdensity ln_TFP_LP_29 if country=="Italy" & year==2008, lw(medthin) color(maroon) lp(dash)) (kdensity ln_TFP_LP_29 if country=="France" & year==2001,lw(medthick) color(edkblue)) (kdensity ln_TFP_LP_29 if country=="France" & year==2008,lw(medthin) color(edkblue) lp(dash)), title("Levinsohn-Petrin") xtitle("Total Factor Productivity (ln)") xscale(titlegap(*15)) ytitle("") name(Levinsohn_Petrin_TFP, replace) legend(pos(6) col(2) lab(1 "Italy, 2001") lab(2 "Italy, 2008") lab(3 "France, 2001") lab(4 "France, 2008"))

twoway (kdensity ln_TFP_WRDG_29 if country=="Italy" & year==2001, lw(medthick) color(maroon)) (kdensity ln_TFP_WRDG_29 if country=="Italy" & year==2008, lw(medthin) color(maroon) lp(dash)) (kdensity ln_TFP_WRDG_29 if country=="France" & year==2001,lw(medthick) color(edkblue)) (kdensity ln_TFP_WRDG_29 if country=="France" & year==2008,lw(medthin) color(edkblue) lp(dash)), title("Wooldridge") xtitle("Total Factor Productivity (ln)") xscale(titlegap(*15)) ytitle("") name(Wooldridge_TFP, replace) legend(pos(6) col(2) lab(1 "Italy, 2001") lab(2 "Italy, 2008") lab(3 "France, 2001") lab(4 "France, 2008"))

grc1leg Levinsohn_Petrin_TFP Wooldridge_TFP, rows(1) altshrink
graph export "figures/TFP_NACE29_ln_P4_C.eps", replace

twoway (kdensity TFP_LP_29 if country=="Italy" & year==2001, lw(medthick) color(maroon)) (kdensity TFP_LP_29 if country=="Italy" & year==2008, lw(medthin) color(maroon) lp(dash)) (kdensity TFP_LP_29 if country=="France" & year==2001,lw(medthick) color(edkblue)) (kdensity TFP_LP_29 if country=="France" & year==2008,lw(medthin) color(edkblue) lp(dash)), title("Levinsohn-Petrin") xtitle("Total Factor Productivity") xscale(titlegap(*15)) ytitle("") name(Levinsohn_Petrin_TFP, replace) legend(pos(6) col(2) lab(1 "Italy, 2001") lab(2 "Italy, 2008") lab(3 "France, 2001") lab(4 "France, 2008"))

twoway (kdensity TFP_WRDG_29 if country=="Italy" & year==2001, lw(medthick) color(maroon)) (kdensity TFP_WRDG_29 if country=="Italy" & year==2008, lw(medthin) color(maroon) lp(dash)) (kdensity TFP_WRDG_29 if country=="France" & year==2001,lw(medthick) color(edkblue)) (kdensity TFP_WRDG_29 if country=="France" & year==2008,lw(medthin) color(edkblue) lp(dash)), title("Wooldridge") xtitle("Total Factor Productivity") xscale(titlegap(*15)) ytitle("") name(Wooldridge_TFP, replace) legend(pos(6) col(2) lab(1 "Italy, 2001") lab(2 "Italy, 2008") lab(3 "France, 2001") lab(4 "France, 2008"))

grc1leg Levinsohn_Petrin_TFP Wooldridge_TFP, rows(1) altshrink
graph export "figures/TFP_NACE29_P4_C.eps", replace




*at baseline italian firm were more productive than french in 2001 and still in 2008. while 2008 crisis shifted the number if italian firm in the left tail, while in france in 2008 firms increased their productivity reaching italian productivity even though still a bit lower. 2008 crisis was more impactful just for italy 

*****************************
************* D *************
*****************************


**the largest density of obs is always found in the middle of the distrib
**but we cannot speack of symmetry bc the tail have different shapes

*****************************
************* E *************
*****************************

** look at the graphs. in the french case from 2001 to 2008 there was an homogeneous shift (nessun cambio di forma)
**while in the italian case the shift was not homogeneous





*********************************************************
********************** SECOND PART **********************
*********************************************************


*************************************
************* PROBLEM 5 *************
*************************************
{
	
*****************************
************* A *************
*****************************
use "DTA\Imports_US_China_Take_Home.dta", clear
*levelsof year
*Gli anni sono dal 1989 al 2006

egen nace_num = group(nace)
xtset nace_num year
gen delta_US_imports = l1.real_USimports_china - l6.real_USimports_china

drop nace_num

save "DTA\temp1.dta", replace

use "DTA\Imports_China_Take_Home.dta", clear
*levelsof year
*Gli anni sono dal 1988 al 2007

egen country_nace = group(country nace)
xtset country_nace year 

*gen lag6 = l6.real_imports_china
*gen lag1 = l1.real_imports_china
gen delta_imports = l1.real_imports_china - l6.real_imports_china

drop country_nace

merge m:m year nace using "DTA\temp1.dta", keepusing(delta_US_imports)

/*Normale che ci siano 84 not matched perchè abbiamo due anni in più nel master data (1988 e 2007)

    Result                      Number of obs
    -----------------------------------------
    Not matched                            84
        from master                        84  (_merge==1)
        from using                          0  (_merge==2)

    Matched                               756  (_merge==3)
    -----------------------------------------

*/

drop _merge
erase "DTA\temp1.dta"
sort country nace year

drop if delta_US_imports==. & delta_imports==.
drop real_imports_china

save "DTA\temp2.dta", replace

use "DTA\Employment_Shares_Take_Home.dta", clear
*levelsof year
*Dal 1988 al 2007

merge m:m country year nace using "DTA\temp2.dta"


/* I not matched sono dovuti al fatto che ho droppato quando la differenza tra i lag era missing a causa della mancanza dei dati oltre una certa data

    Result                      Number of obs
    -----------------------------------------
    Not matched                         5,057
        from master                     5,057  (_merge==1)
        from using                          0  (_merge==2)

    Matched                            11,743  (_merge==3)
    -----------------------------------------
*/

drop _merge
erase "DTA\temp2.dta"

sort country nuts2 nace year

gen product_china_shock = (empl/tot_empl_nuts2)*(delta_imports/tot_empl_country_nace)
gen product_china_shock_US = (empl/tot_empl_nuts2)*(delta_US_imports/tot_empl_country_nace)

bysort country nuts2 year: egen china_shock = sum(product_china_shock)
bysort country nuts2 year: egen china_shock_US = sum(product_china_shock_US)

replace china_shock=. if china_shock==0
*(5,040 real changes made, 5,040 to missing) 
*Il risultato è dovuto ai not matched del merge - 17 osservazioni che avevano missing ma che nel collapse successivo andranno via
*bro if _merge==1 &  china_shock!=0
replace china_shock_US=. if china_shock_US==0
drop delta_imports delta_US_imports product_china_shock product_china_shock_US

sort country nuts2 nace year
collapse (mean) china_shock china_shock_US, by(year country nuts2_name nuts2)
sort country nuts2 year

save "DTA\China_Shock.dta", replace

*****************************
************* B *************
*****************************

collapse (mean) china_shock china_shock_US, by(country nuts2_name)
save "DTA\China_Shock_cross_sec.dta", replace


*****************************
************* C *************
*****************************

*ssc install spmap
*ssc install shp2dta
*ssc install mif2dta
*ssc install geo2xy

*************
*preparing the dataset with shapefile data for the map
spshape2dta "Figure Support\NUTS_RG_03M_2021_4326", replace saving(nuts2)
use nuts2, clear
ren NUTS_ID nuts2_id
cap ren NAME_LATN nuts2_name
cap ren NUTS_NAME nuts2_name

replace nuts2_name="Andalucia" if nuts2_name=="Andalucía"
replace nuts2_name="Aragon" if nuts2_name=="Aragón"
replace nuts2_name="Bolzano" if nuts2_name=="Bolzano-Bozen"
replace nuts2_name="Castilla - La Mancha" if nuts2_name=="Castilla-La Mancha"
replace nuts2_name="Castilla y Leon" if nuts2_name=="Castilla y León"
replace nuts2_name="Cataluna" if nuts2_name=="Cataluña"
replace nuts2_name="Islas Baleares" if nuts2_name=="Illes Balears"
replace nuts2_name="Nord-Pas-de-Calais" if nuts2_name=="Nord-Pas de Calais"
replace nuts2_name="Pais Vasco" if nuts2_name=="País Vasco"
replace nuts2_name="Provence-Alpes-Côted'Azur" if nuts2_id=="FRL0"
replace nuts2_name="Region de Murcia" if nuts2_name=="Región de Murcia"
replace nuts2_name="Valle d'Aosta" if nuts2_id=="ITC2"
replace nuts2_name="Île-de-France" if nuts2_name=="Ile-de-France"
replace nuts2_name="Centre" if nuts2_name=="Centre — Val de Loire"	///to be asked to the TA if same

drop if LEVL_CODE==1
drop if nuts2_name=="Cantabria" & LEVL_CODE==3
drop if nuts2_name=="La Rioja" & LEVL_CODE==3
compress
sort _ID
save, replace
use nuts2_shp, clear
merge m:1 _ID using nuts2
drop if _m!=3
keep _ID _X _Y
keep if _X > -25 & _Y >30 // Get rid of the small islands
geo2xy _Y _X, proj (lambert_sphere) replace
sort _ID
save, replace
*************

*Figure: china shock
use "DTA\China_Shock_cross_sec.dta", clear
format china_shock %12.2f
merge m:m nuts2_name using nuts2.dta
drop if _merge!=3
spmap china_shock using nuts2_shp, id(_ID) fcolor(Blues2) legstyle(1) ///legenda(off) clnumber(5) to change the numbers of classes

graph rename Graph map_china_shock, replace
*graph export "Figures\map_china_shock.png", replace
graph export "figures\map_china_shock.eps", replace


*Figure: employment in the manufacturing sector pre-sample
use "DTA\Employment_Shares_Take_Home.dta", clear
collapse (mean) empl, by(country nuts2_name nace)
collapse (sum) empl, by(country nuts2_name)
merge m:m nuts2_name using nuts2.dta
drop if _merge!=3
spmap empl using nuts2_shp, id(_ID) fcolor(Blues2) ///legenda(off)

graph rename Graph map_empl, replace
*graph export "Figures\map_empl.png", replace
graph export "Figures\map_empl.eps", replace

erase nuts2.dta
erase nuts2_shp.dta

}

***************************************
************** Problem 6 **************
***************************************
{
	
use "DTA\China_Shock.dta", clear

collapse (mean) china_shock china_shock_US, by(nuts2)

save "DTA\temp3.dta", replace


use "DTA\EEI_TH_5d_2022_V2.dta", clear

keep if year>=2014 & year<=2017

bysort nuts_code nace2_2_group: gen share_tert_educ_base=share_tert_educ if year==2014
bysort nuts_code nace2_2_group: gen lnpop_base=lnpop if year==2014
bysort nuts_code nace2_2_group: gen control_gdp_base=control_gdp if year==2014

collapse (mean) tfp mean_uwage control_gdp_base share_tert_educ_base lnpop_base, by(nuts_code nace2_2_group ind cou)
rename nuts_code nuts2

merge m:1 nuts2 using "DTA\temp3.dta"

/*
    Result                      Number of obs
    -----------------------------------------
    Not matched                            25
        from master                        25  (_merge==1)
        from using                          0  (_merge==2)

    Matched                               658  (_merge==3)
    -----------------------------------------
*/



drop _merge

local Controls "control_gdp_base share_tert_educ_base lnpop_base"
local Countries_FEs "i.cou"
local Industries_FEs "i.ind"

egen nuts2_num = group(nuts2)
xtset nuts2_num

*******************
***** A and B *****
*******************

** OLS Regression
reghdfe tfp china_shock `Controls', absorb(cou ind) vce(cluster nuts2)
outreg2 using "Tables\Problem_VI_AB.tex", replace tex lab title("Problem VI - Dependent Variable: TFP Mean") ctitle("OLS") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, Wage Control, NO)

*reg tfp china_shock `Controls' `Countries_FEs' `Industries_FEs', vce(cluster nuts2)
*outreg2 using "Tables\Problem_VI_AB.tex", replace tex lab drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: TFP Mean") ctitle("OLS") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, Wage Control, NO)

** IV First Stage
ivreg2 tfp (china_shock=china_shock_US) `Controls' `Countries_FEs' `Industries_FEs', first savefirst cluster(nuts2)
return list
ereturn list
scalar F_stat = e(widstat)
est restore _ivreg2_china_shock
outreg2 using "Tables\Problem_VI_AB.tex", tex append drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: TFP Mean") ctitle("IV First Stage") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, Wage Control, NO) addstat("F-statistic IVs", F_stat)

** IV Second Stage
ivreg2 tfp (china_shock=china_shock_US) `Controls' `Countries_FEs' `Industries_FEs', first  cluster(nuts2)
outreg2 using "Tables\Problem_VI_AB.tex", tex append drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: TFP Mean") ctitle("IV Second Stage") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, Wage Control, NO)


******** With WAGE as control

** OLS Regression
reghdfe tfp china_shock `Controls' mean_uwage, absorb(cou ind) vce(cluster nuts2)
outreg2 using "Tables\Problem_VI_AB.tex", append tex lab title("Problem VI - Dependent Variable: TFP Mean") ctitle("OLS") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, Wage Control, YES)

*reg tfp china_shock `Controls' mean_uwage `Countries_FEs' `Industries_FEs', vce(cluster nuts2)
*outreg2 using "Tables\Problem_VI_AB.tex", append tex lab drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: TFP Mean") ctitle("OLS") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, Wage Control, YES)

** IV First Stage
ivreg2 tfp (china_shock=china_shock_US) `Controls' mean_uwage `Countries_FEs' `Industries_FEs', first savefirst cluster(nuts2)
return list
ereturn list
scalar F_stat = e(widstat)
est restore _ivreg2_china_shock
outreg2 using "Tables\Problem_VI_AB.tex", tex append drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: TFP Mean") ctitle("IV First Stage") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, Wage Control, YES) addstat("F-statistic IVs", F_stat)

** IV Second Stage
ivreg2 tfp (china_shock=china_shock_US) `Controls' mean_uwage `Countries_FEs' `Industries_FEs', first cluster(nuts2)
outreg2 using "Tables\Problem_VI_AB.tex", tex append drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: TFP Mean") ctitle("IV Second Stage") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, Wage Control, YES)

*****************
******* C *******
*****************

** OLS Regression
reghdfe mean_uwage china_shock `Controls' , absorb(cou ind) vce(cluster nuts2)
outreg2 using "Tables\Problem_VI_CD.tex", replace tex lab title("Problem VI - Dependent Variable: Wage Mean") ctitle("OLS") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, TFP Control, NO)

*reg mean_uwage china_shock `Controls' `Countries_FEs' `Industries_FEs' , vce(cluster nuts2)
*outreg2 using "Tables\Problem_VI_CD.tex", replace tex lab drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: Wage Mean") ctitle("OLS") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, TFP Control, NO)

** IV First Stage
ivreg2 mean_uwage (china_shock=china_shock_US) `Controls' `Countries_FEs' `Industries_FEs', first savefirst cluster(nuts2)
return list
ereturn list
scalar F_stat = e(widstat)
est restore _ivreg2_china_shock
outreg2 using "Tables\Problem_VI_CD.tex", tex append drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: Wage Mean") ctitle("IV First Stage") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, TFP Control, NO) addstat("F-statistic IVs", F_stat)

** IV Second Stage
ivreg2 mean_uwage (china_shock=china_shock_US) `Controls' `Countries_FEs' `Industries_FEs', first cluster(nuts2)
outreg2 using "Tables\Problem_VI_CD.tex", tex append drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: Wage Mean") ctitle("IV Second Stage") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, TFP Control, NO)

*****************
******* D *******
*****************

** OLS Regression
reghdfe mean_uwage china_shock `Controls' tfp, absorb(cou ind) vce(cluster nuts2)
outreg2 using "Tables\Problem_VI_CD.tex", append tex lab title("Problem VI - Dependent Variable: Wage Mean") ctitle("OLS") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, TFP Control, YES)

*reg mean_uwage china_shock `Controls' tfp `Countries_FEs' `Industries_FEs' , vce(cluster nuts2)
*outreg2 using "Tables\Problem_VI_CD.tex", append tex lab drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: Wage Mean") ctitle("OLS") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, TFP Control, YES)

** IV First Stage
ivreg2 mean_uwage (china_shock=china_shock_US) `Controls' tfp `Countries_FEs' `Industries_FEs', first savefirst cluster(nuts2)
return list
ereturn list
scalar F_stat = e(widstat)
est restore _ivreg2_china_shock
outreg2 using "Tables\Problem_VI_CD.tex", tex append drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: Wage Mean") ctitle("IV First Stage") br bdec(4) addtext(Controls, YES, Countries FEs, Industries FEs, YES, YES, Cluster, NUTS2, TFP Control, YES) addstat("F-statistic IVs", F_stat)

** IV Second Stage
ivreg2 mean_uwage (china_shock=china_shock_US) `Controls' tfp `Countries_FEs' `Industries_FEs', first  cluster(nuts2)
outreg2 using "Tables\Problem_VI_CD.tex", tex append drop(`Countries_FEs' `Industries_FEs') title("Problem VI - Dependent Variable: Wage Mean") ctitle("IV Second Stage") br bdec(4) addtext(Controls, YES, Countries FEs, YES, Industries FEs, YES, Cluster, NUTS2, TFP Control, YES)
}

***************************************
************** Problem 7 **************
***************************************
{
	
clear all

set maxvar 10000

use "DTA\ESS8MDWe02.1_F1.dta", clear

keep if cntry == "IT"

keep pspwght gndr agea edlvdit nuts2 prtvtbit

replace agea=. if agea==.d
replace edlvdit=. if edlvdit==5555 | edlvdit==.b | edlvdit==.c
replace prtvtbit=. if prtvtbit==.a | prtvtbit==.b | prtvtbit==.c

rename gndr gender
rename agea age
rename edlvdit education
rename prtvtbit party_voted

*tab party_voted
*tab education

label list prtvtbit
*Fratelli d'italia = 10
*Lega Nord = 9
label list gndr
*Male = 1
*Female = 2

gen rigth_vote = (party_voted==9 | party_voted==10)
gen male = (gender==1)

drop party_voted

save "DTA\ESS_wave8.dta", replace

use "DTA\China_Shock.dta", clear

keep if country=="Italy"

collapse (mean) china_shock china_shock_US, by(nuts2_name nuts2)

merge 1:m nuts2 using "DTA\ESS_wave8.dta"
drop if _merge==1 
*Droppo il molise perchè non esiste
drop _merge


local Controls "age gender"
local Education_FEs "i.education"

** OLS Regression
reghdfe rigth_vote china_shock `Controls' [pweight=pspwght], absorb(education) vce(cluster nuts2)
outreg2 using "Tables\Problem_VII.tex", replace tex lab nocons title("Problem VII - Dependent Variable: Right Vote") ctitle("OLS") br bdec(4) addtext(Controls, YES, Education FEs, YES)

** IV First Stage
ivreg2 rigth_vote (china_shock=china_shock_US) `Controls' `Education_FEs' [pweight=pspwght], first savefirst cluster(nuts2)
return list
ereturn list
scalar F_stat = e(widstat)
est restore _ivreg2_china_shock
outreg2 using "Tables\Problem_VII.tex", tex append drop(`Education_FEs') nocons title("Problem VII - Dependent Variable: Right Vote") ctitle("IV First Stage") br bdec(4) addtext(Controls, YES, Education FEs, YES) addstat("F-statistic IVs", F_stat)

** IV Second Stage
ivreg2 rigth_vote (china_shock=china_shock_US) `Controls' `Education_FEs' [pweight=pspwght], first cluster(nuts2)
outreg2 using "Tables\Problem_VII.tex", tex append drop(`Education_FEs') nocons title("Problem VII - Dependent Variable: Right Vote") ctitle("IV Second Stage") br bdec(4) addtext(Controls, YES, Education FEs, YES)


}







