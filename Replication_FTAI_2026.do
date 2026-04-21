*************************************************************
* Replication for "Environmental impacts of indigenous land restitution in Chile"
* Jaimovich et al. (Feb. 2026)

*Use this do file with the Maindata_FTAI_2026.dta dataset
*************************************************************
* Timestamps
display "Started at: " c(current_date) " " c(current_time)

*Install required libraries if not already installed
*ssc install estout, replace
*ssc install palettes, replace
*ssc install colrspace, replace

* Setting global style for plots
graph set window fontface "Arial"
graph set print fontface "Arial"
global FONTSIZE_SMALL small
global FONTSIZE_VSMALL vsmall

global esttab_options starlevels(* 0.1 ** 0.05 *** 0.01) ///
	   noconstant compress booktabs nonotes nonumbers	
							 
clear 
cd "/your/path/to/root/of/repo/goes/here" 

use "data\Maindata_FTAI_2026.dta", clear
xtset objectid year
set varabbrev off


***********************************************************
* Results in Table 1
***********************************************************

* Staggered DiD estimates using the procedure proposed by Callaway and Sant'Anna (2021, CS)
* STATA 18 or newer version required 

estimates clear
foreach var in grass crop natural plantation{  
	qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "results/CS_Table1.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}

* Two-way fixed effects DiD estimates (TWFE)

estimates clear
foreach var of varlist grass crop natural plantation{
	xtreg `var' FTAI i.year lclim_*, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeT1_`var'
}

esttab twfeT1_* using "results/TWFE_Table1.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)



***********************************************************
* Results in Table 2
* CS estimates, STATA 18 or newer version required 
***********************************************************

** Baseline: grassland
estimates clear
foreach var in base_Grass_grass base_Grass_crop base_Grass_natural base_Grass_plantation{
	qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "results/CS_Table2_grass.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) varlabels(r1vs0.FTAI"11") collab(none) nonumb append 
	clear matrix
}


** Baseline: cropland
estimates clear
foreach var in base_Crop_grass base_Crop_crop base_Crop_natural base_Crop_plantation{
	qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "results/CS_Table2_crop.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) varlabels(r1vs0.FTAI"11") collab(none) nonumb append 
	clear matrix
}


** Baseline: natural forest
estimates clear
foreach var in base_Natural_grass base_Natural_crop base_Natural_natural base_Natural_plantation{
	qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "results/CS_Table2_natural.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) varlabels(r1vs0.FTAI"11") collab(none) nonumb append 
	clear matrix
}


** Baseline: natural plantation

estimates clear
foreach var in base_Plants_grass base_Plants_crop base_Plants_natural base_Plants_plantation{
	qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "results/CS_Table2_plantation.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) varlabels(r1vs0.FTAI"11") collab(none) nonumb append 
	clear matrix
}



***********************************************************
* Results in Table 3
* Two-way fixed effects DiD estimates (TWFE), heterogeneous results
***********************************************************


sum distance, d
cap gen FTAI_far=1 if distance>r(p50) & FTAI==1 & distance!=.
cap gen FTAI_close=1 if distance<=r(p50) & FTAI==1 & distance!=.
replace FTAI_far=0 if FTAI_far==.  
replace FTAI_close=0 if FTAI_close==.  

cap gen FTAI_reservation=FTAI*d_reservation
replace FTAI_reservation=0 if FTAI_far==1 
replace FTAI_close=0 if FTAI_reservation==1  

gen FTAI_other=1 if FTAI==1 & FTAI_reservation==0 & FTAI_close==0 & FTAI_far==0
replace FTAI_other=0 if FTAI_other==.

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var'  FTAI_reservation FTAI_close FTAI_far i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfe_T3_`var'
}

esttab twfe_T3_* using "results/TWFE_Table3.tex", $esttab_options replace scalars(mean_depvar clusters plots)  keep(FTAI_reservation FTAI_close FTAI_far) se b(%9.3f) se(%9.3f)


***********************************************************
* Results in Figures 2, 3, and 4 
* CS estimates, STATA 18 or newer version required 
***********************************************************

* Fig. 2

estimates clear
foreach var in grass crop natural plantation{   
	qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)

	estat aggregation, dynamic(-10/10) graph(xlabel(,labsize(normal)) xline(0) legend(off) title("") ytitle("Treatment effect", size(vlarge)) xtitle("Time since restitution (years)", size(huge)) yscale(range(-0.1 0.1)) ylabel(-0.1(0.02)0.1) )
	graph export "results/Fig2_`var'.png", replace
	
	*DYNAMIC RESULTS
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...]
	esttab using "results/CS_dyn_Fig2.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb  append
}

* Fig. 3

estimates clear
foreach var in log_carbon biodiv trees_highero trees_lowero{
	qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)

	estat aggregation, dynamic(-10/10) graph(xlabel(,labsize(normal)) xline(0) legend(off) title("") ytitle("Treatment effect", size(vlarge)) xtitle("Time since restitution (years)", size(huge)) yscale(range(-0.1 0.1)) ylabel(-0.1(0.02)0.1) )
	graph export "results/Fig3_`var'.png", replace
	
	*DYNAMIC RESULTS
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...]
	esttab using "results/CS_dyn_Fig3.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb  append
}

* Fig. 4

estimates clear
foreach var in log_EVIgrass log_EVIplants log_EVInatural{ 
	qui: xthdidregress aipw (`var' lclim_*) (FTAI) if year>2003 & T_year>2004, group(objectid) vce(cluster community_ID) controlgroup(notyet)
	
	estat aggregation, dynamic(-10/10) graph(xlabel(,labsize(normal)) xline(0) legend(off) title("") ytitle("Treatment effect", size(vlarge)) xtitle("Time since restitution (years)", size(huge)) yscale(range(-0.1 0.1)) ylabel(-0.1(0.02)0.1) )
	graph export "results/Fig4_`var'.png", replace
	
	*DYNAMIC RESULTS
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...]
	esttab using "results/CS_dyn_Fig4.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb  append
}





***************************************************************************
* Appendix tables
***************************************************************************


log using "results/No_Table_Results_2026.log" /*for results not stored as table*/

***********************************************************
** Table A1: Descriptive statistics for land use
***********************************************************

* Main sample
sum grass crop natural plantation  

* 2001 and 2009
sum grass crop natural plantation if year==2001
sum grass crop natural plantation if year==2019

* Before and after treatment
sum grass crop natural plantation if t==-1
sum grass crop natural plantation if t==3
sum grass crop natural plantation if t==6

***********************************************************
** Table A2: Descriptive statistics for other variables
***********************************************************

**Upper panel (time-variant outcomes)

* Main sample
sum log_carbon biodiv trees_highero trees_lowero 
sum log_EVI* if year>2003 & T_year>2004 

* First and last year
sum log_carbon biodiv trees_highero trees_lowero if year==2001
sum log_EVI* if year==2004
sum log_carbon biodiv trees_highero trees_lowero log_EVI* if year==2019

* Before and after treatment
sum log_carbon biodiv trees_highero trees_lowero log_EVI* if t==-1
sum log_carbon biodiv trees_highero trees_lowero log_EVI* if t==3
sum log_carbon biodiv trees_highero trees_lowero log_EVI* if t==6

**Lower panel (time-invariant variables)

sum d_reservation distance families pj conflict90_plot_2km conflict90_pj_2km if year==2001, d /*The descriptives here are the same in each year*/

***********************************************************
** Table A3: Land cover transitions from before to after treatment
***********************************************************
// Table A3, first two columns
preserve
use  "data/lc_transitions.dta", clear

*Keep only pixels with no null data in both periods
keep if lc0!="missing" & lc1!="missing"

* Create dummies for t0 land covers
gen lc0_grass  = lc0=="grass"
gen lc0_crop   = lc0=="crop"
gen lc0_plant  = lc0=="plantation"
gen lc0_native = lc0=="native"
gen lc0_ambig  = lc0=="ambiguous"
gen lc0_other  = lc0=="other"

* Collapse the data at the property/baseline land cover level
collapse (mean) lc0_grass lc0_crop lc0_plant lc0_native lc0_ambig lc0_other, by(objectid)

* Report, for each base land cover, average graction at baseline and number of 
* properties with positive coverage
summ lc0_grass lc0_crop lc0_plant lc0_native lc0_ambig lc0_other

gen lc0_grass_gt0   =  (lc0_grass>0)
gen lc0_crop_gt0    =  (lc0_crop>0)
gen lc0_plant_gt0   =  (lc0_plant>0)
gen lc0_native_gt0  =  (lc0_native>0)
gen lc0_ambig_gt0   =  (lc0_ambig>0)
gen lc0_other_gt0   =  (lc0_other>0)

total lc0_grass_gt0 lc0_crop_gt0 lc0_plant_gt0 lc0_native_gt0 lc0_ambig_gt0 lc0_other_gt0
restore


preserve
// Table A3, rest of columns
use  "data/lc_transitions.dta", clear

*Keep only pixels with no null data in both periods
keep if lc0!="missing" & lc1!="missing"

* Create dummies for t+3 land covers
gen lc1_grass  = lc1=="grass"
gen lc1_crop   = lc1=="crop"
gen lc1_plant  = lc1=="plantation"
gen lc1_native = lc1=="native"
gen lc1_ambig  = lc1=="ambiguous"
gen lc1_other  = lc1=="other"

* Collapse the data at the property/baseline land cover level
collapse (mean) lc1_grass lc1_crop lc1_plant lc1_native lc1_ambig lc1_other, by(objectid lc0)

* Report, for each base land cover, average fraction of land that transitioned to each
* land cover class in t+3. This is the information used to fill Table A3
bysort lc0: summ lc1_grass lc1_crop lc1_plant lc1_native lc1_ambig lc1_other
restore


***********************************************************
** Table A4: Years since treatment 
***********************************************************

cap gen t2= year- T_year
tab t2
* The % of treated in the sample described in Table A3 referes to the number of treated properties between 2001-2019 (a total of 1,504 properties)



***********************************************************
*** Table A5: Plot-level characteristics by cohorts of treatment
***********************************************************

* by presidential periods
*Lagos
  cap gen T_2002_2005=(T_year>=2002 & T_year<=2005) 
  cap gen FTAI_2002_2005=T_2002_2005*FTAI
*Bachelete 1
  cap gen T_2006_2009=(T_year>=2006 & T_year<=2009) 
  cap gen FTAI_2006_2009=T_2006_2009*FTAI
*Piñera 1
  cap gen T_2010_2013=(T_year>=2010 & T_year<=2013) 
  cap gen FTAI_2010_2013=T_2010_2013*FTAI
*Bachelete 2
  cap gen T_2014_2017=(T_year>=2014 & T_year<=2017) 
  cap gen FTAI_2014_2017=T_2014_2017*FTAI
*Piñera 2
  cap gen T_2018_2021=(T_year>=2018 & T_year<=2021) 
  cap gen FTAI_2018_2021=T_2018_2021*FTAI

gen cohort=1 if T_2002_2005==1
replace cohort=2 if T_2006_2009==1
replace cohort=3 if T_2010_2013==1
replace cohort=4 if T_2014_2017==1
replace cohort=5 if T_2018_2021==1

bysort community_ID T_year year: gen plots_purchase=_N
bysort community_ID T_year year: egen total_area=sum(area)
gen density=total_area/families

foreach var of varlist grass crop natural plantation d_reservation conflict90_plot_2km conflict90_pj_2km {
replace `var'=`var'*100	
}

dtable  grass crop natural plantation area families total_area density plots_purchase d_reservation distance distance_capital distance_ruta5 pj conflict90_plot_2km conflict90_pj_2km if  year==2001, export("results/decstats_cohort.tex", replace) continuous(, statistics(mean sd)) by(cohort, nototals) nformat(%9.1f)

foreach var of varlist grass crop natural plantation d_reservation conflict90_plot_2km conflict90_pj_2km {
replace `var'=`var'/100	
}


***********************************************************
*** Table A6: Spillover effects (1km)
***********************************************************

estimates clear
foreach var of varlist grass crop natural plantation{
qui: xthdidregress aipw (`var'_1km lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)

	*AGGREGATED RESULTS 
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "results/CS_appendixA6.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var'_1km, lhs(Cohort)) varlabels(r1vs0.FTAI"11") collab(none) nonumb append 
	clear matrix
}

foreach var of varlist grass crop natural plantation{
	xtreg `var'_1km FTAI i.year lclim_*, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var'_1km if e(sample)
	estadd scalar mean_depvar= r(mean)
	sum `var'_1km if e(sample) & year==2001
	estadd scalar mean_2001= r(mean)
	sum `var'_1km if e(sample) & year==2019
	estadd scalar mean_2019= r(mean)
	est sto twfeA6_`var'
}

esttab twfeA6_* using "results/TWFE_appendixA6.tex", $esttab_options scalars(mean_depvar mean_2001 mean_2019 clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)


***********************************************************
*** Table A7: First restitution by each community
***********************************************************

bysort community_ID (year): egen T_year_min = min(T_year)
gen T_first=1
replace T_first=0 if T_year > T_year_min + 1

estimates clear
foreach var of varlist grass crop natural plantation{
qui: xthdidregress aipw (`var' lclim_*) (FTAI) if T_first==1, group(objectid) vce(cluster community_ID) controlgroup(notyet)

	*AGGREGATED RESULTS 
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "results/CS_appendixA7.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var'_1km, lhs(Cohort)) varlabels(r1vs0.FTAI"11") collab(none) nonumb append 
	clear matrix
}

foreach var of varlist grass crop natural plantation{
	xtreg `var' FTAI i.year lclim_*  if T_first==1, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var'_1km if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeA7_`var'
}

esttab twfeA7_* using "results/TWFE_appendixA7.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)


***********************************************************
*** Table A8: Robustness of main land use change results to alternate controls
***********************************************************

***Panel A 

estimates clear
foreach var of varlist grass crop natural plantation{
	xtreg `var' FTAI  i.year lclim_* escolaridad - lny, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfecasen_`var'
}
esttab twfecasen_* using "results/TWFE_appendixA8_A.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)


***Panel B

estimates clear
foreach var in grass crop natural plantation{  
	qui: xthdidregress aipw (`var' lclim_* escolaridad - lny) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "results/CS_appendixA8_B.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}


***Panel C

estimates clear
foreach var of varlist grass crop natural plantation{
	xtreg `var' FTAI  i.year bin_* lclim_rain_* SDrain lag*, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeclim2_`var'
}
esttab twfeclim2_* using "results/TWFE_appendixA8_C.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)


***Panel D

estimates clear
foreach var in grass crop natural plantation{  
	qui: xthdidregress aipw (`var'  bin_* lclim_rain_*  SDrain lag*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "results/CS_appendixA8_D.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}


***Panel E
gen ldist_capital = log(distance_capital)
gen ldist_ruta5   = log(distance_ruta5)
estimates clear
foreach var in grass crop natural plantation{  
	qui: xthdidregress aipw (`var' lclim_* wheaty ldist_capital ldist_ruta5) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "results/CS_appendixA8_E.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}


***********************************************************
*** Table A9: Results using original data sources
***********************************************************

estimates clear
foreach var of varlist forest_all plantation_mapbiomas natural_mapbiomas mosaic_mapbiomas{
qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)

	*AGGREGATED RESULTS 
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "results/CS_appendixA9.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) varlabels(r1vs0.FTAI"11") collab(none) nonumb append 
	clear matrix
}

foreach var of varlist forest_all plantation_mapbiomas natural_mapbiomas mosaic_mapbiomas{
	xtreg `var' FTAI i.year lclim_*, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeA9_`var'
}

esttab twfeA9_* using "results/TWFE_appendixA9.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)


***********************************************************
*** Table A10: Short-term effects: TWFE only for t+5
***********************************************************

***Panel A 

gen sample_t5=0
replace sample_t5=1 if t<=5
replace sample_t5=1 if T_year>2019

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var'  FTAI  i.year lclim_* if sample_t5==1, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfe_t5_`var'
}

esttab twfe_t5_* using "results/TWFE_appendixA10_A.tex", $esttab_options replace scalars(mean_depvar clusters plots)  keep(FTAI) se b(%9.3f) se(%9.3f)


***Panel B
bysort objectid:egen yy=max(t)
gen sample_t5_balanced=0
replace sample_t5_balanced=1 if t<=5
replace sample_t5_balanced=0 if yy<5
drop yy

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var'  FTAI  i.year lclim_* if sample_t5_balanced==1, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfe_t5b_`var'
}

esttab twfe_t5b_* using "results/TWFE_appendixA10_B.tex", $esttab_options replace scalars(mean_depvar clusters plots)  keep(FTAI) se b(%9.3f) se(%9.3f)



***********************************************************
*** Table A11: Aggregated results for other variables 
***********************************************************

* CS estimates in the upper panel 

estimates clear
foreach var in log_carbon biodiv trees_highero trees_lowero{
    
	qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "results/CS_appendixA11.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}

foreach var in log_EVIgrass log_EVIcrop log_EVInatural log_EVIplants{
	qui: xthdidregress aipw (`var' lclim_*) (FTAI) if year>2003 & T_year>2004, group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "results/CS_appendixA11.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}

* TWFE estimates in the lower panel 

estimates clear
foreach var of varlist log_carbon biodiv trees_highero trees_lowero{
	xtreg `var' FTAI i.year lclim_*, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeA11_`var'
}

foreach var of varlist log_EVI*{
	xtreg `var' FTAI i.year lclim_*  if year>2003 & T_year>2004 , fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeA11_`var'
}

esttab twfeA11_* using "results/TWFE_appendixA11.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)



***********************************************************
*** Table A12: Heterogeneous effects by predominant land use at baseline (2001)
***********************************************************

egen tt=rowmax(grass crop natural plantation)  if year==2001
foreach var of varlist grass crop natural plantation{
	gen T_basemax`var'=0
    replace T_basemax`var'=1 if `var'==tt
	bysort objectid: egen tt2=max(T_basemax`var')
	replace T_basemax`var'=1 if T_basemax`var'!=tt2
	gen FTAI_basemax_`var'=FTAI*T_basemax`var'
	drop tt2
}
drop tt


estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var'  FTAI_basemax* i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfe_base_`var'
}

esttab twfe_base_* using "results/TWFE_appendixA12.tex", $esttab_options replace scalars(mean_depvar clusters plots)  keep(FTAI*) se b(%9.3f) se(%9.3f)



***********************************************************
*** Table A13: Heterogeneous effects by erodability
***********************************************************

** Panel A

estimates clear

foreach var of varlist grass crop natural plantation{
xtreg `var'_lowero FTAI i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfel_`var'
}

esttab twfel_* using "results/TWFE_appendixA13_A.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)


** Panel B

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var'_highero FTAI i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfeh_`var'
}

esttab twfeh_* using "results/TWFE_appendixA13_B.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)



***********************************************************
*** Table A14: Heterogeneous effects by community characteristics and conflict
***********************************************************

cap gen log_distance=log(distance)
cap gen FTAI_ldist=log_distance*FTAI
cap gen log_families=log(families)
cap gen FTAI_lfam=log_families*FTAI


** Panel A

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var' FTAI FTAI_ldist i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfed_`var'
}

esttab twfed_* using "results/TWFE_appendixA14_A.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)


** Panel B

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var' FTAI FTAI_lfam i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfef_`var'
}

esttab twfef_* using "results/TWFE_appendixA14_B.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)


** Panel C

bysort cod_region: egen temp=median(pj)
cap gen old_community=0 if pj!=.
replace old_community=1 if pj<temp
drop temp
cap gen new_community=old_community+1
replace new_community=0 if new_community==2
cap gen FTAI_old=FTAI*old_community
cap gen FTAI_new=FTAI*new_community

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var'  FTAI_old FTAI_new i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfe_old_`var'
}

esttab twfe_old_* using "results/TWFE_appendixA14_C.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(FTAI*) se b(%9.3f) se(%9.3f)


** Panel D

foreach buffer in 500m 1km 2km{
	cap gen FTAI_conflict90_plot_`buffer'=FTAI*conflict90_plot_`buffer'
}

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var'  FTAI  FTAI_conflict90_plot_2km i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfe_confl_`var'

}

esttab twfe_confl_* using "results/TWFE_appendixA14_D.tex", $esttab_options replace scalars(mean_depvar clusters plots)  keep(FTAI*) se b(%9.3f) se(%9.3f)


** Panel E

foreach buffer in 500m 1km 2km{
	cap gen FTAI_conflict90_pj_`buffer'=FTAI*conflict90_pj_`buffer'
}

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var'  FTAI  FTAI_conflict90_pj_2km  i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfe_conflpj_`var'

}

esttab twfe_conflpj_* using "results/TWFE_appendixA14_E.tex", $esttab_options replace scalars(mean_depvar clusters plots)  keep(FTAI*) se b(%9.3f) se(%9.3f)



***********************************************************
** Table A15: Bacon decomposition
*********************************************************** 

foreach var in grass crop natural plantation{
xtdidregress  (`var') (FTAI), group(objectid) time(year) vce(cluster community_ID)
estat bdecomp, summaryonly
}


***********************************************************
** Table B5: EVI Validation
*********************************************************** 

* Column 1
preserve
use "data/EVI_validation.dta", replace
sum ln_EVI_crop, d
keep if inrange(ln_EVI_crop, r(p5),r(p95))
reg ln_valuecrop ln_EVI_crop, r
restore

* Column 2
preserve
use "data/EVI_validation.dta", replace
sum ln_EVI_grass, d
keep if inrange(ln_EVI_grass, r(p5),r(p95))
reg ln_LSU_grass ln_EVI_grass, r
restore


log close


***************************************************************************
* Appendix figures
***************************************************************************

***********************************************************
** Figure A1: Land cover by ethnicity
*********************************************************
preserve
use  "data/census1997.dta", replace

* count number of observations by ethnicity to report in table's notes
table mapuche

* collapse land cover use data
collapse (sum) CultivosAP_superficie ForrajerasPR_superficie PraderasME_superficie PraderasNA_superficie Barbecho_superficie Infra_superficie TerrenoEO_superficie Plant_forestal Bosque_nativo SuperficieTotal, by(mapuche)

* express as fraction of total
foreach var of varlist CultivosAP_superficie ForrajerasPR_superficie PraderasME_superficie PraderasNA_superficie Barbecho_superficie Infra_superficie TerrenoEO_superficie Plant_forestal Bosque_nativo {
	gen super`var' = `var'/SuperficieTotal
}

* clear and reshape to long
keep mapuche super* 
reshape long super, i(mapuche) j(superficie) string

* create new variables with clear labels and order (land cover)
gen superficie_order = .
replace superficie_order = 1 if superficie=="CultivosAP_superficie"
replace superficie_order = 2 if superficie=="Barbecho_superficie"
replace superficie_order = 3 if superficie=="ForrajerasPR_superficie"
replace superficie_order = 4 if superficie=="PraderasME_superficie"
replace superficie_order = 5 if superficie=="PraderasNA_superficie"
replace superficie_order = 6 if superficie=="Bosque_nativo"
replace superficie_order = 7 if superficie=="Plant_forestal"
replace superficie_order = 8 if superficie=="Infra_superficie"
replace superficie_order = 9 if superficie=="TerrenoEO_superficie"

label define superficie_lbl ///
    1 "Crops" ///
    2 "Fallow" ///
    3 "Foragers" ///	
    4 "Improved Grasslands" ///
    5 "Natural Grasslands" ///
    6 "Natural Forest" ///
    7 "Forest Plantations" ///
    8 "Infrastructure" ///
    9 "Barren Land"

label values superficie_order superficie_lbl

* create new variables with clear labels and order (ethnicity)
gen     m = 1  if mapuche==1
replace m = 0  if mapuche==0

label define m_lbl ///
    0 "NM" ///
    1 "M"
label values m m_lbl
	
* create plot
colorpalette cividis
graph hbar (mean) super, ///
      over(m, label(labsize($FONTSIZE_VSMALL))) ///
	  over(superficie_order, label(labsize($FONTSIZE_VSMALL))) ///
      title ("Land Cover by Ethnicity", size($FONTSIZE_SMALL)) ///
	  ytitle("Percent of total land", size($FONTSIZE_SMALL)) ///
	  ylabel(, labsize($FONTSIZE_VSMALL)) ///
	  bar(1, color(`r(p3)')) 
graph export "results/Fig_sup_census.pdf", replace
restore

***********************************************************
** Figure A2: Cohort-specific event study for grasslands
*********************************************************
qui: xthdidregress aipw (grass lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
estat atetplot, title (, size($FONTSIZE_SMALL)) ///
	  ytitle("ATET", size($FONTSIZE_SMALL)) ///
	  ylabel(, labsize($FONTSIZE_VSMALL)) legend(off)
graph export "results/FigS2.pdf", replace ///
    width(7.0866) 


***********************************************************
** Figure A3: Cohort-specific event study for plantations
*********************************************************
qui: xthdidregress aipw (plantation lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
estat atetplot, title (, size($FONTSIZE_SMALL)) ///
	  ytitle("ATET", size($FONTSIZE_SMALL)) ///
	  ylabel(, labsize($FONTSIZE_VSMALL)) legend(off)
graph export "results/FigS3.pdf", replace ///
    width(7.0866) 


***********************************************************
** Figure A4: Cohort-specific event study for natural forests
*********************************************************
qui: xthdidregress aipw (natural lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
estat atetplot, title (, size($FONTSIZE_SMALL)) ///
	  ytitle("ATET", size($FONTSIZE_SMALL)) ///
	  ylabel(, labsize($FONTSIZE_VSMALL)) legend(off)
graph export "results/FigS4.pdf", replace ///
    width(7.0866) 



***********************************************************
* Figure Results
***********************************************************



estimates clear
foreach var in log_carbon biodiv trees_lowero trees_highero{  
	qui: xthdidregress aipw (`var' lclim_*) (FTAI), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "results/CS_Table_Bio.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}
estimates clear
foreach var in log_EVIgrass log_EVInatural log_EVIplants{  
	qui: xthdidregress aipw (`var' lclim_*) (FTAI) if year>2003 & T_year>2004, group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "results/CS_Table_EVI.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}
* Timestamps
display "Finished at: " c(current_date) " " c(current_time)