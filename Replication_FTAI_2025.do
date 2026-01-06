*************************************************************
* Replication for "Environmental impacts of indigenous land restitution in Chile"
* Jaimovich et al. (August, 2025)

*Use this do file with the Maindata_FTAI_2025.dta dataset
*************************************************************

*Install estout if not already installed
*ssc install estout, replace

global esttab_options starlevels(* 0.1 ** 0.05 *** 0.01) ///
	   noconstant compress booktabs nonotes nonumbers	
							 
clear 
*use Maindata_FTAI_2025.dta	/* here use the path to Maindata_FTAI_2025.dta*/
xtset objectid year
set varabbrev off


***********************************************************
* Results in Table 1
***********************************************************

* Staggered DiD estimates using the procedure proposed by Callaway and Sant'Anna (2021, CS)
* STATA 18 or newer version required 

estimates clear
foreach var in grass crop natural plantation{  
	qui: xthdidregress aipw (`var' lclim_*) (treat), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "CS_Table1.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}

* Two-way fixed effects DiD estimates (TWFE)

estimates clear
foreach var of varlist grass crop natural plantation{
	xtreg `var' treat  i.year lclim_*, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeT1_`var'
}

esttab twfeT1_* using "TWFE_Table1.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)



***********************************************************
* Results in Table 2
* CS estimates, STATA 18 or newer version required 
***********************************************************


estimates clear
foreach var in base_Grass_grass base_Grass_crop base_Grass_natural base_Grass_plantation ///
               base_Crop_grass base_Crop_crop base_Crop_natural base_Crop_plantation ///
			   base_Natural_grass base_Natural_crop base_Natural_natural base_Natural_plantation ///
			   base_Plants_grass base_Plants_crop base_Plants_natural base_Plants_plantation {

	qui: xthdidregress aipw (`var' lclim_*) (treat), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "CS_Table2.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) varlabels(r1vs0.treat "11") collab(none) nonumb append 
	clear matrix
}



***********************************************************
* Results in Table 3
* Two-way fixed effects DiD estimates (TWFE), heterogeneous results
***********************************************************


sum distance, d
cap gen treat_far=1 if distance>r(p50) & treat==1 & distance!=.
cap gen treat_close=1 if distance<=r(p50) & treat==1 & distance!=.
replace treat_far=0 if treat_far==.  
replace treat_close=0 if treat_close==.  

cap gen treat_reservation=treat*d_reservation
replace treat_reservation=0 if treat_far==1 
replace treat_close=0 if treat_reservation==1  

gen treat_other=1 if treat==1 & treat_reservation==0 & treat_close==0 & treat_far==0
replace treat_other=0 if treat_other==.

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var'  treat_reservation treat_close treat_far i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfe_T3_`var'
}

esttab twfe_T3_* using "TWFE_Table3.tex", $esttab_options replace scalars(mean_depvar clusters plots)  keep(treat_reservation treat_close treat_far) se b(%9.3f) se(%9.3f)


***********************************************************
* Results in Figures 2, 3, and 4 
* CS estimates, STATA 18 or newer version required 
***********************************************************

* Fig. 2

estimates clear
foreach var in grass crop natural plantation{   
	qui: xthdidregress aipw (`var' lclim_*) (treat), group(objectid) vce(cluster community_ID) controlgroup(notyet)

	estat aggregation, dynamic(-10/10) graph(xlabel(,labsize(normal)) xline(0) legend(off) title("") ytitle("Treatment effect", size(vlarge)) xtitle("Time since restitution (years)", size(huge)) yscale(range(-0.1 0.1)) ylabel(-0.1(0.02)0.1) )
	graph export "Fig2_`var'.png", replace
	
	*DYNAMIC RESULTS
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...]
	esttab using "CS_dyn_Fig2.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb  append
}

* Fig. 3

estimates clear
foreach var in log_carbon biodiv trees_highero trees_lowero{
	qui: xthdidregress aipw (`var' lclim_*) (treat), group(objectid) vce(cluster community_ID) controlgroup(notyet)

	estat aggregation, dynamic(-10/10) graph(xlabel(,labsize(normal)) xline(0) legend(off) title("") ytitle("Treatment effect", size(vlarge)) xtitle("Time since restitution (years)", size(huge)) yscale(range(-0.1 0.1)) ylabel(-0.1(0.02)0.1) )
	graph export "Fig3_`var'.png", replace
	
	*DYNAMIC RESULTS
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...]
	esttab using "CS_dyn_Fig3.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb  append
}

* Fig. 4

estimates clear
foreach var in log_EVIgrass log_EVIplants log_EVInatural{ 
	qui: xthdidregress aipw (`var' lclim_*) (treat) if year>2003 & T_year>2004, group(objectid) vce(cluster community_ID) controlgroup(notyet)
	
	estat aggregation, dynamic(-10/10) graph(xlabel(,labsize(normal)) xline(0) legend(off) title("") ytitle("Treatment effect", size(vlarge)) xtitle("Time since restitution (years)", size(huge)) yscale(range(-0.1 0.1)) ylabel(-0.1(0.02)0.1) )
	graph export "Fig4_`var'.png", replace
	
	*DYNAMIC RESULTS
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...]
	esttab using "CS_dyn_Fig4.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb  append
}




***************************************************************************
* Appendix tables
***************************************************************************


log using FTAI_2025 /*for results not stored as table*/

***********************************************************
** Table A1: Descriptive statistics for land use
***********************************************************

* Main sample
sum grass crop natural plantation  

* First and last year
sum grass crop natural plantation if year==2001
sum grass crop natural plantation if year==2019

* Before and after treatment
cap gen t= year- T_year
replace t=. if T_year>2019
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

sum d_reservation distance families area if year==2001, d /*The descriptives here are the same in each year*/

***********************************************************
** Table A3: Years since treatment 
***********************************************************

tab t
* The % of treated in the sample described in Table A3 referes to the number of treated properties between 2001-2019 (a total of 1,504 properties)

***********************************************************
** Table A4: Bacon decomposition
*********************************************************** 

foreach var in grass crop natural plantation{
xtdidregress  (`var') (treat), group(objectid) time(year) vce(cluster community_ID)
estat bdecomp, summaryonly
}


log close

***********************************************************
*** Table A5: Aggregated results for other variables 
***********************************************************

* CS estimates in the upper panel 

estimates clear
foreach var in log_carbon biodiv trees_highero trees_lowero{
    
	qui: xthdidregress aipw (`var' lclim_*) (treat), group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "CS_appendixA5.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}

foreach var in log_EVIgrass log_EVIcrop log_EVInatural log_EVIplants{
	qui: xthdidregress aipw (`var' lclim_*) (treat) if year>2003 & T_year>2004, group(objectid) vce(cluster community_ID) controlgroup(notyet)
	estat aggregation
	estadd matrix b_a2= r(table)["b", 1 ...]
	estadd matrix se_a2 = r(table)["se", 1 ...]
	estadd matrix p_a2= r(table)["pvalue", 1 ...]
	esttab using "CS_appendixA5.tex", $esttab_options cells(b_a2(fmt(3) star pval(p_a2)) se_a2(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) collab(none) nonumb   append
    clear matrix
}

* TWFE estimates in the lower panel 

estimates clear
foreach var of varlist log_carbon biodiv trees_highero trees_lowero{
	xtreg `var' treat  i.year lclim_*, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeA5_`var'
}

foreach var of varlist log_EVI*{
	xtreg `var' treat  i.year lclim_*  if year>2003 & T_year>2004 , fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeA5_`var'
}

esttab twfeA5_* using "TWFE_appendixA5.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)

***********************************************************
*** Table A6: Additional heterogeneity results
***********************************************************

cap gen log_distance=log(distance)
cap gen treat_ldist=log_distance*treat
cap gen log_families=log(families)
cap gen treat_lfam=log_families*treat
cap gen log_area=log(area)
cap gen treat_larea=log_area*treat

estimates clear
foreach var of varlist grass crop natural plantation{
xtreg `var' treat treat_ldist i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfed_`var'

xtreg `var' treat treat_lfam i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfef_`var'

xtreg `var' treat treat_larea i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfear_`var'

xtreg `var'_lowero treat i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfel_`var'

xtreg `var'_highero treat i.year lclim_*, fe vce(cluster community_ID)
estadd scalar clusters= e(N_clust)
estadd scalar plots= e(N_g)
sum `var' if e(sample)
estadd scalar mean_depvar= r(mean)
est sto twfeh_`var'

}

esttab twfed_* using "TWFE_appendixA6_distance.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)
esttab twfef_* using "TWFE_appendixA6_families.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)
esttab twfear_* using "TWFE_appendixA6_area.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)
esttab twfel_* using "TWFE_appendixA6_lowero.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)
esttab twfeh_* using "TWFE_appendixA6_highero.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)


***********************************************************
*** Table A7: Results using original data sources
***********************************************************

estimates clear
foreach var of varlist forest_all plantation_mapbiomas natural_mapbiomas mosaic_mapbiomas{
qui: xthdidregress aipw (`var' lclim_*) (treat), group(objectid) vce(cluster community_ID) controlgroup(notyet)

	*AGGREGATED RESULTS 
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "CS_appendixA7.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var', lhs(Cohort)) varlabels(r1vs0.treat "11") collab(none) nonumb append 
	clear matrix
}

foreach var of varlist forest_all plantation_mapbiomas natural_mapbiomas mosaic_mapbiomas{
	xtreg `var' treat  i.year lclim_*, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var' if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeA7_`var'
}

esttab twfeA7_* using "TWFE_appendixA7_other.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)


***********************************************************
*** Table A8: Spillover effects (1km)
***********************************************************

estimates clear
foreach var of varlist grass crop natural plantation{
qui: xthdidregress aipw (`var'_1km lclim_*) (treat), group(objectid) vce(cluster community_ID) controlgroup(notyet)

	*AGGREGATED RESULTS 
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "CS_appendixA8.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var'_1km, lhs(Cohort)) varlabels(r1vs0.treat "11") collab(none) nonumb append 
	clear matrix
}

foreach var of varlist grass crop natural plantation{
	xtreg `var'_1km treat  i.year lclim_*, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var'_1km if e(sample)
	estadd scalar mean_depvar= r(mean)
	sum `var'_1km if e(sample) & year==2001
	estadd scalar mean_2001= r(mean)
	sum `var'_1km if e(sample) & year==2019
	estadd scalar mean_2019= r(mean)
	est sto twfeA8_`var'
}

esttab twfeA8_* using "TWFE_appendixA8.tex", $esttab_options scalars(mean_depvar mean_2001 mean_2019 clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)

***********************************************************
*** Table A9: First restitution by each community
***********************************************************

bysort community_ID (year): egen T_year_min = min(T_year)
gen T_first=1
replace T_first=0 if T_year > T_year_min + 1

estimates clear
foreach var of varlist grass crop natural plantation{
qui: xthdidregress aipw (`var' lclim_*) (treat) if T_first==1, group(objectid) vce(cluster community_ID) controlgroup(notyet)

	*AGGREGATED RESULTS 
	estat aggregation
	estadd matrix b_a= r(table)["b", 1 ...]
	estadd matrix se_a = r(table)["se", 1 ...]
	estadd matrix p_a= r(table)["pvalue", 1 ...] 
	esttab using "CS_appendixA9.tex", $esttab_options cells(b_a(fmt(3) star pval(p_a)) se_a(fmt(3) par)) ///
	eqlab(none) mlab(`var'_1km, lhs(Cohort)) varlabels(r1vs0.treat "11") collab(none) nonumb append 
	clear matrix
}

foreach var of varlist grass crop natural plantation{
	xtreg `var' treat  i.year lclim_*  if T_first==1, fe vce(cluster community_ID)
	estadd scalar clusters= e(N_clust)
	estadd scalar plots= e(N_g)
	sum `var'_1km if e(sample)
	estadd scalar mean_depvar= r(mean)
	est sto twfeA9_`var'
}

esttab twfeA9_* using "TWFE_appendixA9.tex", $esttab_options scalars(mean_depvar clusters plots)  replace keep(treat*) se b(%9.3f) se(%9.3f)
