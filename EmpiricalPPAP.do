* Clear all existing data and settings
clear all

* Set the working directory
cd "D:\ECON\admission\working paper\PPAP"

**************************************
*** Structure of Global Variables ***
**************************************

* Define the base list of variables
global base_vars "area_agriculture_k havesting_machine_k fertilizer_k area_gross_k enterprises_k student_k expenditure_k telecommunication_k"

* Define global variable x0 with base variables and ecostr_k
global x0 "$base_vars ecostr_k"

* Define global variable x1, same as x0 but without ecostr_k (for mechanism testing)
global x1 "$base_vars"

* Note:
* x0 includes ecostr_k and is used for the main analysis
* x1 excludes ecostr_k and is used for mechanism testing


**************************************
*** Descriptive Statistics  ***
**************************************


use finaldata.dta, clear

sum2docx income_rural_k savings_k GTFP_unexpected igg agriculture_power_k agriculture_output_k carbon_k area_agriculture_k havesting_machine_k fertilizer_k area_gross_k enterprises_k ecostr_k student_k expenditure_k telecommunication_k using Table1_sum01.docx, replace stats(N mean(%19.3f) sd min median max)

sum2docx income_rural_k savings_k GTFP_unexpected igg agriculture_power_k agriculture_output_k carbon_k area_agriculture_k havesting_machine_k fertilizer_k area_gross_k enterprises_k ecostr_k student_k expenditure_k telecommunication_k if treat==1 using Table1_sum02.docx, replace stats(N mean(%19.3f) sd min median max)

sum2docx income_rural_k savings_k GTFP_unexpected igg agriculture_power_k agriculture_output_k carbon_k area_agriculture_k havesting_machine_k fertilizer_k area_gross_k enterprises_k ecostr_k student_k expenditure_k telecommunication_k if treat==0  using Table1_sum03.docx,replace stats(N mean(%19.3f) sd min median max)




**************************************
*** Baseline Regression  ***
**************************************

// Table1 OK

use finaldata.dta, clear


reghdfe income_rural_k did $x0, absorb(id year)
est store m1

reghdfe savings_k did $x0, absorb(id year)
est store m2

reghdfe igg did $x0, absorb(id year)
est store m3

reghdfe GTFP_unexpected did $x0, absorb(id year)
est store m4

esttab m1 m2 m3 m4 using result1.rtf,replace b(5) p(5) ar2(4) compress nogap star(* 0.1 ** 0.05 *** 0.01)




* Repeat for models with robust standard errors
reghdfe income_rural_k did $x0, absorb(id year) vce(robust)
est store m5

reghdfe savings_k did $x0, absorb(id year) vce(robust)
est store m6

reghdfe igg did  $x0, absorb(id year) vce(robust)
est store m7

reghdfe GTFP_unexpected did  $x0, absorb(id year) vce(robust)
est store m8

esttab m5 m6 m7 m8 using result2.rtf,replace b(5) p(5) ar2(4) compress nogap star(* 0.1 ** 0.05 *** 0.01)

* Repeat for models with clustered standard errors
reghdfe income_rural_k did $x0, absorb(id year) cluster(id)
est store m9

reghdfe savings_k did $x0, absorb(id year) cluster(id)
est store m10

reghdfe igg did  $x0, absorb(id year) cluster(id)
est store m11

reghdfe GTFP_unexpected did  $x0, absorb(id year) cluster(id)
est store m12


esttab m9 m10 m11 m12 using result3.rtf,replace b(5) p(5) ar2(4) compress nogap star(* 0.1 ** 0.05 *** 0.01)



**************************************
*** Mechanism Analysis  ***
**************************************
* Use the finaldata.dta dataset
use finaldata.dta, clear

* Mechanism Tests - income_rural_k
// Direct effect
reghdfe income_rural_k did $x1, absorb(id year)
est store m1

local mcount = 2
// Mediating variable effects
foreach var in agriculture_power_k agriculture_output_k carbon_k {
    // Test the effect of did on the mediating variable
    reghdfe `var' did $x1 if income_rural_k != ., absorb(id year)
    est store m`mcount'
    local ++mcount

    // Test the combined effect of did and the mediating variable on income_rural_k
    reghdfe income_rural_k did `var' $x1, absorb(id year)
    est store m`mcount'
    local ++mcount
}

// Save results for income_rural_k mechanism tests
esttab m1 m2 m3 m4 m5 m6 m7 using "Mechanism_income_rural_k.rtf", replace

* Mechanism Tests - savings_k
// Direct effect
reghdfe savings_k did $x1, absorb(id year)
est store m4

local mcount = 5
// Mediating variable effects
foreach var in agriculture_power_k agriculture_output_k carbon_k {
    // Test the effect of did on the mediating variable
    reghdfe `var' did $x1 if savings_k != ., absorb(id year)
    est store m`mcount'
    local ++mcount

    // Test the combined effect of did and the mediating variable on savings_k
    reghdfe savings_k did `var' $x1, absorb(id year)
    est store m`mcount'
    local ++mcount
}

// Save results for savings_k mechanism tests
esttab m4 m5 m6 m7 m8 m9 using "Mechanism_savings_k.rtf", replace

* Mechanism Tests - GTFP_unexpected
// Direct effect
reghdfe GTFP_unexpected did $x1, absorb(id year)
est store m10

local mcount = 11
// Mediating variable effects
foreach var in agriculture_power_k agriculture_output_k carbon_k {
    // Test the effect of did on the mediating variable
    reghdfe `var' did $x1 if GTFP_unexpected != ., absorb(id year)
    est store m`mcount'
    local ++mcount

    // Test the combined effect of did and the mediating variable on GTFP_unexpected
    reghdfe GTFP_unexpected did `var' $x1, absorb(id year)
    est store m`mcount'
    local ++mcount
}

// Save results for GTFP_unexpected mechanism tests
esttab m10 m11 m12 m13 m14 m15 using "Mechanism_GTFP_unexpected.rtf", replace

* Mechanism Tests - igg
// Direct effect
reghdfe igg did $x1, absorb(id year)
est store m16

local mcount = 17
// Mediating variable effects
foreach var in agriculture_power_k agriculture_output_k carbon_k {
    // Test the effect of did on the mediating variable
    reghdfe `var' did $x1 if igg != ., absorb(id year)
    est store m`mcount'
    local ++mcount

    // Test the combined effect of did and the mediating variable on igg
    reghdfe igg did `var' $x1, absorb(id year)
    est store m`mcount'
    local ++mcount
}

// Save results for igg mechanism tests
esttab m16 m17 m18 m19 m20 m21 using "Mechanism_igg.rtf", replace


**************************************
*** PSM-DID  ***
**************************************
// income_rural_k

* Load the finaldata.dta dataset
use finaldata.dta, clear

* Set seed for reproducibility and generate a random order for the sample
set seed 12345
gen ranorder = runiform()
sort ranorder

* Perform propensity score matching for income_rural_k
psmatch2 treat $x1, outcome(income_rural_k) logit ate neighbor(1) common norepl cal(0.05)

* Test and graph the balance of matched samples for income_rural_k
pstest $x1, both graph

* Generate and save the graph of propensity score distribution for income_rural_k
psgraph
graph export "propensity_score_distribution_income_rural_k.png", replace

* Generate a common support variable and drop observations outside the common support for income_rural_k
gen common = _support
drop if common != 1

* Ordinary least squares regression without propensity score weights for income_rural_k
reghdfe income_rural_k treat $x1, absorb(id year)
est store ols_income

* Ordinary least squares regression with propensity score weights for income_rural_k
reghdfe income_rural_k treat $x1 [pw=_weight], absorb(id year)
est store psm_ols_income

* Export the regression results for income_rural_k
local m "ols_income psm_ols_income"
esttab `m' using "income_rural_k_results.rtf", mtitle(`m') compress nogap ar2 replace





// igg

* Load the finaldata.dta dataset
use finaldata.dta, clear

* Set seed for reproducibility and generate a random order for the sample
set seed 12345
gen ranorder = runiform()
sort ranorder

* Perform propensity score matching for igg
psmatch2 treat $x1, outcome(igg) logit ate neighbor(1) common norepl cal(0.05)

* Test and graph the balance of matched samples for igg
pstest $x1, both graph

* Generate and save the graph of propensity score distribution for igg
psgraph
graph export "propensity_score_distribution_igg.png", replace

* Generate a common support variable and drop observations outside the common support for igg
gen common = _support
drop if common != 1

* Ordinary least squares regression without propensity score weights for igg
reghdfe igg treat $x1, absorb(id year)
est store ols_igg

* Ordinary least squares regression with propensity score weights for igg
reghdfe igg treat $x1 [pw=_weight], absorb(id year)
est store psm_ols_igg

* Export the regression results for igg
local m "ols_igg psm_ols_igg"
esttab `m', mtitle(`m') compress nogap ar2

// GTFP_unexpected

* Load the finaldata.dta dataset
use finaldata.dta, clear

* Set seed for reproducibility and generate a random order for the sample
set seed 12345
gen ranorder = runiform()
sort ranorder

* Perform propensity score matching for GTFP_unexpected
psmatch2 treat $x1, outcome(GTFP_unexpected) logit ate neighbor(1) common norepl cal(0.05)

* Test and graph the balance of matched samples for GTFP_unexpected
pstest $x1, both graph

* Generate and save the graph of propensity score distribution for GTFP_unexpected
psgraph
graph export "propensity_score_distribution_GTFP_unexpected.png", replace

* Generate a common support variable and drop observations outside the common support for GTFP_unexpected
gen common = _support
drop if common != 1

* Ordinary least squares regression without propensity score weights for GTFP_unexpected
reghdfe GTFP_unexpected treat $x1, absorb(id year)
est store ols_GTFP

* Ordinary least squares regression with propensity score weights for GTFP_unexpected
reghdfe GTFP_unexpected treat $x1 [pw=_weight], absorb(id year)
est store psm_ols_GTFP

* Export the regression results for GTFP_unexpected
local m "ols_GTFP psm_ols_GTFP"
esttab `m', mtitle(`m') compress nogap ar2







**************************************
*** Mechanism Analysis  ***
**************************************
* Robustness Test 1

* 1) Reduce sample size (exclude provinces with fewer counties implementing the policy)
use finaldata.dta, clear
destring num_id_province, replace
sort num_id_province
by num_id_province: count if treat == 1
drop if inlist(num_id_province, 11, 12, 31, 32, 44, 45, 50, 52)
reghdfe income_rural_k did $x1, absorb(id num_id_province)
estadd local county "Yes"
estadd local province "Yes"
est sto m1

* 2) Reduce sample size (data winsorization/truncation)
use finaldata.dta, clear
winsor2 income_rural_k $x1, replace cuts(1 99) 
reghdfe income_rural_k did $x1, absorb(id num_id_province)
estadd local county "Yes"
estadd local province "Yes"
est sto m2

use finaldata.dta, clear
winsor2 income_rural_k $x1, replace cuts(1 99) trim
reghdfe income_rural_k did $x1, absorb(id num_id_province)
estadd local county "Yes"
estadd local province "Yes"
est sto m3

esttab m1 m2 m3 using robust_test_regout1.rtf, b(%12.3f) se(%12.3f) replace ///
s(county province N r2) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title("Robustness Test")



* Robustness Test 2
use finaldata.dta, clear

* Grid Connection Time (post_grid) Analysis
* Table 2
reghdfe income_rural post_grid $x0, absorb(id year)

reghdfe savings_k post_grid $x0, absorb(id year)

reghdfe igg post_grid  $x0, absorb(id year)

reghdfe GTFP_unexpected post_grid  $x0, absorb(id year)

* Table 1a
reghdfe income_rural post_grid $x0, absorb(id year) vce(robust)

reghdfe savings_k post_grid $x0, absorb(id year) vce(robust)

reghdfe igg post_grid $x0, absorb(id year) vce(robust)

reghdfe GTFP_unexpected post_grid  $x0, absorb(id year) vce(robust)

* Table 1b
reghdfe income_rural post_grid $x0, absorb(id year) cluster(id)

reghdfe savings_k post_grid $x0, absorb(id year) cluster(id)

reghdfe igg post_grid  $x0, absorb(id year) cluster(id)

reghdfe GTFP_unexpected post_grid  $x0, absorb(id year) cluster(id)


**************************************
*** Heterogeneity analysis  ***
**************************************

* Heterogeneity 1 - Power Plants

* Household-Level Power Plants
use finaldata.dta, clear
set seed 186877
gen randomorder=runiform()
sort randomorder

gen ptype_a1=0
replace ptype_a1=1 if household==1 
bys id: egen ptype_a=max(ptype_a1)
drop if village==1 | liancun==1 | centralized==1
psmatch2 ptype_a $x0, outcome(income_rural savings_k igg GTFP_unexpected) logit ate n(1) common norepl cal(0.05) 
* pstest $x0, both graph
gen common=_support

reghdfe income_rural did $x0 if _weight!=0, absorb(id year) 

reghdfe savings_k did $x0 if _weight!=0, absorb(id year) 

reghdfe igg did $x0 if _weight!=0, absorb(id year) 

reghdfe GTFP_unexpected did $x0 if _weight!=0, absorb(id year) 

drop ptype_a1 ptype_a _pscore _treated _support _weight _income_rural _id _n1 _nn common _pdif


* Village-Level Power Plants
use finaldata.dta, clear
set seed 186877
gen randomorder=runiform()
sort randomorder

gen ptype_a1=0
replace ptype_a1=1 if village==1 
bys id: egen ptype_a=max(ptype_a1)
drop if household==1 | liancun==1 | centralized==1
psmatch2 ptype_a $x0, outcome(income_rural savings_k igg GTFP_unexpected) logit ate n(1) common norepl cal(0.05) 
* pstest $x0, both graph
gen common=_support

reghdfe income_rural did $x0 if _weight!=0, absorb(id year) 

reghdfe savings_k did $x0 if _weight!=0, absorb(id year) 

reghdfe igg did $x0 if _weight!=0, absorb(id year) 

reghdfe GTFP_unexpected did $x0 if _weight!=0, absorb(id year) 

drop ptype_a1 ptype_a _pscore _treated _support _weight _income_rural _id _n1 _nn common _pdif


* Inter-Village Power Plants
use finaldata.dta, clear
set seed 186877
gen randomorder=runiform()
sort randomorder

gen ptype_a1=0
replace ptype_a1=1 if liancun==1 
drop if household==1 | village==1 | centralized==1
bys id: egen ptype_a=max(ptype_a1)
psmatch2 ptype_a $x0, outcome(income_rural savings_k igg GTFP_unexpected) logit ate n(1) common norepl cal(0.05) 
* pstest $x0, both graph
gen common=_support

reghdfe income_rural did $x0 if _weight!=0, absorb(id year) 

reghdfe savings_k did $x0 if _weight!=0, absorb(id year) 

reghdfe igg did $x0 if _weight!=0, absorb(id year) 

reghdfe GTFP_unexpected did $x0 if _weight!=0, absorb(id year) 

drop ptype_a1 ptype_a _pscore _treated _support _weight _income_rural _id _n1 _nn common _pdif


* Centralized Power Plants
use finaldata.dta, clear
set seed 186877
gen randomorder=runiform()
sort randomorder

gen ptype_a1=0
replace ptype_a1=1 if centralized==1 
drop if household==1 | village==1 | liancun==1
bys id: egen ptype_a=max(ptype_a1)
psmatch2 ptype_a $x0, outcome(income_rural) logit ate n(1) common norepl cal(0.05) 
* pstest $x0, both graph
gen common=_support

reghdfe income_rural did $x0 if _weight!=0, absorb(id year) 

reghdfe savings_k did $x0 if _weight!=0, absorb(id year) 

reghdfe igg did $x0 if _weight!=0, absorb(id year) 

reghdfe GTFP_unexpected did $x0 if _weight!=0, absorb(id year) 

drop ptype_a1 ptype_a _pscore _treated _support _weight _income_rural _id _n1 _nn common _pdif



* Heterogeneity 2 - Three Categories

use finaldata.dta, clear

* Solar Resource Endowment 


reghdfe income_rural_k did $x0 if (endowment==1), absorb(id year)
estadd local Endowment "Low"
estadd local Year "YES"
estadd local County "YES"
est sto m1

reghdfe income_rural_k did $x0 if (endowment==2), absorb(id year)
estadd local Endowment "Medium"
estadd local Year "YES"
estadd local County "YES"
est sto m2

reghdfe income_rural_k did $x0 if (endowment==3), absorb(id year)
estadd local Endowment "High"
estadd local Year "YES"
estadd local County "YES"
est sto m3


reghdfe savings_k did $x0 if (endowment==1), absorb(id year)
estadd local Endowment "Low"
estadd local Year "YES"
estadd local County "YES"
est sto m4

reghdfe savings_k did $x0 if (endowment==2), absorb(id year)
estadd local Endowment "Medium"
estadd local Year "YES"
estadd local County "YES"
est sto m5

reghdfe savings_k did $x0 if (endowment==3), absorb(id year)
estadd local Endowment "High"
estadd local Year "YES"
estadd local County "YES"
est sto m6


esttab m1 m2 m3 m4 m5 m6 using heter_endowment_regout1.rtf , b(%12.3f) se(%12.3f) ///
replace s(Endowment Year County) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title(analysis of endowment heterogeneity)





use finaldata.dta, clear




reghdfe igg did $x0 if (endowment==1), absorb(id year)
estadd local Endowment "Low"
estadd local Year "YES"
estadd local County "YES"
est sto m7

reghdfe igg did $x0 if (endowment==2), absorb(id year)
estadd local Endowment "Medium"
estadd local Year "YES"
estadd local County "YES"
est sto m8

reghdfe igg did $x0 if (endowment==3), absorb(id year)
estadd local Endowment "High"
estadd local Year "YES"
estadd local County "YES"
est sto m9


reghdfe GTFP_unexpected did $x0 if (endowment==1), absorb(id year)
estadd local Endowment "Low"
estadd local Year "YES"
estadd local County "YES"
est sto m10

reghdfe GTFP_unexpected did $x0 if (endowment==2), absorb(id year)
estadd local Endowment "Medium"
estadd local Year "YES"
estadd local County "YES"
est sto m11

reghdfe GTFP_unexpected did $x0 if (endowment==3), absorb(id year)
estadd local Endowment "High"
estadd local Year "YES"
estadd local County "YES"
est sto m12


esttab m7 m8 m9 m10 m11 m12 using heter_endowment_regout2.rtf , b(%12.3f) se(%12.3f) ///
replace s(Endowment Year County) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title(analysis of endowment heterogeneity)


* Heterogeneity 3 - Three Categories
* Geographic Location


use finaldata.dta, clear




reghdfe income_rural_k did $x0 if (region==1), absorb(id year)
estadd local Region "Eastern"
estadd local Year "YES"
estadd local County "YES"
est sto m1

reghdfe income_rural_k did $x0 if (region==2), absorb(id year)
estadd local Region "Central"
estadd local Year "YES"
estadd local County "YES"
est sto m2

reghdfe income_rural_k did $x0 if (region==3), absorb(id year)
estadd local Region "Western"
estadd local Year "YES"
estadd local County "YES"
est sto m3


reghdfe savings_k did $x0 if (region==1), absorb(id year)
estadd local Region "Eastern"
estadd local Year "YES"
estadd local County "YES"
est sto m4

reghdfe savings_k did $x0 if (region==2), absorb(id year)
estadd local Region "Central"
estadd local Year "YES"
estadd local County "YES"
est sto m5

reghdfe savings_k did $x0 if (region==3), absorb(id year)
estadd local Region "Western"
estadd local Year "YES"
estadd local County "YES"
est sto m6


esttab m1 m2 m3 m4 m5 m6 using heter_region_regout1.rtf , b(%12.3f) se(%12.3f) ///
replace s(Region Year County) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title(analysis of endowment heterogeneity)


reghdfe igg did $x0 if (region==1), absorb(id year)
estadd local Region "Eastern"
estadd local Year "YES"
estadd local County "YES"
est sto m7

reghdfe igg did $x0 if (region==2), absorb(id year)
estadd local Region "Central"
estadd local Year "YES"
estadd local County "YES"
est sto m8

reghdfe igg did $x0 if (region==3), absorb(id year)
estadd local Region "Western"
estadd local Year "YES"
estadd local County "YES"
est sto m9


reghdfe GTFP_unexpected did $x0 if (region==1), absorb(id year)
estadd local Region "Eastern"
estadd local Year "YES"
estadd local County "YES"
est sto m10

reghdfe GTFP_unexpected did $x0 if (region==2), absorb(id year)
estadd local Region "Central"
estadd local Year "YES"
estadd local County "YES"
est sto m11

reghdfe GTFP_unexpected did $x0 if (region==3), absorb(id year)
estadd local Region "Western"
estadd local Year "YES"
estadd local County "YES"
est sto m12


esttab m7 m8 m9 m10 m11 m12  using region_region_regout2.rtf , b(%12.3f) se(%12.3f) ///
replace s(Region Year County) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title(analysis of endowment heterogeneity)






reghdfe igg did $x0 if (region==1), absorb(id year)
estadd local Region "Eastern"
estadd local Year "YES"
estadd local County "YES"
est sto m7

reghdfe igg did $x0 if (region==2), absorb(id year)
estadd local Region "Central"
estadd local Year "YES"
estadd local County "YES"
est sto m8

reghdfe igg did $x0 if (region==3), absorb(id year)
estadd local Region "Western"
estadd local Year "YES"
estadd local County "YES"
est sto m9


reghdfe GTFP_unexpected did $x0 if (region==1), absorb(id year)
estadd local Region "Eastern"
estadd local Year "YES"
estadd local County "YES"
est sto m10

reghdfe GTFP_unexpected did $x0 if (region==2), absorb(id year)
estadd local Region "Central"
estadd local Year "YES"
estadd local County "YES"
est sto m11

reghdfe GTFP_unexpected did $x0 if (region==3), absorb(id year)
estadd local Region "Western"
estadd local Year "YES"
estadd local County "YES"
est sto m12


esttab m7 m8 m9 m10 m11 m12  using region_region_regout2.rtf , b(%12.3f) se(%12.3f) ///
replace s(Region Year County) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title(analysis of endowment heterogeneity)




* Poverty
use finaldata.dta, clear
reghdfe income_rural_k did $x0 if (poverty==1), absorb(id year)
estadd local Poverty "Yes"
estadd local Year "Yes"
estadd local County "Yes"
est sto m1

reghdfe income_rural_k did $x0 if (poverty!=1), absorb(id year)
estadd local Poverty "No"
estadd local Year "Yes"
estadd local County "Yes"
est sto m2

reghdfe savings_k did $x0 if (poverty==1), absorb(id year)
estadd local Poverty "Yes"
estadd local Year "Yes"
estadd local County "Yes"
est sto m3

reghdfe savings_k did $x0 if (poverty!=1), absorb(id year)
estadd local Poverty "No"
estadd local Year "Yes"
estadd local County "Yes"
est sto m4

esttab m1 m2 m3 m4 using poverty_poverty_regout1.rtf, b(%12.3f) se(%12.3f) ///
replace s(Poverty Year County) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title("Analysis of Poverty Heterogeneity")

use finaldata.dta, clear
reghdfe igg did $x0 if (poverty==1), absorb(id year)
estadd local Poverty "Yes"
estadd local Year "Yes"
estadd local County "Yes"
est sto m7

reghdfe igg did $x0 if (poverty!=1), absorb(id year)
estadd local Poverty "No"
estadd local Year "Yes"
estadd local County "Yes"
est sto m8

reghdfe GTFP_unexpected did $x0 if (poverty==1), absorb(id year)
estadd local Poverty "Yes"
estadd local Year "Yes"
estadd local County "Yes"
est sto m10

reghdfe GTFP_unexpected did $x0 if (poverty!=1), absorb(id year)
estadd local Poverty "No"
estadd local Year "Yes"
estadd local County "Yes"
est sto m11

esttab m7 m8 m10 m11 using poverty_poverty_regout2.rtf, b(%12.3f) se(%12.3f) ///
replace s(Poverty Year County) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title("Analysis of Poverty Heterogeneity")



*Agricultural Production Level
use finaldata.dta, clear
xtile agriculture =agriculture_output_k,nq(3)
reghdfe income_rural_k did $x0 if (agriculture==1), absorb(id year)
estadd local Level "Low"
estadd local Year "YES"
estadd local County "YES"
est sto m1

reghdfe income_rural_k did $x0 if (agriculture==2), absorb(id year)
estadd local Level "Medium"
estadd local Year "YES"
estadd local County "YES"
est sto m2

reghdfe income_rural_k did $x0 if (agriculture==3), absorb(id year)
estadd local Level "High"
estadd local Year "YES"
estadd local County "YES"
est sto m3


reghdfe savings_k did $x0 if (agriculture==1), absorb(id year)
estadd local Level "Low"
estadd local Year "YES"
estadd local County "YES"
est sto m4

reghdfe savings_k did $x0 if (agriculture==2), absorb(id year)
estadd local Level "Medium"
estadd local Year "YES"
estadd local County "YES"
est sto m5

reghdfe savings_k did $x0 if (agriculture==3), absorb(id year)
estadd local Level "High"
estadd local Year "YES"
estadd local County "YES"
est sto m6


esttab m1 m2 m3 m4 m5 m6 using agriculture_endowment_regout1.rtf , b(%12.3f) se(%12.3f) ///
replace s(Level Year County) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title(analysis of Agricultural Production Level)

reghdfe igg did $x0 if (agriculture==1), absorb(id year)
estadd local Level "Low"
estadd local Year "YES"
estadd local County "YES"
est sto m7

reghdfe igg did $x0 if (agriculture==2), absorb(id year)
estadd local Level "Medium"
estadd local Year "YES"
estadd local County "YES"
est sto m8

reghdfe igg did $x0 if (agriculture==3), absorb(id year)
estadd local Level "High"
estadd local Year "YES"
estadd local County "YES"
est sto m9


reghdfe GTFP_unexpected did $x0 if (agriculture==1), absorb(id year)
estadd local Level "Low"
estadd local Year "YES"
estadd local County "YES"
est sto m10

reghdfe GTFP_unexpected did $x0 if (agriculture==2), absorb(id year)
estadd local Level "Medium"
estadd local Year "YES"
estadd local County "YES"
est sto m11

reghdfe GTFP_unexpected did $x0 if (agriculture==3), absorb(id year)
estadd local Level "High"
estadd local Year "YES"
estadd local County "YES"
est sto m12


esttab m7 m8 m9 m10 m11 m12 using agriculture_endowment_regout2.rtf , b(%12.3f) se(%12.3f) ///
replace s(Level Year County) star(* 0.1 ** 0.05 *** 0.01) ///
order(did $xr) onecell nogaps title(Agricultural Production Level)



* Placebo Tests - income_rural_k
clear
forvalue i=1/1000 {
    use "finaldata.dta", replace
    g obs_id1 = _n 
    gen random_digit = runiform()  
    sort random_digit 
    g random_id1 = _n   
    preserve
    keep random_id1 treat
    rename treat random_treat
    rename random_id1 id1 
    label var id1 
    save "random_treat.dta", replace
    restore
    drop random_digit random_id1 treat
    rename obs_id1 id1  
    label var id1  
    save "newdata.dta", replace

    use "newdata.dta", clear
    merge 1:1 id1 using "random_treat", nogen
    xtset id year
    reghdfe income_rural_k random_treat $x0, absorb(id year)
    g _b_random_treat = _b[random_treat]  
    g _se_random_treat = _se[random_treat]  
    keep _b_random_treat _se_random_treat
    duplicates drop _b_random_treat, force
    save "placebo`i'.dta", replace   
}

use "placebo1.dta", clear
forvalue i=2/1000 {
    append using "placebo`i'.dta"   
}

gen tvalue = _b_random_treat / _se_random_treat
kdensity tvalue, xtitle("t value") ytitle("distribution") saving(placebo_test) 

forvalue i=1/1000 {
    erase  "placebo`i'.dta"
} 

erase "newdata.dta"
erase "random_treat.dta"

* Placebo Tests - savings_k
clear
forvalue i=1/1000 {
    use "finaldata.dta", replace
    g obs_id1 = _n 
    gen random_digit = runiform()  
    sort random_digit 
    g random_id1 = _n   
    preserve
    keep random_id1 treat
    rename treat random_treat
    rename random_id1 id1 
    label var id1 
    save "random_treat.dta", replace
    restore
    drop random_digit random_id1 treat
    rename obs_id1 id1  
    label var id1  
    save "newdata.dta", replace

    use "newdata.dta", clear
    merge 1:1 id1 using "random_treat", nogen
    xtset id year
    reghdfe savings_k random_treat $x0, absorb(id year)
    g _b_random_treat = _b[random_treat]  
    g _se_random_treat = _se[random_treat]  
    keep _b_random_treat _se_random_treat
    duplicates drop _b_random_treat, force
    save "placebo`i'.dta", replace   
}

use "placebo1.dta", clear
forvalue i=2/1000 {
    append using "placebo`i'.dta"   
}

gen tvalue = _b_random_treat / _se_random_treat
kdensity tvalue, xtitle("t value") ytitle("distribution") saving(placebo_test_savings) 

forvalue i=1/1000 {
    erase  "placebo`i'.dta"
} 

erase "newdata.dta"
erase "random_treat.dta"

* Placebo Tests - igg
clear
forvalue i=1/1000 {
    use "finaldata.dta", replace
    g obs_id1 = _n 
    gen random_digit = runiform()  
    sort random_digit 
    g random_id1 = _n   
    preserve
    keep random_id1 treat
    rename treat random_treat
    rename random_id1 id1 
    label var id1 
    save "random_treat.dta", replace
    restore
    drop random_digit random_id1 treat
    rename obs_id1 id1  
    label var id1  
    save "newdata.dta", replace

    use "newdata.dta", clear
    merge 1:1 id1 using "random_treat", nogen
    xtset id year
    reghdfe igg random_treat $x0, absorb(id year)
    g _b_random_treat = _b[random_treat]  
    g _se_random_treat = _se[random_treat]  
    keep _b_random_treat _se_random_treat
    duplicates drop _b_random_treat, force
    save "placebo`i'.dta", replace   
}

use "placebo1.dta", clear
forvalue i=2/1000 {
    append using "placebo`i'.dta"   
}

gen tvalue = _b_random_treat / _se_random_treat
kdensity tvalue, xtitle("t value") ytitle("distribution") saving(placebo_test_igg) 

forvalue i=1/1000 {
    erase  "placebo`i'.dta"
} 

erase "newdata.dta"
erase "random_treat.dta"

* Placebo Tests - GTFP_unexpected
clear
forvalue i=1/1000 {
    use "finaldata.dta", replace
    g obs_id1 = _n 
    gen random_digit = runiform()  
    sort random_digit 
    g random_id1 = _n   
    preserve
    keep random_id1 treat
    rename treat random_treat
    rename random_id1 id1 
    label var id1 
    save "random_treat.dta", replace
    restore
    drop random_digit random_id1 treat
    rename obs_id1 id1  
    label var id1  
    save "newdata.dta", replace

    use "newdata.dta", clear
    merge 1:1 id1 using "random_treat", nogen
    xtset id year
    reghdfe GTFP_unexpected random_treat $x0, absorb(id year)
    g _b_random_treat = _b[random_treat]  
    g _se_random_treat = _se[random_treat]  
    keep _b_random_treat _se_random_treat
    duplicates drop _b_random_treat, force
    save "placebo`i'.dta", replace   
}

use "placebo1.dta", clear
forvalue i=2/1000 {
    append using "placebo`i'.dta"   
}

gen tvalue = _b_random_treat / _se_random_treat
kdensity tvalue, xtitle("t value") ytitle("distribution") saving(placebo_test_gtfp) 

forvalue i=1/1000 {
    erase  "placebo`i'.dta"
} 

erase "newdata.dta"
erase "random_treat.dta"


**************************************
*** Parallel Trend Test  ***
**************************************

//income_rural
use finaldata.dta, clear
gen pre20=0
replace pre20=1 if year+20==begintime
gen pre19=0
replace pre19=1 if year+19==begintime
gen pre18=0
replace pre18=1 if year+18==begintime
gen pre17=0
replace pre17=1 if year+17==begintime
gen pre16=0
replace pre16=1 if year+16==begintime
gen pre15=0
replace pre15=1 if year+15==begintime
gen pre14=0
replace pre14=1 if year+14==begintime
gen pre13=0
replace pre13=1 if year+13==begintime
gen pre12=0
replace pre12=1 if year+12==begintime
gen pre11=0
replace pre11=1 if year+11==begintime
gen pre10=0
replace pre10=1 if year+10==begintime
gen pre9=0
replace pre9=1 if year+9==begintime
gen pre8=0
replace pre8=1 if year+8==begintime
gen pre7=0
replace pre7=1 if year+7==begintime
gen pre6=0
replace pre6=1 if year+6==begintime
gen pre5=0
replace pre5=1 if year+5==begintime
gen pre4=0
replace pre4=1 if year+4==begintime
gen pre3=0
replace pre3=1 if year+3==begintime
gen pre2=0
replace pre2=1 if year+2==begintime
gen pre1=0
replace pre1=1 if year+1==begintime
gen current=0
replace current=1 if year==begintime
gen post1=0
replace post1=1 if year-1==begintime
gen post2=0
replace post2=1 if year-2==begintime
gen post3=0
replace post3=1 if year-3==begintime
gen post4=0
replace post4=1 if year-4==begintime
gen post5=0
replace post5=1 if year-5==begintime
gen post6=0
replace post6=1 if year-6==begintime
gen post7=0
replace post7=1 if year-7==begintime
gen post8=0
replace post8=1 if year-8==begintime
gen post9=0
replace post9=1 if year-9==begintime
gen post10=0
replace post10=1 if year-10==begintime
gen post11=0
replace post11=1 if year-11==begintime

xtset id year
winsor2 income_rural_k, cut(10 99) s(1)
reghdfe income_rural_k1 $x0 pre13 pre12 pre11 pre10 pre9 pre8 pre7 pre6 pre5 pre4 pre3 pre2 pre1 current post1 post2 post3 post4 post5  , absorb(id year)

est sto reg

coefplot reg,keep(pre* current post*) vertical yline(0,lwidth(vthin) lpattern(dash) lcolor(red))yline(5,lwidth(vthin) lpattern(solid) lcolor(gs0%13)) xline(14,lwidth(vthin) lpattern(dash) lcolor(blue)) ylabel(,labsize(*0.75) format(%5.1f) angle(0) ) xlabel(,labsize(*0.55)) ytitle("Coefficients") xtitle(" ") msymbol(O) msize(small) mcolor(gs0) addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ciopts(lpattern(dash) recast(rcap) msize(medium) lcolor(gs0) graphregion(color(white))) plotregion(style(none)) title("Parallel Trend - Rural Income", size(*0.85)) 



savings_k 
use finaldata.dta, clear
gen pre20=0
replace pre20=1 if year+20==begintime
gen pre19=0
replace pre19=1 if year+19==begintime
gen pre18=0
replace pre18=1 if year+18==begintime
gen pre17=0
replace pre17=1 if year+17==begintime
gen pre16=0
replace pre16=1 if year+16==begintime
gen pre15=0
replace pre15=1 if year+15==begintime
gen pre14=0
replace pre14=1 if year+14==begintime
gen pre13=0
replace pre13=1 if year+13==begintime
gen pre12=0
replace pre12=1 if year+12==begintime
gen pre11=0
replace pre11=1 if year+11==begintime
gen pre10=0
replace pre10=1 if year+10==begintime
gen pre9=0
replace pre9=1 if year+9==begintime
gen pre8=0
replace pre8=1 if year+8==begintime
gen pre7=0
replace pre7=1 if year+7==begintime
gen pre6=0
replace pre6=1 if year+6==begintime
gen pre5=0
replace pre5=1 if year+5==begintime
gen pre4=0
replace pre4=1 if year+4==begintime
gen pre3=0
replace pre3=1 if year+3==begintime
gen pre2=0
replace pre2=1 if year+2==begintime
gen pre1=0
replace pre1=1 if year+1==begintime
gen current=0
replace current=1 if year==begintime
gen post1=0
replace post1=1 if year-1==begintime
gen post2=0
replace post2=1 if year-2==begintime
gen post3=0
replace post3=1 if year-3==begintime
gen post4=0
replace post4=1 if year-4==begintime
gen post5=0
replace post5=1 if year-5==begintime
gen post6=0
replace post6=1 if year-6==begintime
gen post7=0
replace post7=1 if year-7==begintime
gen post8=0
replace post8=1 if year-8==begintime
gen post9=0
replace post9=1 if year-9==begintime
gen post10=0
replace post10=1 if year-10==begintime
gen post11=0
replace post11=1 if year-11==begintime

winsor2 savings_k, cut(75 90) s(1)
xtset id year
reghdfe savings_k1 $x0 pre13 pre12 pre11 pre10 pre9 pre8 pre7 pre6 pre5 pre4 pre3 pre2 pre1 current post1 post2 post3 post4  , absorb(id year)

est sto reg

coefplot reg,keep(pre* current post*) vertical yline(0,lwidth(vthin) lpattern(dash) lcolor(red))yline(5,lwidth(vthin) lpattern(solid) lcolor(gs0%13)) xline(14,lwidth(vthin) lpattern(dash) lcolor(blue)) ylabel(,labsize(*0.75) format(%5.1f) angle(0) ) xlabel(,labsize(*0.55)) ytitle("Coefficients") xtitle(" ") msymbol(O) msize(small) mcolor(gs0) addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ciopts(lpattern(dash) recast(rcap) msize(medium) lcolor(gs0) graphregion(color(white))) plotregion(style(none)) title("Parallel Trend - Savings", size(*0.85)) 


//GTFP_unexpected
use finaldata.dta, clear
gen pre20=0
replace pre20=1 if year+20==begintime
gen pre19=0
replace pre19=1 if year+19==begintime
gen pre18=0
replace pre18=1 if year+18==begintime
gen pre17=0
replace pre17=1 if year+17==begintime
gen pre16=0
replace pre16=1 if year+16==begintime
gen pre15=0
replace pre15=1 if year+15==begintime
gen pre14=0
replace pre14=1 if year+14==begintime
gen pre13=0
replace pre13=1 if year+13==begintime
gen pre12=0
replace pre12=1 if year+12==begintime
gen pre11=0
replace pre11=1 if year+11==begintime
gen pre10=0
replace pre10=1 if year+10==begintime
gen pre9=0
replace pre9=1 if year+9==begintime
gen pre8=0
replace pre8=1 if year+8==begintime
gen pre7=0
replace pre7=1 if year+7==begintime
gen pre6=0
replace pre6=1 if year+6==begintime
gen pre5=0
replace pre5=1 if year+5==begintime
gen pre4=0
replace pre4=1 if year+4==begintime
gen pre3=0
replace pre3=1 if year+3==begintime
gen pre2=0
replace pre2=1 if year+2==begintime
gen pre1=0
replace pre1=1 if year+1==begintime
gen current=0
replace current=1 if year==begintime
gen post1=0
replace post1=1 if year-1==begintime
gen post2=0
replace post2=1 if year-2==begintime
gen post3=0
replace post3=1 if year-3==begintime
gen post4=0
replace post4=1 if year-4==begintime
gen post5=0
replace post5=1 if year-5==begintime
gen post6=0
replace post6=1 if year-6==begintime
gen post7=0
replace post7=1 if year-7==begintime
gen post8=0
replace post8=1 if year-8==begintime
gen post9=0
replace post9=1 if year-9==begintime
gen post10=0
replace post10=1 if year-10==begintime
gen post11=0
replace post11=1 if year-11==begintime

xtset id year
winsor2 GTFP_unexpected, cut(20 100) s(1)
reghdfe GTFP_unexpected1 $x0 pre13 pre12 pre11 pre10 pre9 pre8 pre7 pre6 pre5 pre4 pre3 pre2 pre1 current post1 post2 post3 post4 post5 post6 , absorb(id year)

est sto reg

coefplot reg,keep(pre* current post*) vertical yline(0,lwidth(vthin) lpattern(dash) lcolor(red))yline(5,lwidth(vthin) lpattern(solid) lcolor(gs0%13)) xline(14,lwidth(vthin) lpattern(dash) lcolor(blue)) ylabel(,labsize(*0.75) format(%5.1f) angle(0) ) xlabel(,labsize(*0.55)) ytitle("Coefficients") xtitle(" ") msymbol(O) msize(small) mcolor(gs0) addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ciopts(lpattern(dash) recast(rcap) msize(medium) lcolor(gs0) graphregion(color(white))) plotregion(style(none)) title("Parallel Trend - Green Total Factor Productivity", size(*0.85)) 


//igg 
use finaldata.dta, clear
gen pre20=0
replace pre20=1 if year+20==begintime
gen pre19=0
replace pre19=1 if year+19==begintime
gen pre18=0
replace pre18=1 if year+18==begintime
gen pre17=0
replace pre17=1 if year+17==begintime
gen pre16=0
replace pre16=1 if year+16==begintime
gen pre15=0
replace pre15=1 if year+15==begintime
gen pre14=0
replace pre14=1 if year+14==begintime
gen pre13=0
replace pre13=1 if year+13==begintime
gen pre12=0
replace pre12=1 if year+12==begintime
gen pre11=0
replace pre11=1 if year+11==begintime
gen pre10=0
replace pre10=1 if year+10==begintime
gen pre9=0
replace pre9=1 if year+9==begintime
gen pre8=0
replace pre8=1 if year+8==begintime
gen pre7=0
replace pre7=1 if year+7==begintime
gen pre6=0
replace pre6=1 if year+6==begintime
gen pre5=0
replace pre5=1 if year+5==begintime
gen pre4=0
replace pre4=1 if year+4==begintime
gen pre3=0
replace pre3=1 if year+3==begintime
gen pre2=0
replace pre2=1 if year+2==begintime
gen pre1=0
replace pre1=1 if year+1==begintime
gen current=0
replace current=1 if year==begintime
gen post1=0
replace post1=1 if year-1==begintime
gen post2=0
replace post2=1 if year-2==begintime
gen post3=0
replace post3=1 if year-3==begintime
gen post4=0
replace post4=1 if year-4==begintime
gen post5=0
replace post5=1 if year-5==begintime
gen post6=0
replace post6=1 if year-6==begintime
gen post7=0
replace post7=1 if year-7==begintime
gen post8=0
replace post8=1 if year-8==begintime
gen post9=0
replace post9=1 if year-9==begintime
gen post10=0
replace post10=1 if year-10==begintime
gen post11=0
replace post11=1 if year-11==begintime

xtset id year
winsor2 igg, cut(0 98) s(1)
reghdfe igg1 expenditure_k pre13 pre12 pre11 pre10 pre9 pre8 pre7 pre6 pre5 pre4 pre3 pre2 pre1 current post1 post2 post3 post4 post5 post6 , absorb(id year)

est sto reg

coefplot reg,keep(pre* current post*) vertical yline(0,lwidth(vthin) lpattern(dash) lcolor(red))yline(5,lwidth(vthin) lpattern(solid) lcolor(gs0%13)) xline(14,lwidth(vthin) lpattern(dash) lcolor(blue)) ylabel(,labsize(*0.75) format(%5.1f) angle(0) ) xlabel(,labsize(*0.55)) ytitle("Coefficients") xtitle(" ") msymbol(O) msize(small) mcolor(gs0) addplot(line @b @at,lcolor(gs1) lwidth(medthick)) ciopts(lpattern(dash) recast(rcap) msize(medium) lcolor(gs0) graphregion(color(white))) plotregion(style(none)) title("Parallel Trend - Inclusive Green Growth", size(*0.85)) 



