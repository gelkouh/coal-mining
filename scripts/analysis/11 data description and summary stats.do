cls
clear all
capture log close _all
set more off
set type double, perm
macro drop _all
clear frames
version 17
graph set window fontface "Times New Roman"
graph set ps fontface "Times New Roman"

gl data_path "C:\Users\celkouh\Downloads\data"
gl input "${data_path}\Output"
gl output_table "..\..\output\tables"
gl output_figure "..\..\output\figures"

**# Summary statistics
use "${input}\03 mine-quarter panel, union safety analysis.dta", clear 

isid MINE_ID year_quarter
gen byte mine_counter = 1
gegen mines_in_district = sum(mine_counter), by(district year_quarter)
gegen mines_in_msha_office = sum(mine_counter), by(msha_office_code year_quarter)

gegen seam_height_in_max = max(seam_height_in)
replace seam_height_in = . if inlist(seam_height_in, 0, seam_height_in_max) // don't use implausibly large values or 0 coal seam heights

local vlist year quarter union mine_age size_100FTEs controller_size_100FTEs coal_production_tons labor_hours productivity penalty_points_prev4qtrs total_injuries traumatic_injuries violations ss_violations mines_in_msha_office mines_in_district seam_height_in

lab var year "Year"
lab var quarter "Quarter"
lab var union "Union"
lab var size_100FTEs "Mine Size (100 FTEs)"
lab var controller_size_100FTEs "Controller Size (100 FTEs)"
lab var mine_age "Mine Age"
lab var coal_production_tons "Coal Production (short tons)"
lab var labor_hours "Underground Labor Hours"
lab var productivity "Productivity (tons/2,000 hrs)"
lab var penalty_points_prev4qtrs "Penalty Pts Last 4 Qtrs"
lab var total_injuries "Total Injury Count"
lab var traumatic_injuries "Traumatic Injury Count"
lab var violations "Total MSHA Violation Count" 
lab var ss_violations "S&S MSHA Violation Count"
lab var mines_in_district "Mines in MSHA District in Qtr"
lab var mines_in_msha_office "Mines with Same MSHA Office in Qtr"
lab var seam_height_in "Coal Seam Height (inches)"

table (var) (result), ///
	stat(count `vlist') ///
    stat(mean `vlist') ///
    stat(sd `vlist') ///
    stat(min `vlist') ///
    stat(max `vlist')

collect label levels result count "N" mean "Mean" sd "Std. Dev." min "Minimum" max "Maximum", modify
collect style cell result[mean sd min max], nformat(%18.2fc)

collect preview 

collect export "${output_table}\summary_stat_tables\summ_stats_embed.tex", replace tableonly // need to manually begin with { \begin{tabular} ...

**# Union safety trend, non-union safety trend, 90 pct trend, composition trend (MSHA, EIA)
preserve 
	keep if union == 1
	collapse (sum) traumatic_injuries_union=traumatic_injuries labor_hours_union=labor_hours, by(year_quarter)
	
	tempfile union_safety_temp
	save `union_safety_temp', replace
restore
preserve 
	keep if union == 0
	collapse (sum) traumatic_injuries_nonunion=traumatic_injuries labor_hours_nonunion=labor_hours, by(year_quarter)
	
	tempfile nonunion_safety_temp
	save `nonunion_safety_temp', replace
restore
preserve 
	keep if union == 0 & MINE_ID_active_90pct == 1
	collapse (sum) traumatic_injuries_long=traumatic_injuries labor_hours_long=labor_hours, by(year_quarter)
	
	tempfile safety_temp
	save `safety_temp', replace
restore

collapse (sum) MINE_ID_active_90pct mine_counter, by(year_quarter)

gen share_active_90pct = MINE_ID_active_90pct / mine_counter
gen byte mine_life_series = 1

merge 1:1 year_quarter using `union_safety_temp', assert(3) nogen
merge 1:1 year_quarter using `nonunion_safety_temp', assert(3) nogen
merge 1:1 year_quarter using `safety_temp', assert(3) nogen

gen traumatic_injury_rate_diff_all = 2000*(traumatic_injuries_nonunion/labor_hours_nonunion) - 2000*(traumatic_injuries_union/labor_hours_union)
gen traumatic_injury_rate_diff_long = 2000*(traumatic_injuries_long/labor_hours_long) - 2000*(traumatic_injuries_union/labor_hours_union)

#d ;
graph twoway 
	lpoly traumatic_injury_rate_diff_all year_quarter, msize(vsmall) bwidth(0.25) ||
	lpoly share_active_90pct year_quarter, msize(vsmall) yaxis(2) lpattern(".-") bwidth(0.25) ||
	lpoly traumatic_injury_rate_diff_long year_quarter, msize(vsmall) lpattern("-") bwidth(0.25)
		
	yline(0, lcolor(black) lwidth(vthin))
	xlabel(2000 2005 2010 2015 2022)
	ylabel(, angle(0) glwidth(vthin) glcolor(gs14) format(%8.0gc))
	ysc(titlegap(5))
	ylabel(0 "0%" .05 "5%" .10 "10%" .15 "15%" .2 "20%", axis(2) angle(0) glwidth(vthin) glcolor(gs14) format(%8.0gc))
	legend(rows(2) region(lstyle(none)) size(small) symxsize(medium) order(1 "Union Safety Premium: All Mines" 3 "Share of Mines in {&ge} 90 Pct Qtrs 2000-2022" 2 "Union Safety Premium: Nonunion Mines in {&ge} 90 Pct Qtrs 2000-2022"))

	ytitle("Nonunion - Union Injuries per 2,000 Hours Worked")
	ytitle("Share of Mines Active in {&ge} 90 Pct Qtrs 2000-2022", axis(2))
	xtitle("")
	
	note(
	"Notes: Trends smoothed with an Epanechnikov kernel function with kernel bandwidth of 0.25 (equivalent to 1 quarter)."
	"Sources: EIA, MSHA."
	, size(vsmall) span)
	
	graphregion(color(white))
;
#d cr
graph export "${output_figure}\summary_stat_figures\union_premium_comp_change_trends_smooth.png", replace

**# Coal demand and production trends
use "${input}\03 mine-quarter panel, union safety analysis.dta", clear 

collapse (sum) coal_production_tons, by(year)
gen und_bit_production = coal_production_tons / 1000000

tempfile und_bit_production_temp
save `und_bit_production_temp', replace

import delimited "${data_path}\EIA\Coal Demand and Production\coal-consumption-by-major-end-users.csv", clear rowrange(7) varnames(6)
rename v1 year
keep if inrange(year, 2000, 2022)
gen total_consumption = transportation + residentialandcommercial + cokeplants + otherindustrial + electricpower 
keep year total_consumption

tempfile consumption_temp
save `consumption_temp', replace

import excel "${data_path}\EIA\Coal Demand and Production\tableES1.xls", cellrange(A4:J79) firstrow clear
rename (Year Total1) (year total_production)
keep if inrange(year, 2000, 2022)
keep year total_production
replace total_production = total_production / 1000000
merge 1:1 year using `consumption_temp', assert(3) nogen
merge 1:1 year using `und_bit_production_temp', assert(3) nogen
gen und_bit_production_share = und_bit_production / total_production

#d ;
graph twoway 
	line total_consumption year, msize(vsmall) ||
	line total_production year, msize(vsmall) lpattern("-")
	xlabel(2000 2005 2010 2015 2022)
	ylabel(, angle(0) glwidth(vthin) glcolor(gs14) format(%8.0gc))
	legend(rows(1) region(lstyle(none)) size(small) symxsize(medium) order(1 "United States Coal Consumption" 2 "United States Coal Production"))

	ytitle("Coal (million short tons)")
	xtitle("")
		
	note(
	"Sources: EIA."
	, size(vsmall) span)
	
	graphregion(color(white))
;
#d cr
graph export "${output_figure}\summary_stat_figures\consumption_production_trends.png", replace

**# Coal fatalities over time
import excel "${data_path}\MSHA\Coal Fatalities\coal_fatalities_1900_2022.xlsx", sheet("Data") firstrow clear

gen fatalities_per_100000 = (Fatalities / Miners) * 100000

#d ;
graph twoway 
	line Fatalities Year, msize(vsmall) ||
	line fatalities_per_100000 Year, msize(vsmall) lpattern("-") yaxis(2)
		
	xlabel(1900 1925 1950 1975 2000 2022)
	ylabel(, axis(2) angle(0) glwidth(vthin) glcolor(gs14) format(%8.0gc))
	ylabel(, angle(0) glwidth(vthin) glcolor(gs14) format(%8.0gc))
	legend(rows(1) region(lstyle(none)) size(small) symxsize(medium) order(1 "Fatalities" 2 "Fatalities per 100,000 Miners"))

	ytitle("Fatalities")
	ytitle("Fatalities per 100,000 Miners", axis(2))
	xtitle("")
	
	note(
	"Note: Office workers included starting in 1973."
	" "
	"Source: MSHA."
	, size(vsmall) span)
	
	graphregion(color(white))
;
#d cr
graph export "${output_figure}\summary_stat_figures\fatalities_trends.png", replace
