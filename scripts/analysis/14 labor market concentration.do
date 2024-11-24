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
gl output_figure "..\..\output\figures"

**# county-level HHI (shares defined for mines and controllers; distribution for mines and for counties)
use "${input}\03 mine-quarter panel, union safety analysis.dta", clear 
isid MINE_ID year_quarter 

foreach share_type in mines controllers {
	preserve 
		if ("`share_type'" == "controllers") gcollapse (sum) labor_hours ss_violations traumatic_injuries, by(county_fips year_quarter CONTROLLER_ID)
		gegen labor_hours_county = sum(labor_hours), by(county_fips year_quarter)
		gen mine_labor_share_sq = (100 * (labor_hours / labor_hours_county))^2
		gcollapse (sum) hhi=mine_labor_share_sq labor_hours ss_violations traumatic_injuries, by(county_fips year_quarter)
		summ hhi
		local mean = round(r(mean))
		
		#d ;
		histogram 
			hhi, color(navy) freq width(250) discrete
				
			xline(`mean', lcolor(red) lwidth(wide))		
			xtitle("County-Level Bituminous Coal Mine Labor Market HHI")
			ytitle("Number of County-Quarters")
			ylabel(, angle(0) glwidth(vthin) glcolor(gs14) format(%8.0gc))
			xlabel(, angle(0) glwidth(vthin) glcolor(gs14) format(%8.0fc))
			
			note("Notes:" "[1] Each observation represents a county-quarter." "[2] Bin width is 250." "[3] Vertical line is at the county-quarter average HHI of `mean'.")

					
			graphregion(color(white))
		;
		#d cr
		graph export "${output_figure}\labor_mkt_concentration\hhi_county_qtr_distn_`share_type'.png", replace
		
		if ("`share_type'" == "controllers") {
			tempfile controller_county_temp
			save `controller_county_temp', replace
		}
	restore
}

**# trends in national and local concentration
use `controller_county_temp', clear

foreach rate_var in ss_violations traumatic_injuries {
	gen `rate_var'_rate = 2000 * (`rate_var' / labor_hours)
	
	if ("`rate_var'" == "ss_violations") local ylab = "S&S Violation Rate"
	if ("`rate_var'" == "traumatic_injuries") local ylab = "Traumatic Injury Rate"
	
	reg `rate_var'_rate hhi, robust
	local nobs = e(N)
	
	binscatter `rate_var'_rate hhi, ///
		xtitle("HHI") ///
		ytitle("`ylab'") ///
		note("Binscatter: `nobs' mine-quarters") 
	graph export "${output_figure}\labor_mkt_concentration\binscatter_`rate_var'_hhi.png", replace 
	
	binscatter `rate_var'_rate hhi, ///
		 absorb(year_quarter) ///
		 controls(i.county_fips) ///
		xtitle("HHI") ///
		ytitle("`ylab'") ///
		note("Binscatter: `nobs' mine-quarters") 
	graph export "${output_figure}\labor_mkt_concentration\binscatter_`rate_var'_hhi_FE.png", replace 
}

gen hhi_x_labor_hours = hhi*labor_hours
gcollapse (sum) hhi_x_labor_hours labor_hours, by(year_quarter) 
gen hhi_county_weighted = hhi_x_labor_hours/labor_hours

save `controller_county_temp', replace

use "${input}\03 mine-quarter panel, union safety analysis.dta", clear 

gcollapse (sum) labor_hours, by(year_quarter CONTROLLER_ID)
gegen labor_hours_total = sum(labor_hours), by(year_quarter)
gen labor_share_sq = (100 * (labor_hours / labor_hours_total))^2
gcollapse (sum) hhi_natl=labor_share_sq, by(year_quarter)

merge 1:1 year_quarter using `controller_county_temp', assert(3) nogen

#d ;
graph twoway 
	line hhi_county_weighted year_quarter, msize(vsmall) ||
	line hhi_natl year_quarter, msize(vsmall) lpattern("-")
		
	xlabel(2000 2005 2010 2015 2022)
	ylabel(, angle(0) glwidth(vthin) glcolor(gs14) format(%8.0gc))
	legend(rows(1) region(lstyle(none)) size(small) symxsize(medium) order(1 "County Weighted Average" 2 "National"))

	ytitle("Labor Market HHI")
	xtitle("")
	
	graphregion(color(white))
;
#d cr
graph export "${output_figure}\labor_mkt_concentration\hhi_trends.png", replace
