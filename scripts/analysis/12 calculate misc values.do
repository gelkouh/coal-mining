clear all
set type double

gl data_path "C:\Users\celkouh\Downloads\data"
gl output_calculated_values "..\..\output\calculated_values"

**# Inflation-adjusted amount paid per violation, average S&S violations and traumatic injuries per FTE
import excel using "${data_path}\CPI\FRED data.xlsx", sheet("FRED") cellrange(B11:C779) firstrow allstring clear
rename (B USACPIALLMINMEI) (year cpi)

destring *, replace
keep if inrange(year, 2000, 2022)

gcollapse (mean) cpi_annual_mean=cpi, by(year)
sort year
gen cpi_indexed_2022 = 100 * (cpi_annual_mean / cpi_annual_mean[_N])

tempfile cpi_2000_2022
save `cpi_2000_2022', replace

use "${data_path}\Output\03 mine-quarter panel, union safety analysis.dta", clear 

gcollapse (sum) violations_amt_paid violations_amt_due violations ss_violations traumatic_injuries labor_hours, by(year)

merge 1:1 year using `cpi_2000_2022', assert(3) nogen

foreach viol_amt in paid due {
	gen violations_amt_`viol_amt'_real = 100 * (violations_amt_`viol_amt' / cpi_indexed_2022)
}

gcollapse (sum) *_real violations ss_violations traumatic_injuries labor_hours

foreach viol_amt in paid due {
	gen violations_amt_`viol_amt'_real_avg = violations_amt_`viol_amt'_real / violations
}

foreach rate_var in ss_violations traumatic_injuries {
	gen `rate_var'_per_FTE = 2000 * (`rate_var'/ labor_hours)
}

keep *_real_avg *_per_FTE

export delimited "${output_calculated_values}\misc_values.csv", replace
