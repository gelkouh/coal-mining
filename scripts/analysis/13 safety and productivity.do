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

**# productivity vs. traumatic injuries; productivity vs. S\&S violations
use "${input}\03 mine-quarter panel, union safety analysis.dta", clear 

gen lnproductivity = ln(productivity)
gen lntrauminjrate = ln(traumatic_injury_rate)
gen lnssviolrate = ln(ss_violation_rate)

reg lnproductivity traumatic_injury_rate, robust
local nobs = e(N)
binscatter lnproductivity traumatic_injury_rate, ///
	xtitle("Traumatic Injury Rate") ///
	ytitle("ln(Productivity (tons/2,000 hrs))") ///
	note("Binscatter: `nobs' mine-quarters") 
graph export "${output_figure}\binscatters\binscatter_lnprod_inj.png", replace 
	
reg lnproductivity ss_violation_rate, robust
local nobs = e(N)
binscatter lnproductivity ss_violation_rate, ///
	xtitle("S&S Violation Rate") ///
	ytitle("ln(Productivity (tons/2,000 hrs))") ///
	note("Binscatter: `nobs' mine-quarters") 
graph export "${output_figure}\binscatters\binscatter_lnprod_viols.png", replace 

reg lnproductivity lntrauminjrate, robust
local nobs = e(N)
binscatter lnproductivity lntrauminjrate, ///
	xtitle("ln(Traumatic Injury Rate)") ///
	ytitle("ln(Productivity (tons/2,000 hrs))") ///
	note("Binscatter: `nobs' mine-quarters") 
graph export "${output_figure}\binscatters\binscatter_lnprod_lninj.png", replace 

reg lnproductivity lnssviolrate, robust
local nobs = e(N)
binscatter lnproductivity lnssviolrate, ///
	xtitle("ln(S&S Violation Rate)") ///
	ytitle("ln(Productivity (tons/2,000 hrs))") ///
	note("Binscatter: `nobs' mine-quarters") 
graph export "${output_figure}\binscatters\binscatter_lnprod_lnviols.png", replace 
