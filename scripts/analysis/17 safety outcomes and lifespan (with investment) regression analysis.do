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

use "${input}\03 mine-quarter panel, union safety analysis.dta", clear

foreach reg_var in traumatic_injury_rate ss_violation_rate MINE_ID_active_qtrs {
	gen `reg_var'_ln = ln(`reg_var')
}

foreach var_to_enc in district msha_office_code year_quarter {
	tostring `var_to_enc', replace
	encode `var_to_enc', gen(`var_to_enc'_enc)
	drop `var_to_enc'
	rename `var_to_enc'_enc `var_to_enc'
} 

gegen MINE_ID_max_year = max(year), by(MINE_ID)
gen byte MINE_ID_not_in_2022 = MINE_ID_max_year != 2022
gegen MINE_ID_max_year_q = max(year_quarter), by(MINE_ID)
gen MINE_ID_qtrs_remaining = (MINE_ID_max_year_q - year_quarter) + 1
gen MINE_ID_qtrs_remaining_ln = ln(MINE_ID_qtrs_remaining)

// IVs
foreach iv_level of varlist district msha_office_code {
	if ("`iv_level'" == "district") local iv_var_prefix district
	if ("`iv_level'" == "msha_office_code") local iv_var_prefix msha_office
	
	preserve
		keep `iv_level' MINE_ID MINE_ID_active_qtrs
		duplicates drop
		
		gen byte mine_count = 1
		
		gegen iv_level_active_qtrs = sum(MINE_ID_active_qtrs), by(`iv_level')
		gegen iv_level_mines = sum(mine_count), by(`iv_level')
		
		gen `iv_var_prefix'_act_qtrs_iv = ((iv_level_mines - 1) / (iv_level_active_qtrs - MINE_ID_active_qtrs))
		
		keep MINE_ID `iv_var_prefix'_act_qtrs_iv
		
		isid MINE_ID
		
		tempfile iv_level_act_qtrs_iv_temp
		save `iv_level_act_qtrs_iv_temp', replace 
	restore 
	preserve
		keep `iv_level' year_quarter MINE_ID ss_violations labor_hours
		duplicates drop
			
		gegen iv_level_ss_violations = sum(ss_violations), by(`iv_level' year_quarter)
		gegen iv_level_labor_hours = sum(labor_hours), by(`iv_level' year_quarter)
		
		gen `iv_var_prefix'_ss_viols_iv = ((iv_level_labor_hours - labor_hours) / (2000*(iv_level_ss_violations - ss_violations)))
		
		keep year_quarter MINE_ID `iv_var_prefix'_ss_viols_iv
		
		isid year_quarter MINE_ID
		
		tempfile iv_level_ss_viols_iv_temp
		save `iv_level_ss_viols_iv_temp', replace 
	restore
	preserve
		keep `iv_level' year_quarter MINE_ID MINE_ID_qtrs_remaining
		duplicates drop
		
		gen byte mine_count = 1
			
		gegen iv_level_qtrs_remaining = sum(MINE_ID_qtrs_remaining), by(`iv_level' year_quarter)
		gegen iv_level_mines = sum(mine_count), by(`iv_level' year_quarter)
		
		gen `iv_var_prefix'_qtrs_remaining_iv = ((iv_level_mines - 1) / (iv_level_qtrs_remaining - MINE_ID_qtrs_remaining))
		gen `iv_var_prefix'_qtrs_remaining_iv_ln = ln(`iv_var_prefix'_qtrs_remaining_iv)
		
		keep year_quarter MINE_ID `iv_var_prefix'_qtrs_remaining_iv*
		
		isid year_quarter MINE_ID
		
		tempfile iv_level_qtrs_remaining_iv_temp
		save `iv_level_qtrs_remaining_iv_temp', replace 
	restore
	
	merge m:1 MINE_ID using `iv_level_act_qtrs_iv_temp', assert(3) nogen
	merge m:1 year_quarter MINE_ID using `iv_level_ss_viols_iv_temp', assert(3) nogen
	merge m:1 year_quarter MINE_ID using `iv_level_qtrs_remaining_iv_temp', assert(3) nogen
}

gegen seam_height_in_max = max(seam_height_in)
replace seam_height_in = . if inlist(seam_height_in, 0, seam_height_in_max) // don't use implausibly large values or 0 coal seam heights
rename seam_height_in MINE_ID_coal_seam
gen MINE_ID_coal_seam_ln = ln(MINE_ID_coal_seam)
gegen MINE_ID_mean_coal_seam = mean(MINE_ID_coal_seam), by(MINE_ID)
gegen MINE_ID_max_coal_seam = max(MINE_ID_coal_seam), by(MINE_ID)
gen MINE_ID_mean_coal_seam_ln = ln(MINE_ID_mean_coal_seam)
gen MINE_ID_max_coal_seam_ln = ln(MINE_ID_max_coal_seam)

lab var msha_office_ss_viols_iv "1 / (S\&S Viol Rate Same MSHA Office)"
lab var MINE_ID_mean_coal_seam "ln(Coal Seam Height)"
lab var MINE_ID_mean_coal_seam_ln "ln(Mean Coal Seam Height)"

lab var MINE_ID_active_qtrs_ln "ln(Active Quarters 2000-2022)"
lab var MINE_ID_qtrs_remaining_ln "ln(Quarters Until Closing)"
lab var ss_violation_rate_ln "ln(S\&S MSHA Violation Rate)"

**# 1a
lab var ss_violation_rate "S\&S Viol Rate"
lab var district_ss_viols_iv "1 / (S\&S Viol Rate in Dist)"
lab var penalty_points_prev4qtrs "Penalty Pts in Prev 4 Qtrs"

eststo: qui ivpoisson cfunction traumatic_injuries (ss_violation_rate = district_ss_viols_iv) i.district, exp(labor_hours) vce(cluster MINE_ID) irr
eststo: qui ivpoisson cfunction traumatic_injuries (ss_violation_rate = penalty_points_prev4qtrs), exp(labor_hours) vce(cluster MINE_ID) irr
eststo: qui ivpoisson cfunction traumatic_injuries (ss_violation_rate = district_ss_viols_iv penalty_points_prev4qtrs) i.district, exp(labor_hours) vce(cluster MINE_ID) irr

esttab using "${output_table}\cf_tables\cf_table_inj_viols_embed.tex", label se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) replace ///
mtitles("\shortstack{(1)}" "\shortstack{(2)}" "\shortstack{(3)}") ///
prehead("{ \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{l*{3}{c}} \hline\hline") ///
posthead("\hline \\ \multicolumn{3}{c}{\textit{A. Traumatic Injury Count}} \\") ///
keep(ss_violation_rate) ///
fragment varwidth(25) eform eqlabel("") nonotes stats(noobs, labels(" ")) nolines nonumbers

esttab using "${output_table}\cf_tables\cf_table_inj_viols_embed.tex", nonumbers label se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) append ///
nomtitles ///
posthead("\hline \\ \multicolumn{3}{c}{\textit{B. S\&S Viol Rate}} \\") ///
keep(district_ss_viols_iv penalty_points_prev4qtrs) ///
fragment varwidth(25) eqlabel("") nonotes stats(noobs, labels(" ")) nolines nonumbers

esttab using "${output_table}\cf_tables\cf_table_inj_viols_embed.tex", nonumbers se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) append ///
nomtitles ///
posthead("\hline \\ \multicolumn{3}{c}{\textit{C. Tests for Endogeneity of Covariates}} \\") ///
keep(c_ss_violation_rate) ///
coeflabels(c_ss_violation_rate "S\&S Viol Rate") ///
fragment varwidth(25) eqlabel("") nonotes stats(noobs, labels(" ")) nolines nonumbers

esttab using "${output_table}\cf_tables\cf_table_inj_viols_embed.tex", nonumbers label se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) append ///
nomtitles ///
posthead("\hline \\") ///
keep(" ") indicate("MSHA Mine District FE=*.district") ///
stats(N, fmt(%10.0f)) ///
fragment varwidth(25) eqlabel("") ///
prefoot("\\ \hline") ///
postfoot("\hline\hline \multicolumn{3}{l}{\footnotesize Exponentiated coefficients in Panel A; standard errors in parentheses clustered at mine level.}\\\multicolumn{3}{l}{\footnotesize \sym{*} \(p<0.05\), \sym{**} \(p<0.01\), \sym{***} \(p<0.001\)}\\ \end{tabular} }")
eststo clear

tempfile mine_quarter_dta
save `mine_quarter_dta', replace

**# 1b
collapse (sum) traumatic_injuries ss_violations labor_hours (mean) MINE_ID_active_qtrs MINE_ID_mean_coal_seam MINE_ID_max_coal_seam, by(MINE_ID msha_office_code district)
isid MINE_ID

gen traumatic_injury_rate = 2000 * (traumatic_injuries / labor_hours)
gen traumatic_injury_rate_ln = ln(traumatic_injury_rate)

gen ss_violation_rate = 2000 * (ss_violations / labor_hours)
gen ss_violation_rate_ln = ln(ss_violation_rate)

// IVs
foreach iv_level of varlist district msha_office_code {
	if ("`iv_level'" == "district") local iv_var_prefix district
	if ("`iv_level'" == "msha_office_code") local iv_var_prefix msha_office
	
	preserve
		keep `iv_level' MINE_ID MINE_ID_active_qtrs
		duplicates drop
		
		gen byte mine_count = 1
		
		gegen iv_level_active_qtrs = sum(MINE_ID_active_qtrs), by(`iv_level')
		gegen iv_level_mines = sum(mine_count), by(`iv_level')
		
		gen `iv_var_prefix'_act_qtrs_iv = ln((iv_level_mines - 1) / (iv_level_active_qtrs - MINE_ID_active_qtrs))
		
		keep MINE_ID `iv_var_prefix'_act_qtrs_iv
		
		isid MINE_ID
		
		tempfile iv_level_act_qtrs_iv_temp
		save `iv_level_act_qtrs_iv_temp', replace 
	restore 
	
	merge m:1 MINE_ID using `iv_level_act_qtrs_iv_temp', assert(3) nogen
}

gen MINE_ID_mean_coal_seam_ln = ln(MINE_ID_mean_coal_seam)
gen MINE_ID_max_coal_seam_ln = ln(MINE_ID_max_coal_seam)

gen MINE_ID_active_qtrs_ln = ln(MINE_ID_active_qtrs)
lab var MINE_ID_active_qtrs "Active Quarters 2000-2022"
lab var MINE_ID_active_qtrs_ln "ln(Active Quarters 2000-2022)"

reg MINE_ID_active_qtrs_ln msha_office_act_qtrs_iv, robust
	local firststageFstat = e(F)
	local nobs = e(N)
	binscatter MINE_ID_active_qtrs_ln msha_office_act_qtrs_iv, ///
		xtitle("ln(1 / (S&S Violation Rate Same MSHA Office))") ///
		ytitle("ln(Active Quarters 2000-2022)") ///
		note("Binscatter: `nobs' mines" "First-stage F-statistic: `: di %5.2f `firststageFstat''") 
	graph export "${output_figure}\binscatters\binscatter_mine_level_msha_office_first_stage.png", replace

reg MINE_ID_active_qtrs_ln MINE_ID_max_coal_seam_ln, robust
	local firststageFstat = e(F)
	local nobs = e(N)
	binscatter MINE_ID_active_qtrs_ln MINE_ID_max_coal_seam_ln, ///
		xtitle("ln(Max Coal Seam Height)") ///
		ytitle("ln(Active Quarters 2000-2022)") ///
		note("Binscatter: `nobs' mines" "First-stage F-statistic: `: di %5.2f `firststageFstat''") 
	graph export "${output_figure}\binscatters\binscatter_mine_level_coal_seam_first_stage.png", replace
	
**# 1b.i
reg ss_violation_rate MINE_ID_active_qtrs_ln, robust
	local firststageFstat = e(F)
	local nobs = e(N)
	binscatter ss_violation_rate MINE_ID_active_qtrs_ln, ///
		xtitle("ln(Active Quarters 2000-2022)") ///
		ytitle("S&S Violation Rate") ///
		note("Binscatter: `nobs' mines") 
	graph export "${output_figure}\binscatters\binscatter_mine_level_viol_rate_activeq.png", replace

eststo: qui reg ss_violation_rate MINE_ID_active_qtrs_ln, robust
eststo: qui reg ss_violation_rate MINE_ID_active_qtrs_ln i.msha_office_code, robust
eststo: qui ivreg2 ss_violation_rate (MINE_ID_active_qtrs_ln = msha_office_act_qtrs_iv) i.msha_office_code, first robust
eststo: qui ivreg2 ss_violation_rate (MINE_ID_active_qtrs_ln = MINE_ID_max_coal_seam_ln), first robust

**# 1b.ii
reg traumatic_injury_rate MINE_ID_active_qtrs_ln, robust
	local firststageFstat = e(F)
	local nobs = e(N)
	binscatter traumatic_injury_rate MINE_ID_active_qtrs_ln, ///
		xtitle("ln(Active Quarters 2000-2022)") ///
		ytitle("Traumatic Injury Rate") ///
		note("Binscatter: `nobs' mines") 
	graph export "${output_figure}\binscatters\binscatter_mine_level_injury_rate_activeq.png", replace

eststo: qui reg traumatic_injury_rate MINE_ID_active_qtrs_ln, robust
eststo: qui reg traumatic_injury_rate MINE_ID_active_qtrs_ln i.district, robust
eststo: qui ivreg2 traumatic_injury_rate (MINE_ID_active_qtrs_ln = msha_office_act_qtrs_iv) i.msha_office_code, first robust
eststo: qui ivreg2 traumatic_injury_rate (MINE_ID_active_qtrs_ln = MINE_ID_max_coal_seam_ln), first robust

esttab using "${output_table}\ols_iv_tables\ols_iv_table_lifespan_embed.tex", nonumbers label se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) replace ///
mtitles("\shortstack{(1)\\OLS}" "\shortstack{(2)\\OLS}" "\shortstack{(3)\\2SLS,\\MSHA Office}" "\shortstack{(4)\\2SLS,\\Coal Seam}" "\shortstack{(5)\\OLS}" "\shortstack{(6)\\OLS}" "\shortstack{(7)\\2SLS,\\MSHA Office}" "\shortstack{(8)\\2SLS,\\Coal Seam}") ///
keep(MINE_ID_active_qtrs_ln) indicate("MSHA Office FE=*.msha_office_code") ///
varwidth(25) interaction(" $\times$ ")style(tex) stats(cdf N, fmt(%10.1f %10.0f) labels("First stage F-stat.")) eqlabel("") mgroups("S\&S Violation Rate" "Traumatic Injury Injury Rate", pattern(1 0 0 0 1 0 0 0) prefix(\multicolumn{@span}{c}{) suffix(}) span) ///
nonotes addnotes( ///
    "Heteroskedasticity-robust standard errors are in parentheses." ///
    "\sym{*} \(p<0.05\), \sym{**} \(p<0.01\), \sym{***} \(p<0.001\)" ///
    )
eststo clear

use `mine_quarter_dta', clear

/*
**# 1b.i
binscatter ss_violation_rate MINE_ID_active_qtrs
ivpoisson cfunction ss_violations (MINE_ID_active_qtrs = district_act_qtrs_iv) i.district, exp(labor_hours) vce(cluster MINE_ID) irr
ivpoisson cfunction ss_violations (MINE_ID_active_qtrs = MINE_ID_max_coal_seam), exp(labor_hours) vce(cluster MINE_ID) irr

**# 1b.ii
binscatter traumatic_injury_rate MINE_ID_active_qtrs
ivpoisson cfunction traumatic_injuries (MINE_ID_active_qtrs = district_act_qtrs_iv) i.district, exp(labor_hours) vce(cluster MINE_ID) irr
ivpoisson cfunction traumatic_injuries (MINE_ID_active_qtrs = MINE_ID_max_coal_seam), exp(labor_hours) vce(cluster MINE_ID) irr
*/

**# 1b.iii
gen endog_interaction = MINE_ID_active_qtrs * ss_violation_rate
gen iv_interaction = district_act_qtrs_iv * district_ss_viols_iv

eststo: qui ivpoisson cfunction traumatic_injuries (ss_violation_rate endog_interaction = district_ss_viols_iv penalty_points_prev4qtrs iv_interaction MINE_ID_max_coal_seam), exp(labor_hours) vce(cluster MINE_ID) irr

eststo: qui ivpoisson cfunction traumatic_injuries (ss_violation_rate endog_interaction = penalty_points_prev4qtrs iv_interaction MINE_ID_max_coal_seam) i.district, exp(labor_hours) vce(cluster MINE_ID) irr

eststo: qui ivpoisson cfunction traumatic_injuries (ss_violation_rate endog_interaction = district_ss_viols_iv penalty_points_prev4qtrs iv_interaction MINE_ID_max_coal_seam) i.district, exp(labor_hours) vce(cluster MINE_ID) irr // p-value on endog_interaction estimate: 0.053

lab var endog_interaction "Act Qtrs $\times$ S\&S Viol Rate"
lab var iv_interaction "[1 / (Mean Act Qtrs in Dist)] $\times$ [1 / (S\&S Viol Rate in Dist)]"
lab var ss_violation_rate "S\&S Viol Rate"
lab var penalty_points_prev4qtrs "Penalty Pts in Prev 4 Qtrs"
lab var MINE_ID_max_coal_seam "Max Coal Seam Height"
lab var district_ss_viols_iv "1 / (S\&S Viol Rate in Dist)"

esttab using "${output_table}\cf_tables\cf_table_inj_viols_int_embed.tex", label se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) replace ///
mtitles("\shortstack{(1)}" "\shortstack{(2)}" "\shortstack{(3)}") ///
prehead("{ \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi} \begin{tabular}{l*{3}{c}} \hline\hline") ///
posthead("\hline \\ \multicolumn{3}{c}{\textit{A. Traumatic Injury Count}} \\") ///
keep(ss_violation_rate endog_interaction) ///
fragment varwidth(25) eform eqlabel("") nonotes stats(noobs, labels(" ")) nolines nonumbers

lab var ss_violation_rate "\hline \\\ \multicolumn{3}{c}{\textit{B. S\&S Viol Rate}} \\\"
lab var endog_interaction "\hline \\\ \multicolumn{3}{c}{\textit{C. Act Qtrs $\times$ S\&S Viol Rate}} \\\"

esttab using "${output_table}\cf_tables\cf_table_inj_viols_int_embed.tex", nonumbers label se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) append ///
nomtitles ///
posthead("\hline \\ \multicolumn{3}{c}{\textit{B. S\&S Viol. Rate}} \\") ///
keep(district_ss_viols_iv penalty_points_prev4qtrs iv_interaction MINE_ID_max_coal_seam) ///
fragment varwidth(25) eqlabel("") nonotes stats(noobs, labels(" ")) nolines nonumbers

esttab using "${output_table}\cf_tables\cf_table_inj_viols_int_embed.tex", nonumbers se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) append ///
nomtitles ///
posthead("\hline \\ \multicolumn{3}{c}{\textit{D. Tests for Endogeneity of Covariates}} \\") ///
keep(c_ss_violation_rate c_endog_interaction) ///
coeflabels(c_ss_violation_rate "S\&S Viol Rate" c_endog_interaction "Act Qtrs $\times$ S\&S Viol Rate") ///
fragment varwidth(25) eqlabel("") nonotes stats(noobs, labels(" ")) nolines nonumbers

esttab using "${output_table}\cf_tables\cf_table_inj_viols_int_embed.tex", nonumbers label se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) append ///
nomtitles ///
posthead("\hline \\") ///
keep(" ") indicate("MSHA Mine District FE=*.district") ///
stats(N, fmt(%10.0f)) ///
fragment varwidth(25) eqlabel("") ///
prefoot("\\ \hline") ///
postfoot("\hline\hline \multicolumn{3}{l}{\footnotesize Exponentiated coefficients in Panel A; standard errors in parentheses clustered at mine level.}\\\multicolumn{3}{l}{\footnotesize \sym{*} \(p<0.05\), \sym{**} \(p<0.01\), \sym{***} \(p<0.001\)}\\ \end{tabular} }")
eststo clear

**# Robustness checks
*Forward and backward looking predictor variables (e.g., mine quarters remaining and mine age)
