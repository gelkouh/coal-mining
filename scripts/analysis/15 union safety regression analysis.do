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
gl output_table "..\..\output\tables\irr_tables_union_premium"

use "${input}\03 mine-quarter panel, union safety analysis.dta", clear

foreach var_to_enc in district year_quarter {
	tostring `var_to_enc', replace
	encode `var_to_enc', gen(`var_to_enc'_enc)
	drop `var_to_enc'
	rename `var_to_enc'_enc `var_to_enc'
} 

drop MINE_ID_active_100pct
gen byte MINE_ID_active_anypct = 1

lab var union "Union"
lab var size_100FTEs "Mine Size"
lab var ln_controller_size_100FTEs "ln(Controller Size)"
lab var mine_age "Mine Age"
lab var productivity "Productivity"
lab var penalty_points_prev4qtrs "Penalty Pts Last 4 Qtrs"
lab var total_injuries "Total Injury Count"
lab var traumatic_injuries "Traumatic Injury Count"
lab var violations "Total MSHA Violation Count" 
lab var ss_violations "Significant and Substantial MSHA Violation Count"

foreach persistence_level of varlist MINE_ID_active_*pct {
	di in red "`persistence_level'"
	
	foreach reg_var_lhs in total_injuries traumatic_injuries violations ss_violations {
		di in red "`reg_var_lhs'"
		
		foreach time_period in 2000_2004 2005_2009 2010_2015 2016_2022 2000_2010 2011_2022 2000_2022 {
			di in red "`time_period'"
			
			local min_year = real(substr("`time_period'", 1, 4))
			local max_year = real(substr("`time_period'", 6, 4))
			
			eststo: qui nbreg `reg_var_lhs' i.union##c.size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs i.year_quarter i.district i.subunit_1 i.subunit_2 i.subunit_3 i.subunit_4 i.subunit_5 i.subunit_6 i.subunit_12 i.subunit_17 i.subunit_30 i.subunit_99 if inrange(year, `min_year', `max_year') & !productivity_top1pct & `persistence_level', irr exp(labor_hours) vce(cluster MINE_ID)
			
			if ("`reg_var_lhs'" == "total_injuries" & "`persistence_level'" == "MINE_ID_active_70pct" & "`time_period'" == "2000_2004") regsave using "${input}/_temp/15 union safety regression output.dta", replace addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "`time_period'") pval ci rtable
			if ("`reg_var_lhs'" != "total_injuries" | "`persistence_level'" != "MINE_ID_active_70pct" | "`time_period'" != "2000_2004") regsave using "${input}/_temp/15 union safety regression output.dta", append addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "`time_period'") pval ci rtable
		}
		
		local lhs_lab : variable label `reg_var_lhs'
		
		esttab using "${output_table}\irr_table_`persistence_level'_`reg_var_lhs'.tex", nonumbers label se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) replace ///
		mtitles("\shortstack{(1)\\2000-2004}" "\shortstack{(2)\\2005-2009}" "\shortstack{(3)\\2010-2015}" "\shortstack{(4)\\2016-2022}" "\shortstack{(5)\\2000-2010}" "\shortstack{(6)\\2011-2022}" "\shortstack{(7)\\2000-2022}") ///
		keep(1.union 1.union#c.size_100FTEs size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs) indicate("Mine Subunit FE=*.subunit_*" "MSHA Mine District FE=*.district" "Year-Quarter FE=*.year_quarter") ///
		varwidth(25) interaction(" $\times$ ")style(tex) eform eqlabel("") mgroups("`lhs_lab'", prefix(\multicolumn{@span}{c}{) suffix(}) span) ///
		stats(N, fmt(%10.0f)) ///
		nonotes addnotes( ///
			"Exponentiated coefficients; standard errors in parentheses clustered at mine level." ///
			"\sym{*} \(p<0.05\), \sym{**} \(p<0.01\), \sym{***} \(p<0.001\)" ///
			)
		eststo clear
		
		foreach time_period in 2000_2004 2005_2009 2010_2015 2016_2022 2000_2010 2011_2022 2000_2022 {
			di in red "no_interaction_`time_period'"
			
			local min_year = real(substr("`time_period'", 1, 4))
			local max_year = real(substr("`time_period'", 6, 4))
			
			eststo: qui nbreg `reg_var_lhs' i.union c.size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs i.year_quarter i.district i.subunit_1 i.subunit_2 i.subunit_3 i.subunit_4 i.subunit_5 i.subunit_6 i.subunit_12 i.subunit_17 i.subunit_30 i.subunit_99 if inrange(year, `min_year', `max_year') & !productivity_top1pct & `persistence_level', irr exp(labor_hours) vce(cluster MINE_ID)
			
			if ("`reg_var_lhs'" == "total_injuries" & "`persistence_level'" == "MINE_ID_active_70pct" & "`time_period'" == "2000_2004") regsave using "${input}/_temp/15 union safety regression output - no interaction.dta", replace addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "`time_period'") pval ci rtable
			if ("`reg_var_lhs'" != "total_injuries" | "`persistence_level'" != "MINE_ID_active_70pct" | "`time_period'" != "2000_2004") regsave using "${input}/_temp/15 union safety regression output - no interaction.dta", append addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "`time_period'") pval ci rtable
		}
		
		local lhs_lab : variable label `reg_var_lhs'
		
		esttab using "${output_table}\irr_table_no_interaction_`persistence_level'_`reg_var_lhs'.tex", nonumbers label se(%10.3f) star(* 0.05 ** 0.01 *** 0.001) replace ///
		mtitles("\shortstack{(1)\\2000-2004}" "\shortstack{(2)\\2005-2009}" "\shortstack{(3)\\2010-2015}" "\shortstack{(4)\\2016-2022}" "\shortstack{(5)\\2000-2010}" "\shortstack{(6)\\2011-2022}" "\shortstack{(7)\\2000-2022}") ///
		keep(1.union size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs) indicate("Mine Subunit FE=*.subunit_*" "MSHA Mine District FE=*.district" "Year-Quarter FE=*.year_quarter") ///
		varwidth(25) interaction(" $\times$ ")style(tex) eform eqlabel("") mgroups("`lhs_lab'", prefix(\multicolumn{@span}{c}{) suffix(}) span) ///
		stats(N, fmt(%10.0f)) ///
		nonotes addnotes( ///
			"Exponentiated coefficients; standard errors in parentheses clustered at mine level." ///
			"\sym{*} \(p<0.05\), \sym{**} \(p<0.01\), \sym{***} \(p<0.001\)" ///
			)
		eststo clear
	}
}
