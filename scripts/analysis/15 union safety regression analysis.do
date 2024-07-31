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
gl output ""

use "${input}\03 mine-quarter panel, union safety analysis.dta", clear

foreach var_to_enc in district year_quarter {
	tostring `var_to_enc', replace
	encode `var_to_enc', gen(`var_to_enc'_enc)
	drop `var_to_enc'
	rename `var_to_enc'_enc `var_to_enc'
} 

*LaTeX output: regsave or outreg2

drop MINE_ID_active_100pct
gen byte MINE_ID_active_anypct = 1
foreach persistence_level of varlist MINE_ID_active_*pct {
	di in red "`persistence_level'"
	
	foreach reg_var_lhs in total_injuries traumatic_injuries violations ss_violations {
		di in red "`reg_var_lhs'"
		di in red "Period 1a"
		qui nbreg `reg_var_lhs' i.union##c.size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs i.year_quarter i.district i.subunit_1 i.subunit_2 i.subunit_3 i.subunit_4 i.subunit_5 i.subunit_6 i.subunit_12 i.subunit_17 i.subunit_30 i.subunit_99 if inrange(year, 2000, 2004) & !productivity_top1pct & `persistence_level', irr exp(labor_hours) vce(cluster MINE_ID)
		if ("`reg_var_lhs'" == "total_injuries" & "`persistence_level'" == "MINE_ID_active_70pct") regsave using "${input}/_temp/15 union safety regression output.dta", replace addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "2000_2004") pval ci rtable
		if ("`reg_var_lhs'" != "total_injuries" | "`persistence_level'" != "MINE_ID_active_70pct") regsave using "${input}/_temp/15 union safety regression output.dta", append addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "2000_2004") pval ci rtable

		di in red "Period 2a"
		qui nbreg `reg_var_lhs' i.union##c.size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs i.year_quarter i.district i.subunit_1 i.subunit_2 i.subunit_3 i.subunit_4 i.subunit_5 i.subunit_6 i.subunit_12 i.subunit_17 i.subunit_30 i.subunit_99 if inrange(year, 2005, 2009) & !productivity_top1pct & `persistence_level', irr exp(labor_hours) vce(cluster MINE_ID)
		regsave using "${input}/_temp/15 union safety regression output.dta", append addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "2005_2009") pval ci rtable

		di in red "Period 3a"
		qui nbreg `reg_var_lhs' i.union##c.size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs i.year_quarter i.district i.subunit_1 i.subunit_2 i.subunit_3 i.subunit_4 i.subunit_5 i.subunit_6 i.subunit_12 i.subunit_17 i.subunit_30 i.subunit_99 if inrange(year, 2010, 2015) & !productivity_top1pct & `persistence_level', irr exp(labor_hours) vce(cluster MINE_ID)
		regsave using "${input}/_temp/15 union safety regression output.dta", append addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "2010_2015") pval ci rtable

		di in red "Period 4a"
		qui nbreg `reg_var_lhs' i.union##c.size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs i.year_quarter i.district i.subunit_1 i.subunit_2 i.subunit_3 i.subunit_4 i.subunit_5 i.subunit_6 i.subunit_12 i.subunit_17 i.subunit_30 i.subunit_99 if inrange(year, 2016, 2022) & !productivity_top1pct & `persistence_level', irr exp(labor_hours) vce(cluster MINE_ID)
		regsave using "${input}/_temp/15 union safety regression output.dta", append addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "2016_2022") pval ci rtable
		
		di in red "Period 1b"
		qui nbreg `reg_var_lhs' i.union##c.size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs i.year_quarter i.district i.subunit_1 i.subunit_2 i.subunit_3 i.subunit_4 i.subunit_5 i.subunit_6 i.subunit_12 i.subunit_17 i.subunit_30 i.subunit_99 if inrange(year, 2000, 2010) & !productivity_top1pct & `persistence_level', irr exp(labor_hours) vce(cluster MINE_ID)
		regsave using "${input}/_temp/15 union safety regression output.dta", append addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "2000_2010") pval ci rtable

		di in red "Period 2b"
		qui nbreg `reg_var_lhs' i.union##c.size_100FTEs ln_controller_size_100FTEs mine_age productivity penalty_points_prev4qtrs i.year_quarter i.district i.subunit_1 i.subunit_2 i.subunit_3 i.subunit_4 i.subunit_5 i.subunit_6 i.subunit_12 i.subunit_17 i.subunit_30 i.subunit_99 if inrange(year, 2011, 2022) & !productivity_top1pct & `persistence_level', irr exp(labor_hours) vce(cluster MINE_ID)
		regsave using "${input}/_temp/15 union safety regression output.dta", append addlabel(model,"`reg_var_lhs', `persistence_level'", lhs_var, "`reg_var_lhs'", persistence_level, "`persistence_level'", year_range, "2011_2022") pval ci rtable
	}
}
