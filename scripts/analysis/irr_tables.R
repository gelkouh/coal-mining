# Last updated: Mar 25, 2023

root <- getwd()
while(basename(root) != "coal-mining") { # this is the name of your project directory you want to use
  root <- dirname(root)
}
source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")
outputdir <- file.path(dir, "coal-mining", "output")

library(tidyverse)
library(texreg)
library(mfx)

mine_panel_quarters_import <- read_csv(file.path(ddir, "cleaned", "mine_panel_quarters.csv"))
mine_panel_years_import <- read_csv(file.path(ddir, "cleaned", "mine_panel_years.csv"))

mines_after_2015_df <- mine_panel_quarters_import %>%
  dplyr::filter(zero_production_quarter == 0, year >= 2015) %>%
  dplyr::select(MINE_ID) %>%
  distinct() %>%
  mutate(active_after2015 = 1)
union_ever_pre2011_df <- mine_panel_quarters_import %>%
  dplyr::filter(zero_production_quarter == 0, year <= 2010) %>%
  group_by(MINE_ID) %>%
  summarize(union_ever_pre2011 = max(union, na.rm = TRUE))
union_ever_df <- mine_panel_quarters_import %>%
  group_by(MINE_ID) %>%
  summarize(union_ever = max(union, na.rm = TRUE))
union_in_county_df <- mine_panel_quarters_import %>%
  dplyr::filter(zero_production_quarter == 0) %>%
  group_by(year_quarter, county_fips) %>%
  summarize(union_in_county = max(union, na.rm = TRUE))
union_in_county_pre_2011_df <- mine_panel_quarters_import %>%
  dplyr::filter(zero_production_quarter == 0, year <= 2010) %>%
  group_by(county_fips) %>%
  summarize(union_in_county_pre2011 = max(union, na.rm = TRUE))

mine_panel_quarters <- mine_panel_quarters_import %>%
  dplyr::filter(zero_production_quarter == 0) %>%
  left_join(union_ever_df, by = "MINE_ID") %>%
  left_join(union_ever_pre2011_df, by = "MINE_ID") %>%
  left_join(mines_after_2015_df, by = "MINE_ID") %>%
  left_join(union_in_county_df, by = c("year_quarter", "county_fips")) %>%
  left_join(union_in_county_pre_2011_df, by = "county_fips") %>%
  group_by(year_quarter, county_fips) %>%
  mutate(active_mines = n()) %>%
  ungroup() %>%
  group_by(year_quarter, county_fips, CONTROLLER_ID) %>%
  mutate(same_controller = n(),
         controller_size_100employees_county = sum(size_100employees, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_same_controller_county = same_controller/active_mines) %>%
  group_by(year_quarter, county_fips) %>%
  mutate(largest_pct_controller = max(pct_same_controller_county, na.rm = TRUE),
         size_100employees_county = sum(size_100employees, na.rm = TRUE),
         controller_share_county = 100*controller_size_100employees_county/size_100employees_county,
         hhi_county = sum(controller_share_county^2, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year_quarter, CONTROLLER_ID) %>%
  mutate(controller_size_100employees_natl = sum(size_100employees, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year_quarter) %>%
  mutate(size_100employees_natl = sum(size_100employees, na.rm = TRUE),
         controller_share_natl = 100*controller_size_100employees_natl/size_100employees_natl,
         hhi_natl = sum(controller_share_natl^2, na.rm = TRUE)) %>%
  ungroup()

mine_panel_years <- mine_panel_years_import %>%
  dplyr::filter(zero_production_year == 0) %>%
  left_join(union_ever_df, by = "MINE_ID") %>%
  left_join(mines_after_2015_df, by = "MINE_ID")

# https://search.r-project.org/CRAN/refmans/texreg/html/extract-negbinirr-method.html
negbinirr(traumatic_injuries ~ union_in_county_pre2011*size_100employees 
          + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
          + factor(year_quarter) + factor(district) 
          + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
          data = filter(mine_panel_quarters, union == 0, year > 2010))

##----------##
# Violations, size_100employees
##----------##
irr_100employees_violations_1 <- extract(negbinirr(violations ~ union*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = mine_panel_quarters),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)
irr_100employees_violations_2 <- extract(negbinirr(violations ~ union*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, year <= 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)
irr_100employees_violations_3 <- extract(negbinirr(violations ~ union*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, year > 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)

irr_100employees_ss_violations_1 <- extract(negbinirr(ss_violations ~ union*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = mine_panel_quarters),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)
irr_100employees_ss_violations_2 <- extract(negbinirr(ss_violations ~ union*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, year <= 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)
irr_100employees_ss_violations_3 <- extract(negbinirr(ss_violations ~ union*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, year > 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)

irr_100employees_violations_table <- texreg(list(irr_100employees_violations_1, irr_100employees_violations_2, 
                                                  irr_100employees_violations_3, irr_100employees_ss_violations_1, 
                                                  irr_100employees_ss_violations_2, irr_100employees_ss_violations_3), 
                                             digits = 3, include.ci = FALSE, #stars = numeric(0),
                                             custom.coef.map = list('union'= " Union ",
                                                                    'size_100employees' = " Mine Size ",
                                                                    'union:size_100employees' = " Union $ \\times $ Mine Size "),
                                             custom.model.names = c("(Total, Full)","(Total, Early)","(Total, Late)",
                                                                    "(S\\&S, Full)","(S\\&S, Early)","(S\\&S, Late)"),
                                             caption = "Effect of Union Status on MSHA Violation Frequency",
                                             label = "irr_100employees_violations")
irr_100employees_violations_table
write.table(irr_100employees_violations_table, file.path(outputdir, 'tables', 'irr_100employees_violations_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

##----------##
# Injuries, size_100employees
##----------##
irr_100employees_total_injuries_1 <- extract(negbinirr(total_injuries ~ union*size_100employees 
                                                   + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                   + factor(year_quarter) + factor(district) 
                                                   + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                   data = mine_panel_quarters),
                                         include.deviance = FALSE,
                                         include.aic = FALSE,
                                         include.bic = FALSE)
irr_100employees_total_injuries_2 <- extract(negbinirr(total_injuries ~ union*size_100employees 
                                                   + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                   + factor(year_quarter) + factor(district) 
                                                   + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                   data = filter(mine_panel_quarters, year <= 2010)),
                                         include.deviance = FALSE,
                                         include.aic = FALSE,
                                         include.bic = FALSE)
irr_100employees_total_injuries_3 <- extract(negbinirr(total_injuries ~ union*size_100employees 
                                                   + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                   + factor(year_quarter) + factor(district) 
                                                   + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                   data = filter(mine_panel_quarters, year > 2010)),
                                         include.deviance = FALSE,
                                         include.aic = FALSE,
                                         include.bic = FALSE)

irr_100employees_traumatic_injuries_1 <- extract(negbinirr(traumatic_injuries ~ union*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = mine_panel_quarters),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)
irr_100employees_traumatic_injuries_2 <- extract(negbinirr(traumatic_injuries ~ union*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, year <= 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)
irr_100employees_traumatic_injuries_3 <- extract(negbinirr(traumatic_injuries ~ union*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, year > 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)

irr_100employees_injuries_table <- texreg(list(irr_100employees_total_injuries_1, irr_100employees_total_injuries_2, 
                                               irr_100employees_total_injuries_3, irr_100employees_traumatic_injuries_1, 
                                               irr_100employees_traumatic_injuries_2, irr_100employees_traumatic_injuries_3), 
                                            digits = 3, include.ci = FALSE, #stars = numeric(0),
                                            custom.coef.map = list('union'= " Union ",
                                                                   'size_100employees' = " Mine Size ",
                                                                   'union:size_100employees' = " Union $ \\times $ Mine Size "),
                                            custom.model.names = c("(Total, Full)","(Total, Early)","(Total, Late)",
                                                                   "(Traum., Full)","(Traum., Early)","(Traum., Late)"),
                                            caption = "Effect of Union Status on Injury Frequency",
                                            label = "irr_100employees_injuries")
irr_100employees_injuries_table
write.table(irr_100employees_injuries_table, file.path(outputdir, 'tables', 'irr_100employees_injuries_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

##----------##
# Active only in second period, size_100employees
##----------##
irr_100employees_activelate_1 <- extract(negbinirr(total_injuries ~ union*size_100employees 
                                                     + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                     + factor(year_quarter) + factor(district) 
                                                     + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                     data = filter(mine_panel_quarters, active_after2015 == 1, year <= 2010)),
                                           include.deviance = FALSE,
                                           include.aic = FALSE,
                                           include.bic = FALSE)
irr_100employees_activelate_2 <- extract(negbinirr(traumatic_injuries ~ union*size_100employees 
                                                       + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                       + factor(year_quarter) + factor(district) 
                                                       + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                       data = filter(mine_panel_quarters, active_after2015 == 1, year <= 2010)),
                                             include.deviance = FALSE,
                                             include.aic = FALSE,
                                             include.bic = FALSE)
irr_100employees_activelate_3 <- extract(negbinirr(violations ~ union*size_100employees 
                                                       + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                       + factor(year_quarter) + factor(district) 
                                                       + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                       data = filter(mine_panel_quarters, active_after2015 == 1, year <= 2010)),
                                             include.deviance = FALSE,
                                             include.aic = FALSE,
                                             include.bic = FALSE)
irr_100employees_activelate_4 <- extract(negbinirr(ss_violations ~ union*size_100employees 
                                                       + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                       + factor(year_quarter) + factor(district) 
                                                       + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                       data = filter(mine_panel_quarters, active_after2015 == 1, year <= 2010)),
                                             include.deviance = FALSE,
                                             include.aic = FALSE,
                                             include.bic = FALSE)

irr_100employees_activelate_table <- texreg(list(irr_100employees_activelate_1, irr_100employees_activelate_2, 
                                                 irr_100employees_activelate_3, irr_100employees_activelate_4), 
                                          digits = 3, include.ci = FALSE, #stars = numeric(0),
                                          custom.coef.map = list('union'= " Union ",
                                                                 'size_100employees' = " Mine Size ",
                                                                 'union:size_100employees' = " Union $ \\times $ Mine Size "),
                                          custom.model.names = c("(Total Inj.)","(Traum. Inj.)", "(Total Viol.)", "(S\\&S Viol.)"),
                                          caption = "Effect of Union Status on Safety in Early Period for Mines in Sample after 2015",
                                          label = "irr_100employees_activelate")
irr_100employees_activelate_table
write.table(irr_100employees_activelate_table, file.path(outputdir, 'tables', 'irr_100employees_activelate_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

##----------##
# Union externalities, size_100employees
##----------##
irr_100employees_union_extern_1 <- extract(negbinirr(traumatic_injuries ~ union_in_county*size_100employees 
                                                       + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                       + factor(year_quarter) + factor(district) 
                                                       + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                       data = filter(mine_panel_quarters, union == 0, year <= 2010)),
                                             include.deviance = FALSE,
                                             include.aic = FALSE,
                                             include.bic = FALSE)
irr_100employees_union_extern_2 <- extract(negbinirr(traumatic_injuries ~ union_in_county*size_100employees 
                                                       + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                       + factor(year_quarter) + factor(district) 
                                                       + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                       data = filter(mine_panel_quarters, union == 0, year > 2010)),
                                             include.deviance = FALSE,
                                             include.aic = FALSE,
                                             include.bic = FALSE)
irr_100employees_union_extern_3 <- extract(negbinirr(ss_violations ~ union_in_county*size_100employees 
                                                     + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                     + factor(year_quarter) + factor(district) 
                                                     + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                     data = filter(mine_panel_quarters, union == 0, year <= 2010)),
                                           include.deviance = FALSE,
                                           include.aic = FALSE,
                                           include.bic = FALSE)
irr_100employees_union_extern_4 <- extract(negbinirr(ss_violations ~ union_in_county*size_100employees 
                                                     + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                     + factor(year_quarter) + factor(district) 
                                                     + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                     data = filter(mine_panel_quarters, union == 0, year > 2010)),
                                           include.deviance = FALSE,
                                           include.aic = FALSE,
                                           include.bic = FALSE)

irr_100employees_union_extern_5 <- extract(negbinirr(traumatic_injuries ~ union_in_county_pre2011*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, union == 0, year > 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)
irr_100employees_union_extern_6 <- extract(negbinirr(ss_violations ~ union_in_county_pre2011*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, union == 0, year > 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)

irr_100employees_union_extern_table <- texreg(list(irr_100employees_union_extern_1, irr_100employees_union_extern_2, 
                                                           irr_100employees_union_extern_3, irr_100employees_union_extern_4,
                                                   irr_100employees_union_extern_5, irr_100employees_union_extern_6), 
                                            digits = 3, include.ci = FALSE, #stars = numeric(0),
                                            custom.coef.map = list('union_in_county'= " Union Cnty ",
                                                                   'union_in_county:size_100employees' = " Union Cnty $ \\times $ Mine Size ",
                                                                   'union_in_county_pre2011'= " Union Cnty Pre-2011 ",
                                                                   'union_in_county_pre2011:size_100employees' = " Union Cnty Pre-2011 $ \\times $ Mine Size ",
                                                                   'size_100employees' = " Mine Size "),
                                            custom.model.names = c("(Traum Inj., E.)","(Traum. Inj., L.)", "(S\\&S Viol., E.)", "(S\\&S Viol., L.)",
                                                                   "(Traum. Inj.)","(S\\&S Viol.)"),
                                            caption = "Effect of Union Spillovers on Nonunion Mine Safety",
                                            label = "irr_100employees_union_extern")
irr_100employees_union_extern_table
write.table(irr_100employees_union_extern_table, file.path(outputdir, 'tables', 'irr_100employees_union_extern_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

##----------##
# Union inertia, size_100employees
##----------##
irr_100employees_union_inertia_1 <- extract(negbinirr(traumatic_injuries ~ union_ever*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, union == 0, year > 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)
irr_100employees_union_inertia_2 <- extract(negbinirr(ss_violations ~ union_ever*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, union == 0, year > 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)

irr_100employees_union_inertia_3 <- extract(negbinirr(traumatic_injuries ~ union_ever_pre2011*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, union == 0, year > 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)
irr_100employees_union_inertia_4 <- extract(negbinirr(ss_violations ~ union_ever_pre2011*size_100employees 
                                                      + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                      + factor(year_quarter) + factor(district) 
                                                      + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                      data = filter(mine_panel_quarters, union == 0, year > 2010)),
                                            include.deviance = FALSE,
                                            include.aic = FALSE,
                                            include.bic = FALSE)

irr_100employees_union_inertia_table <- texreg(list(irr_100employees_union_inertia_1, irr_100employees_union_inertia_2, 
                                                    irr_100employees_union_inertia_3, irr_100employees_union_inertia_4), 
                                              digits = 3, include.ci = FALSE, #stars = numeric(0),
                                              custom.coef.map = list('union_ever'= " Union Ever ",
                                                                     'union_ever:size_100employees' = " Union Ever $ \\times $ Mine Size ",
                                                                     'union_ever_pre2011'= " Union Pre-2011 ",
                                                                     'union_ever_pre2011:size_100employees' = " Union Pre-2011 $ \\times $ Mine Size ",
                                                                     'size_100employees' = " Mine Size "),
                                              custom.model.names = c("(Traum Inj., E.)","(Traum. Inj., L.)", "(S\\&S Viol., E.)", "(S\\&S Viol., L.)"),
                                              caption = "Effect of Past Union Status on Nonunion Mine Safety",
                                              label = "irr_100employees_union_inertia")
irr_100employees_union_inertia_table
write.table(irr_100employees_union_inertia_table, file.path(outputdir, 'tables', 'irr_100employees_union_inertia_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

##----------##
# MINER Act violations, size_100employees
##----------##
irr_100employees_mineract_violations_1 <- extract(negbinirr(violations_miner_act ~ union*size_100employees 
                                                   + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                   + factor(year_quarter) + factor(district) 
                                                   + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                   data = filter(mine_panel_quarters, year >= 2007, year <= 2009)),
                                         include.deviance = FALSE,
                                         include.aic = FALSE,
                                         include.bic = FALSE)
irr_100employees_mineract_violations_2 <- extract(negbinirr(violations_miner_act ~ union*size_100employees 
                                                            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                            + factor(year_quarter) + factor(district) 
                                                            + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                            data = filter(mine_panel_quarters, year >= 2007, year <= 2011)),
                                                  include.deviance = FALSE,
                                                  include.aic = FALSE,
                                                  include.bic = FALSE)
irr_100employees_mineract_violations_3 <- extract(negbinirr(violations_miner_act ~ union*size_100employees 
                                                            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                            + factor(year_quarter) + factor(district) 
                                                            + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                            data = filter(mine_panel_quarters, year >= 2007, year <= 2013)),
                                                  include.deviance = FALSE,
                                                  include.aic = FALSE,
                                                  include.bic = FALSE)

irr_100employees_mineract_ss_violations_1 <- extract(negbinirr(ss_violations_miner_act ~ union*size_100employees 
                                                            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                            + factor(year_quarter) + factor(district) 
                                                            + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                            data = filter(mine_panel_quarters, year >= 2007, year <= 2009)),
                                                  include.deviance = FALSE,
                                                  include.aic = FALSE,
                                                  include.bic = FALSE)
irr_100employees_mineract_ss_violations_2 <- extract(negbinirr(ss_violations_miner_act ~ union*size_100employees 
                                                            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                            + factor(year_quarter) + factor(district) 
                                                            + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                            data = filter(mine_panel_quarters, year >= 2007, year <= 2011)),
                                                  include.deviance = FALSE,
                                                  include.aic = FALSE,
                                                  include.bic = FALSE)
irr_100employees_mineract_ss_violations_3 <- extract(negbinirr(ss_violations_miner_act ~ union*size_100employees 
                                                            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
                                                            + factor(year_quarter) + factor(district) 
                                                            + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
                                                            data = filter(mine_panel_quarters, year >= 2007, year <= 2013)),
                                                  include.deviance = FALSE,
                                                  include.aic = FALSE,
                                                  include.bic = FALSE)

irr_100employees_mineract_violations_table <- texreg(list(irr_100employees_mineract_violations_1, irr_100employees_mineract_violations_2, 
                                                 irr_100employees_mineract_violations_3, irr_100employees_mineract_ss_violations_1, 
                                                 irr_100employees_mineract_ss_violations_2, irr_100employees_mineract_ss_violations_3), 
                                            digits = 3, include.ci = FALSE, #stars = numeric(0),
                                            custom.coef.map = list('union'= " Union ",
                                                                   'size_100employees' = " Mine Size ",
                                                                   'union:size_100employees' = " Union $ \\times $ Mine Size "),
                                            custom.model.names = c("(Total, 07-09)","(Total, 07-11)","(Total, 07-13)",
                                                                   "(S\\&S, 07-09)","(S\\&S, 07-11)","(S\\&S, 07-13)"),
                                            caption = "Effect of Union Status on MINER Act Violation Frequency",
                                            label = "irr_100employees_mineract_violations")
irr_100employees_mineract_violations_table
write.table(irr_100employees_mineract_violations_table, file.path(outputdir, 'tables', 'irr_100employees_mineract_violations_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

