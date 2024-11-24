# Clean MSHA data and create mine-quarter panel dataset

# Header ------------------------------------------------------------------

rm(list = ls())

root <- getwd()
while(basename(root) != "coal-mining") root <- dirname(root)

source(file.path(root, "scripts", "header_script.R"))

# Globals and file paths --------------------------------------------------

source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

input <- file.path(ddir, "Raw Cleaned")
output <- file.path(ddir, "Intermediate")

# 1 accident injuries -----------------------------------------------------

accidents_1_raw <- readRDS(file.path(input, "01b msha 1 accident injuries.rds")) %T>%
  dplyr::glimpse()
  
accidents_1 <- accidents_1_raw  %>%
  filter(SUBUNIT_CD == 1) %>%
  rename(year = CAL_YR,
         quarter = CAL_QTR) %>%
  mutate(fatality = ifelse(DEGREE_INJURY_CD == "01", 1, 0),
         traumatic_injury = ifelse(NATURE_INJURY_CD %in% c("100",
                                                           "220",
                                                           "200",
                                                           "320",
                                                           "180",
                                                           "130",
                                                           "301",
                                                           "120",
                                                           "360",
                                                           "302",
                                                           "170") |
                                     fatality == 1,
                                   1, 0),
         nontraumatic_injury = ifelse(traumatic_injury == 0, 1, 0),
         injury = 1,
         lost_workday_injury = DAYS_LOST > 0) %>%
  summarize(fatalities = sum(fatality, na.rm = TRUE),
            traumatic_injuries = sum(traumatic_injury, na.rm = TRUE),
            nontraumatic_injuries = sum(nontraumatic_injury, na.rm = TRUE),
            total_injuries = sum(injury, na.rm = TRUE),
            lost_workday_injuries = sum(lost_workday_injury, na.rm = TRUE),
            .by = c(MINE_ID, year, quarter)) %>%
  arrange(MINE_ID, year, quarter) %T>%
  dplyr::glimpse()

# 8 Controller Operator History -------------------------------------------

controllers_operators_8_raw <- readRDS(file.path(input, "01b msha 8 controller operator history.rds")) %T>%
  dplyr::glimpse()

# Controllers
controller_id_names <- controllers_operators_8_raw %>%
  dplyr::select(CONTROLLER_ID, CONTROLLER_NAME) %>%
  dplyr::distinct() %>%
  dplyr::summarize(CONTROLLER_NAMES = paste0(CONTROLLER_NAME, collapse = "; "),
                   .by = c(CONTROLLER_ID)) %T>%
  dplyr::glimpse()

controller_id_types <- controllers_operators_8_raw %>%
  dplyr::select(CONTROLLER_ID, CONTROLLER_TYPE) %>%
  dplyr::distinct() %>%
  dplyr::summarize(CONTROLLER_TYPES = paste0(CONTROLLER_TYPE, collapse = "; "),
                   .by = c(CONTROLLER_ID)) %>%
  dplyr::mutate(CONTROLLER_TYPES = ifelse(CONTROLLER_TYPES == "COMPANY; PERSON", 
                                          "PERSON; COMPANY", 
                                          CONTROLLER_TYPES)) %T>%
  dplyr::glimpse()

controller_history_long <- controllers_operators_8_raw %>%
  dplyr::filter(COAL_METAL_IND == "C") %>%
  dplyr::mutate(start_date = mdy(CONTROLLER_START_DT),
                start_year = year(start_date), 
                start_quarter = quarter(start_date),
                start_quarter_fraction = (as.numeric(start_quarter)-1.0)*0.25,
                start_year_quarter = as.numeric(start_year) + as.numeric(start_quarter_fraction)) %>%
  dplyr::arrange(MINE_ID, start_date) %>%
  dplyr::group_by(MINE_ID, start_year_quarter) %>%
  dplyr::slice_tail() %>%
  dplyr::ungroup() %>%
  dplyr::select(MINE_ID, CONTROLLER_ID, start_year_quarter)

controller_history_panel_quarters <- expand.grid(start_year_quarter = seq(from = min(controller_history_long$start_year_quarter), to = 2024.00, by = 0.25),
                                                 MINE_ID = unique(controller_history_long$MINE_ID),
                                                 stringsAsFactors = FALSE) %>%
  dplyr::left_join(controller_history_long,
                   by = c("MINE_ID", "start_year_quarter")) %>% 
  dplyr::group_by(MINE_ID) %>% 
  tidyr::fill(c("CONTROLLER_ID")) %>%
  dplyr::filter(!is.na(CONTROLLER_ID)) %>%
  as.data.frame() %>%
  dplyr::mutate(controller_change = ifelse(MINE_ID == lag(MINE_ID) & CONTROLLER_ID != lag(CONTROLLER_ID),
                                           1, 0),
                start_year = floor(start_year_quarter)) %>%
  dplyr::filter(start_year >= 2000) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(controller_id_names, 
                   by = "CONTROLLER_ID") %>%
  dplyr::left_join(controller_id_types, 
                   by = "CONTROLLER_ID") %T>%
  dplyr::glimpse()

# Operators
# Age of mine defined as years since the first operator began work at the mine (top censored at 1970)
operator_mine_age <- controllers_operators_8_raw %>%
  dplyr::filter(COAL_METAL_IND == "C") %>%
  dplyr::mutate(start_date = mdy(OPERATOR_START_DT),
                start_year = year(start_date)) %>%
  dplyr::summarize(first_year = min(start_year, na.rm = TRUE),
                   .by = c(MINE_ID)) %>%
  dplyr::mutate(first_year = ifelse(first_year < 1970, 1970, first_year)) %T>%
  dplyr::glimpse()

operator_id_names <- controllers_operators_8_raw %>%
  dplyr::select(OPERATOR_ID, OPERATOR_NAME) %>%
  dplyr::distinct() %>%
  dplyr::summarize(OPERATOR_NAMES = paste0(OPERATOR_NAME, collapse = "; "),
                   .by = c(OPERATOR_ID)) %T>%
  dplyr::glimpse()

operator_history_long <- controllers_operators_8_raw %>%
  dplyr::filter(COAL_METAL_IND == "C") %>%
  dplyr::mutate(start_date = mdy(OPERATOR_START_DT),
                start_year = year(start_date), 
                start_quarter = quarter(start_date),
                start_quarter_fraction = (as.numeric(start_quarter)-1.0)*0.25,
                start_year_quarter = as.numeric(start_year) + as.numeric(start_quarter_fraction)) %>%
  dplyr::arrange(MINE_ID, start_date) %>%
  dplyr::group_by(MINE_ID, start_year_quarter) %>%
  dplyr::slice_tail() %>%
  dplyr::ungroup() %>%
  dplyr::select(MINE_ID, OPERATOR_ID, start_year_quarter) %T>%
  dplyr::glimpse()

operator_history_panel_quarters <- expand.grid(start_year_quarter = seq(from = min(operator_history_long$start_year_quarter), to = 2024.00, by = 0.25),
                                               MINE_ID = unique(operator_history_long$MINE_ID),
                                               stringsAsFactors = FALSE) %>%
  dplyr::left_join(operator_history_long,
                   by = c("MINE_ID", "start_year_quarter")) %>% 
  dplyr::group_by(MINE_ID) %>% 
  tidyr::fill(c("OPERATOR_ID")) %>%
  dplyr::filter(!is.na(OPERATOR_ID)) %>%
  as.data.frame() %>%
  dplyr::mutate(operator_change = ifelse(MINE_ID == lag(MINE_ID) & OPERATOR_ID != lag(OPERATOR_ID),
                                         1, 0),
                start_year = floor(start_year_quarter)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(start_year >= 2000) %>%
  dplyr::left_join(operator_id_names, 
                   by = "OPERATOR_ID") %T>%
  dplyr::glimpse()

controllers_operators_8 <- controller_history_panel_quarters %>%
  dplyr::full_join(operator_history_panel_quarters,
                   by = c("MINE_ID", "start_year_quarter", "start_year")) %>%
  dplyr::mutate(start_quarter = start_year_quarter - start_year,
                year = start_year, 
                quarter = ifelse(start_quarter == 0, 1,
                                 ifelse(start_quarter == 0.25, 2,
                                        ifelse(start_quarter == 0.5, 3,
                                               4)))) %>%
  dplyr::select(-start_year_quarter, -start_year, -start_quarter) %>%
  dplyr::left_join(operator_mine_age, 
                   by = "MINE_ID") %>%
  dplyr::mutate(mine_age = year - first_year) %T>%
  dplyr::glimpse()

# 10 employment production data set (quarterly) ---------------------------

minesprodquarterly_10_raw <- readRDS(file.path(input, "01b msha 10 employment production data set (quarterly).rds")) %T>%
  dplyr::glimpse()

minesprodquarterly_10_subunits <- minesprodquarterly_10_raw %>%
  dplyr::rename(year = CAL_YR,
         quarter = CAL_QTR) %>%
  dplyr::select(year, quarter, MINE_ID, SUBUNIT_CD) %>%
  dplyr::distinct() %>%
  dplyr::mutate(one = 1) %>%
  reshape2::dcast(year + quarter + MINE_ID ~ paste0("subunit_", SUBUNIT_CD),
                  value.var = "one") %>%
  dplyr::mutate_all(~coalesce(.,0)) %T>%
  dplyr::glimpse()

minesprodquarterly_10 <- minesprodquarterly_10_raw %>%
  dplyr::rename(year = CAL_YR,
                quarter = CAL_QTR) %>%
  dplyr::filter(SUBUNIT_CD == 1) %>%
  dplyr::summarize(labor_hours = sum(HOURS_WORKED, na.rm = TRUE),
                   coal_production_tons = sum(COAL_PRODUCTION, na.rm = TRUE),
                   avg_employee_count = sum(AVG_EMPLOYEE_CNT, na.rm = TRUE),
                   .by = c(MINE_ID, year, quarter)) %>%
  dplyr::mutate(zero_production_quarter = (labor_hours == 0 | coal_production_tons == 0),
                FTEs = labor_hours/500,
                productivity = ifelse(zero_production_quarter == 1, 0, 2000*(coal_production_tons/labor_hours)),
                size_100FTEs = FTEs/100,
                size_100employees = avg_employee_count/100,
                size_1milliontons = coal_production_tons/1000000) %>%
  dplyr::left_join(minesprodquarterly_10_subunits,
                   by = c("MINE_ID", "year", "quarter")) %T>%
  dplyr::glimpse()

# 17 violations data set --------------------------------------------------

violations_17_raw <- readRDS(file.path(input, "01b msha 17 violations data set.rds")) %T>%
  dplyr::glimpse()

violations_17_int <- violations_17_raw %>%
  dplyr::mutate(underground = (MINE_TYPE == "Underground"),
                inspection_date = lubridate::mdy(INSPECTION_END_DT),
                violation_date = lubridate::mdy(VIOLATION_OCCUR_DT),
                year = lubridate::year(violation_date),
                quarter = lubridate::quarter(violation_date),
                ss_violation = (SIG_SUB == "Y")) %>%
  dplyr::filter(!is.na(MINE_ID),
                underground == 1) %T>%
  dplyr::glimpse()

violations_17 <- violations_17_int %>%
  dplyr::summarize(violations = n(), 
                   ss_violations = sum(ss_violation, na.rm = TRUE),
                   violations_amt_due = sum(AMOUNT_DUE, na.rm = TRUE),
                   violations_amt_paid = sum(AMOUNT_PAID, na.rm = TRUE),
                   .by = c(MINE_ID, year, quarter)) %T>%
  dplyr::glimpse()

# 13 mines data set -------------------------------------------------------

mines_13_raw <- readRDS(file.path(input, "01b msha 13 mines data set.rds")) %T>%
  dplyr::glimpse()

mines_13 <- mines_13_raw %>%
  dplyr::filter(PRIMARY_CANVASS_CD == "2") %>%
  dplyr::rename(district = DISTRICT,
                msha_office_code = OFFICE_CD,
                inspector_office_dist_miles = MILES_FROM_OFFICE,
                avg_mine_height_in = AVG_MINE_HEIGHT,
                longitude = LONGITUDE,
                latitude = LATITUDE,
                state = STATE,
                county_fips = FIPS_CNTY_CD,
                county = FIPS_CNTY_NM) %>%
  dplyr::mutate(bituminous = 1,
                pillar_recovery_used = ifelse(PILLAR_RECOVERY_USED == "Y", 1, 0),
                part_48_training_restriction = ifelse(PART48_TRAINING == "Y", 1, 0)) %>%
  dplyr::select(MINE_ID, bituminous, inspector_office_dist_miles, 
                avg_mine_height_in, longitude, latitude, pillar_recovery_used, 
                part_48_training_restriction, msha_office_code, district,
                county, county_fips, state) %T>%
  dplyr::glimpse()


# 19 assessed violations --------------------------------------------------

assessed_violations_19_raw <- readRDS(file.path(input, "01b msha 19 assessed violations.rds")) %T>%
  dplyr::glimpse()

# Thousands of penalty points in the previous calendar year
assessed_violations_19 <- assessed_violations_19_raw %>%
  dplyr::mutate(occurrence_date = lubridate::mdy(OCCURRENCE_DT),
                year = lubridate::year(occurrence_date),
                quarter = lubridate::quarter(occurrence_date)) %>%
  dplyr::summarize(penalty_points = sum(PENALTY_POINTS, na.rm = TRUE),
                   .by = c("MINE_ID", "year", "quarter")) %>%
  dplyr::mutate(penalty_points = penalty_points / 1000) %T>%
  dplyr::glimpse()

# Create mine-quarter panel -----------------------------------------------

# The sample used includes all underground bituminous mine-quarters from 1993 to 2010 with positive coal production and positive hours worked.
msha_mine_quarter_panel_unfiltered <- mines_13 %>%
  dplyr::inner_join(controllers_operators_8,
                    by = "MINE_ID") %>%
  dplyr::filter(year >= 2000,
                year <= 2023) %>%
  dplyr::left_join(accidents_1,
                   by = c("MINE_ID", "year", "quarter")) %>%
  dplyr::left_join(violations_17,
                   by = c("MINE_ID", "year", "quarter")) %>%
  dplyr::left_join(assessed_violations_19,
                   by = c("MINE_ID", "year", "quarter")) %>%
  dplyr::left_join(minesprodquarterly_10,
                   by = c("MINE_ID", "year", "quarter")) %>%
  dplyr::mutate(pos_production_quarter = (coal_production_tons > 0 & labor_hours > 0)) %T>%
  dplyr::glimpse()

positive_production_mines <- msha_mine_quarter_panel_unfiltered %>%
  dplyr::summarize(pos_production_quarters = sum(pos_production_quarter, na.rm = TRUE),
                   .by = "MINE_ID") %>%
  dplyr::filter(pos_production_quarters >= 1) %>%
  dplyr::select(MINE_ID) %T>%
  dplyr::glimpse()

msha_mine_quarter_panel <- msha_mine_quarter_panel_unfiltered %>%
  dplyr::inner_join(positive_production_mines,
                    by = "MINE_ID") %>%
  dplyr::mutate(zero_production_quarter = !pos_production_quarter | is.na(pos_production_quarter),
                fatalities = NA_to_0(fatalities),
                traumatic_injuries = NA_to_0(traumatic_injuries),
                total_injuries = NA_to_0(total_injuries),
                violations = NA_to_0(violations),
                ss_violations = NA_to_0(ss_violations),
                lost_workday_injuries = NA_to_0(lost_workday_injuries),
                penalty_points = NA_to_0(penalty_points)) %>%
  dplyr::select(-pos_production_quarter) %T>%
  dplyr::glimpse()

saveRDS(msha_mine_quarter_panel, file.path(output, "02a msha mine-quarter panel.rds"))
