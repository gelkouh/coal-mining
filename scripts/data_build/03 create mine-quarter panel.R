# Create mine-quarter panel dataset 

# Header ------------------------------------------------------------------

rm(list = ls())

root <- getwd()
while(basename(root) != "coal-mining") root <- dirname(root)

source(file.path(root, "scripts", "header_script.R"))

# Globals and file paths --------------------------------------------------

min_year <- 2000
max_year <- 2022

source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

input_eia <- file.path(ddir, "Raw Cleaned")
input_msha <- file.path(ddir, "Intermediate")
input_capiq <- file.path(ddir, "Capital IQ")
output <- file.path(ddir, "Output")


# Capital IQ coal seam height data ----------------------------------------

capiq_coalseam_raw <- readxl::read_excel(file.path(input_capiq, "SPGlobal_Export_7-30-2024_6704c458-c6c6-4499-b2ea-65c92f11cd62.xls"),
                                         skip = 3, 
                                         col_types = "text", 
                                         na = "NA") %T>%
  dplyr::glimpse()

for (i in 4:ncol(capiq_coalseam_raw)) {
  raw_yq <- capiq_coalseam_raw[1, i][[1]]
  quarter <- switch(stringr::str_sub(raw_yq, 1, 2),
                    "03" = 1,
                    "06" = 2,
                    "09" = 3,
                    "12" = 4)
  
  year_short <- stringr::str_sub(raw_yq, 4, 5)
  if (stringr::str_sub(year_short, 1, 1) == "9") year <- as.numeric(paste0("19", year_short))
  if (stringr::str_sub(year_short, 1, 1) != "9") year <- as.numeric(paste0("20", year_short))
  
  year_quarter <- paste0("y", year, "q", quarter)
  
  colnames(capiq_coalseam_raw)[i] <- as.character(year_quarter)
}

capiq_coalseam_long <- capiq_coalseam_raw %>%
  dplyr::filter(!is.na(MSHA_MINE_ID)) %>%
  dplyr::select(-NAME, -MINE_UNIQUE_IDENTIFIER) %>%
  data.table::melt(id = "MSHA_MINE_ID", variable.name = "year_quarter", value.name = "seam_height_in") %>%
  dplyr::mutate(seam_height_in = as.numeric(seam_height_in),
                MINE_ID = as.double(MSHA_MINE_ID),
                year = as.numeric(stringr::str_sub(year_quarter, 2, 5)),
                quarter = as.numeric(stringr::str_sub(year_quarter, 7, 7))) %>%
  dplyr::filter(!is.na(seam_height_in)) %>%
  dplyr::select(MINE_ID, year, quarter, seam_height_in) %T>%
  dplyr::glimpse()

# Combine MSHA and EIA union status data ----------------------------------

msha_panel <- readRDS(file.path(input_msha, "02a msha mine-quarter panel.rds")) %T>%
  dplyr::glimpse()

eia_panel <- readr::read_csv(file.path(input_eia, "01a eia annual mine union status, 1993-2022.csv")) %T>%
                               dplyr::glimpse()

eia_panel_cleaned <- eia_panel %>%
  dplyr::rename(MINE_ID = msha_id) %>%
  dplyr::mutate(MINE_ID = as.double(MINE_ID)) %>%
  dplyr::filter(!is.na(MINE_ID),
                mine_type_eia == "Underground") %>%
  dplyr::group_by(MINE_ID) %>%
  dplyr::arrange(MINE_ID, year) %>%
  dplyr::mutate(union_change_direction = union - lag(union),
                union_change_year = ifelse(union_change_direction == 0, 0, 1),
                union_change_direction = ifelse(is.na(union_change_direction), 0, union_change_direction),
                union_change_year = ifelse(is.na(union_change_year), 0, union_change_year),
                subsidiary_indicator = company_type_eia == "Operating Subsidiary" & !is.na(company_type_eia),
                # 2012 EIA data seems to have some errors where 6 mines that are union mines in 2011 and 2013 are marked as nonunion in 2012
                # Internet for news articles about union status at these mines around 2012 returned no relevant results 
                union = ifelse((MINE_ID == 100851 & year == 2012) | 
                                 (MINE_ID == 4200121 & year == 2012) | 
                                 (MINE_ID == 4601537 & year == 2012) | 
                                 (MINE_ID == 4601816 & year == 2012) | 
                                 (MINE_ID == 4606618 & year == 2012) | 
                                 (MINE_ID == 4609152 & year == 2012), 
                               1, union),
                company_type_eia = ifelse(company_type_eia == "Indepedent Producer Operator", "Independent Producer Operator", company_type_eia)) %>%
  dplyr::ungroup() %T>%
  dplyr::glimpse()

eia_underground_mines <- eia_panel_cleaned %>%
  dplyr::filter(year >= min_year,
                year <= max_year) %>%
  dplyr::select(MINE_ID) %>%
  dplyr::distinct() %T>%
  dplyr::glimpse()

# Ln (Controller size): Log of controller size measure. Controller size measure is either 100 FTEs, 100 employees, or one million tons
# In analysis, drop any mine-quarters in which a mine reported zero coal production and/or zero hours worked
# => i.e., filter(zero_production_quarter == 0)
# (Morantz (2013))
mine_panel <- msha_panel %>%
  dplyr::filter(year >= min_year,
                year <= max_year) %>% 
  dplyr::inner_join(eia_underground_mines,
                    by = "MINE_ID") %>%
  dplyr::left_join(eia_panel_cleaned,
                   by = c("MINE_ID", "year")) %>%
  dplyr::left_join(capiq_coalseam_long,
                   by = c("MINE_ID", "year", "quarter")) %>%
  dplyr::mutate(controller_size_100FTEs = sum(size_100FTEs, na.rm = TRUE),
                ln_controller_size_100FTEs = log(controller_size_100FTEs),
                controller_size_100employees = sum(size_100employees, na.rm = TRUE),
                ln_controller_size_100employees = log(controller_size_100employees),
                controller_size_1milliontons = sum(size_1milliontons, na.rm = TRUE),
                ln_controller_size_1milliontons = log(controller_size_1milliontons),
                .by = c("CONTROLLER_ID", "year", "quarter")) %>%
  dplyr::arrange(MINE_ID, year, quarter) %>%
  dplyr::mutate(penalty_points_prev4qtrs = dplyr::lag(penalty_points, n = 1L) + 
                  dplyr::lag(penalty_points, n = 2L) +
                  dplyr::lag(penalty_points, n = 3L) +
                  dplyr::lag(penalty_points, n = 4L),
                penalty_points_prev4qtrs = ifelse(is.na(penalty_points_prev4qtrs),
                                                  (4/3) * (dplyr::lag(penalty_points, n = 1L) + 
                                                    dplyr::lag(penalty_points, n = 2L) +
                                                    dplyr::lag(penalty_points, n = 3L)),
                                                  penalty_points_prev4qtrs),
                penalty_points_prev4qtrs = ifelse(is.na(penalty_points_prev4qtrs),
                                                  2 * (dplyr::lag(penalty_points, n = 1L) + 
                                                    dplyr::lag(penalty_points, n = 2L)),
                                                  penalty_points_prev4qtrs),
                penalty_points_prev4qtrs = ifelse(is.na(penalty_points_prev4qtrs),
                                                  4 * dplyr::lag(penalty_points, n = 1L),
                                                  penalty_points_prev4qtrs),
                penalty_points_prev4qtrs = ifelse(is.na(penalty_points_prev4qtrs),
                                                  4 * penalty_points,
                                                  penalty_points_prev4qtrs),
                .by = MINE_ID) %>%
  dplyr::mutate(quarter_fraction = (as.numeric(quarter)-1.0)*0.25,
                year_quarter = as.numeric(year) + as.numeric(quarter_fraction),
                quarter_1 = ifelse(quarter == 1, 1, 0),
                quarter_2 = ifelse(quarter == 2, 1, 0),
                quarter_3 = ifelse(quarter == 3, 1, 0),
                quarter_4 = ifelse(quarter == 4, 1, 0)) %T>%
  dplyr::glimpse()

if (nrow(mine_panel) == nrow(mine_panel %>% dplyr::select(MINE_ID, year, quarter) %>% dplyr::distinct())) {
  haven::write_dta(mine_panel, file.path(output, "03 mine-quarter panel.dta"))
  saveRDS(mine_panel, file.path(output, "03 mine-quarter panel.rds"))
} else {
  print('WARNING: DUPLICATE OBSERVATIONS')
}

# Create union status and mine safety analysis panel ----------------------

# Focusing on underground mines that extract bituminous coal; injury rate is injuries per 2,000 hours worked [compute violation rate similarly]
# (Morantz (2013))
# We define Labor Productivity as tons of coal produced divided by mine-worker hours 
# and trim the top 1% of firm-year observations.
# (I will trim the top 1% of mine-quarter observations)
# (Christensen et al. (2017))
mine_panel_analysis <- mine_panel %>%
  dplyr::filter(mine_type_eia == "Underground",
                zero_production_quarter == 0) %>%
  dplyr::mutate(productivity_top1pct = ifelse(productivity >= quantile(mine_panel$productivity, .99, na.rm = TRUE)[[1]],
                                              1, 0),
                fatality_rate = 2000*(fatalities/labor_hours),
                traumatic_injury_rate = 2000*(traumatic_injuries/labor_hours),
                nontraumatic_injury_rate = 2000*(nontraumatic_injuries/labor_hours),
                total_injury_rate = 2000*(total_injuries/labor_hours),
                violation_rate = 2000*(violations/labor_hours),
                ss_violation_rate = 2000*(ss_violations/labor_hours),
                max_possible_active_qtrs = 23*4) %>%
  dplyr::mutate(MINE_ID_active_qtrs = n(),
                MINE_ID_active_70pct = ((MINE_ID_active_qtrs / max_possible_active_qtrs) >= .7),
                MINE_ID_active_75pct = ((MINE_ID_active_qtrs / max_possible_active_qtrs) >= .75),
                MINE_ID_active_80pct = ((MINE_ID_active_qtrs / max_possible_active_qtrs) >= .8),
                MINE_ID_active_90pct = ((MINE_ID_active_qtrs / max_possible_active_qtrs) >= .9),
                MINE_ID_active_100pct = ((MINE_ID_active_qtrs / max_possible_active_qtrs) == 1),
                .by = MINE_ID) %T>%
  dplyr::glimpse() 

haven::write_dta(mine_panel_analysis, file.path(output, "03 mine-quarter panel, union safety analysis.dta"))
saveRDS(mine_panel_analysis, file.path(output, "03 mine-quarter panel, union safety analysis.rds"))

for (time_trend_agg in c("union status", "overall")) {
  if (time_trend_agg == "union status") agg_vars <- c("year_quarter", "union")
  if (time_trend_agg == "overall") agg_vars <- c("year_quarter")
  
  time_trend <- mine_panel_analysis %>%
    dplyr::summarize(fatalities = sum(fatalities),
                     traumatic_injuries = sum(traumatic_injuries),
                     total_injuries = sum(total_injuries),
                     violations = sum(violations),
                     ss_violations = sum(violations),
                     operator_changes = sum(operator_change, na.rm = TRUE),
                     controller_changes = sum(controller_change, na.rm = TRUE),
                     coal_production_tons = sum(coal_production_tons),
                     labor_hours = sum(labor_hours),
                     total_avg_employee_count = sum(avg_employee_count),
                     active_mines = n(),
                     .by = agg_vars) %>%
    dplyr::mutate(fatality_rate = 2000*(fatalities/labor_hours),
                  traumatic_injury_rate = 2000*(traumatic_injuries/labor_hours),
                  total_injury_rate = 2000*(total_injuries/labor_hours),
                  violation_rate = 2000*(violations/labor_hours),
                  ss_violation_rate = 2000*(ss_violations/labor_hours)) %T>%
    dplyr::glimpse()
  
  haven::write_dta(time_trend, file.path(output, stringr::str_glue("03 {time_trend_agg} trends.dta")))
  saveRDS(time_trend, file.path(output, stringr::str_glue("03 {time_trend_agg} trends.rds")))
}
