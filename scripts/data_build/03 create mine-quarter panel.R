# Create mine-quarter panel dataset 

# Header ------------------------------------------------------------------

rm(list = ls())

root <- getwd()
while(basename(root) != "coal-mining") root <- dirname(root)

source(file.path(root, "scripts", "header_script.R"))

# Globals and file paths --------------------------------------------------

source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

input_eia <- file.path(ddir, "Raw Cleaned")
output <- input_msha <- file.path(ddir, "Intermediate")

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
                subsidiary_indicator = ifelse(company_type_eia == "Operating Subsidiary", 1, 0),
                # 2012 EIA data seems to have some errors where 6 mines that are union mines in 2011 and 2013 are marked as nonunion in 2012
                # Internet for news articles about union status at these mines around 2012 returned no relevant results 
                union = ifelse((MINE_ID == 100851 & year == 2012) | 
                                 (MINE_ID == 4200121 & year == 2012) | 
                                 (MINE_ID == 4601537 & year == 2012) | 
                                 (MINE_ID == 4601816 & year == 2012) | 
                                 (MINE_ID == 4606618 & year == 2012) | 
                                 (MINE_ID == 4609152 & year == 2012), 
                               1, union),
                company_type_eia = ifelse("Indepedent Producer Operator", "Independent Producer Operator", company_type_eia)) %>%
  dplyr::ungroup() %T>%
  dplyr::glimpse()

eia_underground_mines <- eia_panel_cleaned %>%
  dplyr::filter(year >= 2000,
                year <= 2023) %>%
  dplyr::select(MINE_ID) %>%
  dplyr::distinct() %T>%
  dplyr::glimpse()

mine_panel <- msha_panel %>%
  dplyr::inner_join(eia_underground_mines,
                    by = "MINE_ID") %>%
  dplyr::inner_join(eia_panel_cleaned,
                   by = c("MINE_ID", "year")) %T>%
  dplyr::glimpse()






haven::write_dta(msha_mine_quarter_panel, file.path(output, "03.dta"))
saveRDS(msha_mine_quarter_panel, file.path(output, "03.rds"))










msha_panel_years <- read_csv(file.path(ddir, "cleaned", "msha_panel_years.csv"))
msha_panel_quarters <- read_csv(file.path(ddir, "cleaned", "msha_panel_quarters.csv"))

# Ln (Controller size): Log of controller size measure. Controller size measure is either 100 FTEs, 100 employees, or one million tons
controller_size_years <- msha_panel_years %>%
  group_by(CONTROLLER_ID, year) %>%
  summarize(controller_size_100FTEs = sum(size_100FTEs, na.rm = TRUE),
            ln_controller_size_100FTEs = log(controller_size_100FTEs),
            controller_size_100employees = sum(size_100employees, na.rm = TRUE),
            ln_controller_size_100employees = log(controller_size_100employees),
            controller_size_1milliontons = sum(size_1milliontons, na.rm = TRUE),
            ln_controller_size_1milliontons = log(controller_size_1milliontons))
msha_panel_years <- msha_panel_years %>%
  left_join(controller_size_years, by = c("CONTROLLER_ID", "year"))

controller_size_quarters <- msha_panel_quarters %>%
  group_by(CONTROLLER_ID, year, quarter) %>%
  summarize(controller_size_100FTEs = sum(size_100FTEs, na.rm = TRUE),
            ln_controller_size_100FTEs = log(controller_size_100FTEs),
            controller_size_100employees = sum(size_100employees, na.rm = TRUE),
            ln_controller_size_100employees = log(controller_size_100employees),
            controller_size_1milliontons = sum(size_1milliontons, na.rm = TRUE),
            ln_controller_size_1milliontons = log(controller_size_1milliontons))
msha_panel_quarters <- msha_panel_quarters %>%
  left_join(controller_size_quarters, by = c("CONTROLLER_ID", "year", "quarter"))

# Focusing on underground mines that extract bituminous coal; injury rate is injuries per 2,000 hours worked [compute violation rate similarly]
# (Morantz (2013))
# In analysis, drop any mine-quarters in which a mine reported zero coal production and/or zero hours worked
# (given by zero_total_production_quarter and zero_underground_production_quarter variables 
# => e.g., filter(zero_production_quarter == 0)
# (Morantz (2013))
mine_panel_years <- eia_df %>%
  full_join(msha_panel_years, by = c('MINE_ID', 'year')) %>%
  arrange(MINE_ID, year) %>%
  filter(type == "Underground",
         bituminous == 1,
         year >= 2000,
         year <= 2021) %>%
  mutate(fatality_rate = ifelse(zero_production_year == 0, 2000*(fatalities/labor_hours), 0),
         traumatic_injury_rate = ifelse(zero_production_year == 0, 2000*(traumatic_injuries/labor_hours), 0),
         nontraumatic_injury_rate = ifelse(zero_production_year == 0, 2000*(nontraumatic_injuries/labor_hours), 0),
         total_injury_rate = ifelse(zero_production_year == 0, 2000*(total_injuries/labor_hours), 0),
         
         violation_rate = ifelse(zero_production_year == 0, 2000*(violations/labor_hours), 0),
         ss_violation_rate = ifelse(zero_production_year == 0, 2000*(ss_violations/labor_hours), 0),
         violation_miner_act_rate = ifelse(zero_production_year == 0, 2000*(violations_miner_act/labor_hours), 0),
         ss_violation_miner_act_rate = ifelse(zero_production_year == 0, 2000*(ss_violations_miner_act/labor_hours), 0))

mine_panel_quarters <- mutate(eia_df, quarter = 1) %>%
  rbind(mutate(eia_df, quarter = 2)) %>%
  rbind(mutate(eia_df, quarter = 3)) %>%
  rbind(mutate(eia_df, quarter = 4)) %>%
  full_join(msha_panel_quarters, by = c('MINE_ID', 'year', 'quarter')) %>%
  arrange(MINE_ID, year, quarter) %>%
  filter(type == "Underground",
         bituminous == 1,
         year >= 2000,
         year <= 2021) %>%
  mutate(quarter_fraction = (as.numeric(quarter)-1.0)*0.25,
         year_quarter = as.numeric(year) + as.numeric(quarter_fraction),
         quarter_1 = ifelse(quarter == 1, 1, 0),
         quarter_2 = ifelse(quarter == 2, 1, 0),
         quarter_3 = ifelse(quarter == 3, 1, 0),
         quarter_4 = ifelse(quarter == 4, 1, 0),
         
         fatality_rate = ifelse(zero_production_quarter == 0, 2000*(fatalities/labor_hours), 0),
         traumatic_injury_rate = ifelse(zero_production_quarter == 0, 2000*(traumatic_injuries/labor_hours), 0),
         nontraumatic_injury_rate = ifelse(zero_production_quarter == 0, 2000*(nontraumatic_injuries/labor_hours), 0),
         total_injury_rate = ifelse(zero_production_quarter == 0, 2000*(total_injuries/labor_hours), 0),
         
         violation_rate = ifelse(zero_production_quarter == 0, 2000*(violations/labor_hours), 0),
         ss_violation_rate = ifelse(zero_production_quarter == 0, 2000*(ss_violations/labor_hours), 0),
         violation_miner_act_rate = ifelse(zero_production_quarter == 0, 2000*(violations_miner_act/labor_hours), 0),
         ss_violation_miner_act_rate = ifelse(zero_production_quarter == 0, 2000*(ss_violations_miner_act/labor_hours), 0))


# We define Labor Productivity as tons of coal produced divided by mine-worker hours 
# and trim the top 1% of firm-year observations.
# (I will trim the top 1% of mine-quarter observations)
# (Christensen et al. (2017))
# First make sure no duplicated mine-quarters and mine-years
if (nrow(mine_panel_quarters) == nrow(mine_panel_quarters %>% dplyr::select(MINE_ID, year, quarter) %>% distinct())) {
  mine_panel_quarters <- mine_panel_quarters %>%
    mutate(productivity_top1pct = ifelse(productivity >= quantile(mine_panel_quarters$productivity, .99, na.rm = TRUE)[[1]],
                                         1, 0))
  
  write_csv(mine_panel_quarters, file.path(ddir, "cleaned", "mine_panel_quarters.csv"))
} else {
  print('WARNING: DUPLICATE OBSERVATIONS')
}

if (nrow(mine_panel_years) == nrow(mine_panel_years %>% dplyr::select(MINE_ID, year) %>% distinct())) {
  mine_panel_years <- mine_panel_years %>%
    mutate(productivity_top1pct = ifelse(productivity >= quantile(mine_panel_years$productivity, .99, na.rm = TRUE)[[1]],
                                         1, 0))
  
  write_csv(mine_panel_years, file.path(ddir, "cleaned", "mine_panel_years.csv"))
} else {
  print('WARNING: DUPLICATE OBSERVATIONS')
}


time_trend_df <- msha_mine_quarter_panel %>%
  filter(year >= 2000,
         year <= 2023,
         zero_production_quarter == 0) %>%
  mutate(violations_nonminer_act = violations - violations_miner_act,
         ss_violations_nonminer_act = ss_violations - ss_violations_miner_act) %>%
  group_by(year_quarter) %>%
  summarize(total_injury_rate = 2000*(sum(total_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            traumatic_injury_rate = 2000*(sum(traumatic_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            violation_rate = 2000*(sum(violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            ss_violation_rate = 2000*(sum(ss_violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            labor_hours = sum(labor_hours, na.rm = TRUE),
            total_avg_employee_count = sum(avg_employee_count, na.rm = TRUE),
            coal_production_tons = sum(coal_production_tons, na.rm = TRUE),
            productivity = 2000*(coal_production_tons/labor_hours),
            operator_changes = sum(operator_change, na.rm = TRUE),
            controller_changes = sum(controller_change, na.rm = TRUE),
            active_mines = n())




