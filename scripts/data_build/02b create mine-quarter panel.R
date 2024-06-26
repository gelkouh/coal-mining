# Last updated: Apr 5, 2023

root <- getwd()
while(basename(root) != "coal-mining") { # this is the name of your project directory you want to use
  root <- dirname(root)
}
source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

library(tidyverse)

eia_df <- read_csv(file.path(ddir, "cleaned", "coalpublic_1993_2021_mines.csv")) %>%
  rename(MINE_ID = `MSHA ID`,
         mine_name_eia = `Mine Name`,
         year = Year,
         status_eia = `Mine Status`,
         type = `Mine Type`,
         union_code = `Union Code`,
         company_type = `Company Type`,
         production_tons_eia = `Production (short tons)`) %>%
  dplyr::select(MINE_ID, year, mine_name_eia, status_eia, type, union_code, union, company_type, production_tons_eia) %>%
  mutate(MINE_ID = as.double(MINE_ID)) %>%
  filter(!is.na(MINE_ID)) %>%
  group_by(MINE_ID) %>%
  arrange(MINE_ID, year) %>%
  mutate(union_change_direction = union - lag(union),
         union_change_year = ifelse(union_change_direction == 0, 0, 1),
         union_change_direction = ifelse(is.na(union_change_direction), 0, union_change_direction),
         union_change_year = ifelse(is.na(union_change_year), 0, union_change_year),
         subsidiary_indicator = ifelse(company_type == "Operating Subsidiary", 1, 0),
         # 2012 EIA data seems to have some errors where 6 mines that are union mines in 2011 and 2013 are marked as nonunion in 2012
         # Internet for news articles about union status at these mines around 2012 returned no relevant results 
         union = ifelse((MINE_ID == 100851 & year == 2012) | 
                          (MINE_ID == 4200121 & year == 2012) | 
                          (MINE_ID == 4601537 & year == 2012) | 
                          (MINE_ID == 4601816 & year == 2012) | 
                          (MINE_ID == 4606618 & year == 2012) | 
                          (MINE_ID == 4609152 & year == 2012), 
                        1, union))

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

