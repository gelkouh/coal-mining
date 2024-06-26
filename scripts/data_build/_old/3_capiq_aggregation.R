# Last updated: Feb 16, 2023

root <- getwd()
while(basename(root) != "coal-mining") { # this is the name of your project directory you want to use
  root <- dirname(root)
}
source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

library(tidyverse)
library(lubridate)
library(reshape2)
library(readxl)

# FRED Inflation Adjustment data
inflation_adj <- read_excel(file.path(ddir, "FRED", "PCU212112212112.XLS"), skip = 10, col_types = c("date","numeric")) %>%
  rename(date = observation_date,
         inflation_index = PCU212112212112) %>%
  mutate(date = ymd(date), 
         year = year(date),
         month = month(date))

# S&P Capital IQ mine data
  
##-----##
# MINE DEALS
##-----##

metals_mining_properties_deals <- read_excel(file.path(ddir, "capiq", "metals_mining_properties_deals_01202023.xls"), skip = 3, col_types = "text") %>%
  filter(!is.na(PROP_ID), (MINE_TYPE1 == "Underground" | MINE_TYPE2 == "Underground" | MINE_TYPE3 == "Underground")) %>%
  select(!c(`SNL_DEAL_KEY...16`,
            `SNL_DEAL_KEY...17`,
            `SNL_DEAL_KEY...18`,
            `SNL_DEAL_KEY...19`))

# Export and match by hand 80 mines 
# (using lat-long of mine location, current ownership and operator details, and common mine names)
write_csv(metals_mining_properties_deals, file.path(ddir, "cleaned", "temp", "capiq_mining_properties_deals.csv"))

# NOTE: using "controller" variable from MSHA for now to do analysis when ownership of a mine changes

##-----##
# MINE CONTRACTS AND POWERPLANT DATA (Monthly Fuel Deliveries) - done
##-----##

# Import data
contract_col_types <- c("text","text","text","text","text","text","text","date","text","text","text","text","text","text","text","text","text","text","text")
contracts <- read_excel(file.path(ddir, "capiq", "SPGlobal_Export_2002_2003.xls"), skip = 3, col_types = contract_col_types)
  
years_list <- c("2003_2004", "2005_2006", "2007_2008", "2009_2010", "2011_2013", "2014_2017", "2018_2022")
for (years in years_list) {
  print(years)
  temp <- read_excel(file.path(ddir, "capiq", paste0("SPGlobal_Export_", years, ".xls")), skip = 3, col_types = contract_col_types)
  
  
  contracts <- contracts %>%
    bind_rows(temp) %>%
    filter(!is.na(KEY_MINE_ID),
           FUEL_TYPE_DETAIL == "Bituminous Coal") %>%
    distinct()
  
  rm(temp)
}

# Can only use contracts with both price and coal quantity data AND MINE_ID: 136267 contracts
contracts_semi_cleaned <- contracts %>%
  mutate(DLVY_DATE = ymd(DLVY_DATE),
         year = year(DLVY_DATE), 
         month = month(DLVY_DATE)) %>%
  left_join(inflation_adj, by = c("year", "month")) %>%
  filter(!is.na(QTY_COAL_PURCH), 
         QTY_COAL_PURCH != "NA",
         !is.na(PRICE_PER_TON),
         PRICE_PER_TON != "NA") %>%
  mutate(QTY_COAL_PURCH = as.numeric(QTY_COAL_PURCH), 
         PRICE_PER_TON = 100*as.numeric(PRICE_PER_TON)/inflation_index,
         QTY_COAL_PURCH_x_PRICE_PER_TON = QTY_COAL_PURCH*PRICE_PER_TON,
         ln_PRICE_PER_TON = log(PRICE_PER_TON), 
         ln_QTY_COAL_PURCH = log(QTY_COAL_PURCH),
         year = as.numeric(DLVY_YR),
         KEY_MINE_ID = as.numeric(KEY_MINE_ID)) %>%
  rename(contract_type = FUEL_CONTRACT_TYPE) %>%
  filter(!is.na(KEY_MINE_ID))

nrow(contracts_semi_cleaned)

# Checking negative price elasticity of demand
lm1i <- lm(ln_QTY_COAL_PURCH ~ ln_PRICE_PER_TON, data = contracts_semi_cleaned)
lm1ii <- lm(ln_QTY_COAL_PURCH ~ ln_PRICE_PER_TON*factor(contract_type), data = contracts_semi_cleaned)
lm1iii <- lm(ln_QTY_COAL_PURCH ~ ln_PRICE_PER_TON*factor(year), data = filter(contracts_semi_cleaned, contract_type == "Contract"))
summary(lm1i)
summary(lm1ii)
summary(lm1iii)

# Aggregate by mine 
contracts_year_mine <- contracts_semi_cleaned %>%
  group_by(KEY_MINE_ID, year, contract_type) %>%
  summarize(contract_quantity_yr = sum(QTY_COAL_PURCH, na.rm = TRUE), 
            QTY_COAL_PURCH_x_PRICE_PER_TON_sum = sum(QTY_COAL_PURCH_x_PRICE_PER_TON, na.rm = TRUE)) %>%
  mutate(contract_price_per_ton_wtavg_yr = QTY_COAL_PURCH_x_PRICE_PER_TON_sum/contract_quantity_yr,
         ln_contract_price_per_ton_wtavg_yr = log(contract_price_per_ton_wtavg_yr),
         ln_contract_quantity_yr = log(contract_quantity_yr)) %>%
  dplyr::select(!QTY_COAL_PURCH_x_PRICE_PER_TON_sum)

# Checking negative price elasticity of demand
lm2i <- lm(ln_contract_quantity_yr ~ ln_contract_price_per_ton_wtavg_yr, data = contracts_year_mine)
lm2ii <- lm(ln_contract_quantity_yr ~ ln_contract_price_per_ton_wtavg_yr*factor(contract_type), data = contracts_year_mine)
lm2iii <- lm(ln_contract_quantity_yr ~ ln_contract_price_per_ton_wtavg_yr*factor(year), data = filter(contracts_year_mine, contract_type == 'Contract'))
summary(lm2i)
summary(lm2ii)
summary(lm2iii)

##-----##
# Seam height - done
##-----##

seamheight <- read_excel(file.path(ddir, "capiq", "SPGlobal_Export_seam_height.xls"), skip = 3, col_types = "text", na = "NA") %>%
  filter(!is.na(MSHA_MINE_ID))

for (i in 4:24) {
  year <- 2026-i
  colnames(seamheight)[i] <- as.character(year)
}

seamheight_long <- seamheight %>%
  select(-NAME, -MSHA_MINE_ID) %>%
  melt(id = "MINE_UNIQUE_IDENTIFIER", variable.name = "year", value.name = "seam_height_in") %>%
  mutate(na_indicator = ifelse(is.na(seam_height_in), 1, 0),
         ones = 1, 
         seam_height_in_yr = as.numeric(seam_height_in),
         year = 2023-as.numeric(year),
         KEY_MINE_ID = as.numeric(MINE_UNIQUE_IDENTIFIER))

# Percentage of mine-years missing data: 79 percent
sum(seamheight_long$na_indicator)/sum(seamheight_long$ones)
hist(seamheight_long$seam_height_in_yr)

seamheight_long <- seamheight_long %>%
  filter(na_indicator == 0) %>%
  select(-na_indicator, -ones)

msha_capiq_crosswalk <- seamheight %>%
  mutate(KEY_MINE_ID = as.numeric(MINE_UNIQUE_IDENTIFIER)) %>%
  rename(MINE_ID = MSHA_MINE_ID) %>%
  select(KEY_MINE_ID, MINE_ID) %>%
  distinct()

contracts_seam_height_mine_year <- seamheight_long %>%
  full_join(contracts_year_mine, by = c("KEY_MINE_ID", "year")) %>%
  left_join(msha_capiq_crosswalk, by = c("KEY_MINE_ID")) %>%
  mutate(MINE_ID = as.numeric(MINE_ID))

eia_df <- read_csv(file.path(ddir, "cleaned", "coalpublic_1993_2021_mines.csv")) %>%
  rename(MINE_ID = `MSHA ID`,
         mine_name = `Mine Name`,
         year = Year,
         state = `Mine State`,
         county = `Mine County`,
         status = `Mine Status`,
         type = `Mine Type`,
         union_code = `Union Code`) %>%
  select(MINE_ID, year, mine_name, state, county, status, type, union_code, union) %>%
  mutate(MINE_ID = as.double(MINE_ID)) %>%
  filter(!is.na(MINE_ID))
  
contracts_cleaned <- contracts_semi_cleaned %>%
  left_join(msha_capiq_crosswalk, by = c("KEY_MINE_ID")) %>%
  mutate(MINE_ID = as.numeric(MINE_ID)) %>%
  filter(!is.na(MINE_ID)) %>%
  left_join(eia_df, by = c("MINE_ID", "year")) %>%
  filter(type == "Underground")

# Checking negative price elasticity of demand
lm3i <- lm(ln_QTY_COAL_PURCH ~ ln_PRICE_PER_TON, data = contracts_cleaned)
lm3ii <- lm(ln_QTY_COAL_PURCH ~ ln_PRICE_PER_TON*factor(contract_type), data = contracts_cleaned)
lm3iii <- lm(ln_QTY_COAL_PURCH ~ ln_PRICE_PER_TON*factor(year), data = filter(contracts_cleaned, contract_type == "Contract"))
summary(lm3i)
summary(lm3ii)
summary(lm3iii)


write_csv(contracts_seam_height_mine_year, file.path(ddir, "cleaned", "contracts_seam_height_mine_year.csv"))
write_csv(contracts_cleaned, file.path(ddir, "cleaned", "contracts_cleaned.csv"))

