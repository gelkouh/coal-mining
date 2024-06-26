# Last updated: Apr 4, 2023

root <- getwd()
while(basename(root) != "coal-mining") { # this is the name of your project directory you want to use
  root <- dirname(root)
}
source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

library(tidyverse)
library(readxl)

# 2015 is the base year
cpi_data <- read_excel(file.path(ddir, "FRED", "CPALTT01USQ661S.xls"), skip = 10, col_types = c("date","numeric")) %>%
  rename(date = observation_date,
         cpi = CPALTT01USQ661S) %>%
  mutate(year = year(date),
         quarter = quarter(date)) %>%
  dplyr::filter(year >= 2000, 
         year <= 2021) %>%
  dplyr::select(-date)

df <- read_csv(file.path(ddir, "countywagebyindustry - bls", "mining", paste0(2000, "_QCEW_mining_quarterly.csv"))) %>%
  dplyr::filter(industry_code == 2121) %>%
  dplyr::select(area_fips, industry_code, year, qtr, 
                qtrly_estabs, avg_wkly_wage, month1_emplvl, month2_emplvl, month3_emplvl) %>%
  mutate(avg_emplvl = (month1_emplvl+month2_emplvl+month3_emplvl)/3)

for (i in 2001:2021) {
  print(i)
  df_temp <- read_csv(file.path(ddir, "countywagebyindustry - bls", "mining", paste0(i, "_QCEW_mining_quarterly.csv"))) %>%
    dplyr::filter(industry_code == 2121) %>%
    dplyr::select(area_fips, industry_code, year, qtr, 
                  qtrly_estabs, avg_wkly_wage, month1_emplvl, month2_emplvl, month3_emplvl) %>%
    mutate(avg_emplvl = (month1_emplvl+month2_emplvl+month3_emplvl)/3)
  
  df <- df %>%
    bind_rows(df_temp)
}

df_wages_unadjusted <- df %>% 
  rename(quarter = qtr) %>%
  mutate(quarter_fraction = (as.numeric(quarter)-1.0)*0.25,
         year_quarter = as.numeric(year) + as.numeric(quarter_fraction)) %>%
  dplyr::select(-month1_emplvl, -month2_emplvl, -month3_emplvl, -quarter_fraction)

df_wages_adjusted <- df_wages_unadjusted %>%
  left_join(cpi_data, by = c("year", "quarter")) %>%
  mutate(avg_wkly_wage_real = 100*(avg_wkly_wage/cpi))

write_csv(df_wages_adjusted, file.path(ddir, "cleaned", "naics_2121_county_wages_2000_2021.csv"))

