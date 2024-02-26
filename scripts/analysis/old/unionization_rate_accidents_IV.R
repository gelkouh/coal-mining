# Test first stage for accidents as instrument for unionization

# TAKEAWAY:
# No correlation in rough tests
# Only a small proportion of underground coal mines (6.2%) changed union status during the period examined (1993–2010), 
# and those that did change status seem to be highly unrepresentative of the population as a whole (see footnote 19 [of Morantz (2013)])"
# => does not seem fruitful to try to find instrument for unionization
# Instead, 
#   A. exploit variation created by mergers to see how some outcome changes for unionized vs. non-unionized mines
#   B. look at how unionized vs. non-unionized (underground) mines are differentially affected by MINER Act

library(tidyverse)
library(reshape2)

ddir <- "/Users/gelkouh/Library/CloudStorage/OneDrive-Personal/Documents/School/UChicago/Thesis/MINING/data"

df_injuries_long <- read_csv(file.path(ddir, "capiq", "annual_injuries.csv"), skip = 2, col_names = TRUE) %>% 
  filter(!is.na(MINE_UNIQUE_IDENTIFIER)) %>%
  melt(id.vars = c("NAME", "MINE_UNIQUE_IDENTIFIER"),
       measure.vars = c("2021 Y",
                        "2020 Y",
                        "2019 Y",
                        "2018 Y",
                        "2017 Y",
                        "2016 Y",
                        "2015 Y",
                        "2014 Y",
                        "2013 Y",
                        "2012 Y",
                        "2011 Y",
                        "2010 Y",
                        "2009 Y",
                        "2008 Y",
                        "2007 Y",
                        "2006 Y",
                        "2005 Y",
                        "2004 Y",
                        "2003 Y",
                        "2002 Y",
                        "2001 Y",
                        "2000 Y",
                        "1999 Y",
                        "1998 Y",
                        "1997 Y",
                        "1996 Y",
                        "1995 Y",
                        "1994 Y")
  ) %>%
  rename(year = variable,
         num_injuries = value) %>%
  mutate(year = as.numeric(substr(year,1,nchar(as.character(year))-2)),
         num_injuries = ifelse(is.na(num_injuries), 0, num_injuries))

df_mineid_mshaid_crosswalk <- read_csv(file.path(ddir, "capiq", "mineuniqueid_mshaid.csv"), skip = 3) %>%
  rename(MINE_UNIQUE_IDENTIFIER = X2, MSHA_MINE_ID = X3) %>% 
  select(MINE_UNIQUE_IDENTIFIER, MSHA_MINE_ID)

df_injuries_long <- df_injuries_long %>%
  merge(df_mineid_mshaid_crosswalk, by = c("MINE_UNIQUE_IDENTIFIER"))

df_unionization <- read_csv(file.path(ddir, "coalpublic - eia", "coalpublic_1993_2021_filtered.csv"),
                            col_types = cols(`Average Employees` = col_character(),
                            `Labor Hours` = col_character(),
                            `Coal Supply Region` = col_character())) %>%
  select(Year, `MSHA ID`, `Mine State`, `Mine County`, `Mine Status`, `Average Employees`, `drop`, `union`) %>%
  rename(year = Year, 
         MSHA_MINE_ID = `MSHA ID`,
         state = `Mine State`,
         county = `Mine County`,
         status = `Mine Status`,
         avg_employees = `Average Employees`) %>%
  mutate(state_county = paste(state, county, sep = "_")) %>%
  filter(drop != 1)

df_injuries_unionization <- df_injuries_long %>%
  merge(df_unionization, by = c("year", "MSHA_MINE_ID")) %>%
  rename(msha_id = MSHA_MINE_ID) %>%
  select(state_county, year, msha_id, avg_employees, num_injuries, union) %>%
  arrange(state_county, msha_id, year) %>%
  filter(!is.na(avg_employees)) %>%
  mutate(avg_employees = as.numeric(avg_employees), 
         num_injuries = as.numeric(num_injuries),
         union = as.numeric(union)) %>%
  mutate(union_employees = union*avg_employees) %>%
  arrange(msha_id, year)

df_injuries_unionization_county <- df_injuries_unionization %>%
  group_by(state_county, year) %>%
  summarise(tot_avg_employees = sum(avg_employees),
            tot_union_employees = sum(union_employees),
            tot_injuries = sum(num_injuries)) %>%
  mutate(injuries_per_100_workers = 100*(tot_injuries/tot_avg_employees),
         pct_workers_unionized = tot_union_employees/tot_avg_employees) %>%
  select(state_county, year, injuries_per_100_workers, pct_workers_unionized, tot_injuries) %>%
  arrange(state_county, year) %>%
  mutate(tot_injuries_lag1 = lag(tot_injuries), 
         pct_workers_unionized_lag1 = lag(pct_workers_unionized),
         pct_workers_unionized_lag2 = lag(pct_workers_unionized_lag1),
         injuries_per_100_workers_lag1 = lag(injuries_per_100_workers),
         injuries_per_100_workers_lag2 = lag(injuries_per_100_workers_lag1),
         change_injuries_per_100_workers_lag2_1 = injuries_per_100_workers_lag1 - injuries_per_100_workers_lag2,
         change_pct_workers_unionized_lag1 = pct_workers_unionized - pct_workers_unionized_lag1,
         change_pct_workers_unionized_lag2 = pct_workers_unionized_lag1 - pct_workers_unionized_lag2) %>%
  ungroup()

lm_1 <- lm(pct_workers_unionized ~ tot_injuries_lag1, data = df_injuries_unionization_county)
summary(lm_1)

lm_2 <- lm(change_pct_workers_unionized_lag1 ~ tot_injuries_lag1, data = df_injuries_unionization_county)
summary(lm_2)
