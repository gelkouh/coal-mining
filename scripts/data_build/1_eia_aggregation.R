# Last updated: Jan 9, 2023

root <- getwd()
while(basename(root) != "coal-mining") { # this is the name of your project directory you want to use
  root <- dirname(root)
}
source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

library(tidyverse)

# Data from U.S. Energy Information Administration Form EIA-7A, 'Annual Survey of Coal Production and Preparation,' 
# U.S. Department of Labor, Mine Safety and Health Administration Form 7000-2, 'Quarterly Mine Employment and Coal Production Report.'
# EIA annual reports exclude mines producing less than 25,000 short tons in summary tables, etc.
# Average is quarterly average
# Downloaded from: https://www.eia.gov/coal/data.php

df <- read_csv(file.path(ddir, "coalpublic - eia", paste0("coalpublic", 1993, ".csv")), skip = 3,
               col_types = cols(
                 Year = col_double(),
                 `MSHA ID` = col_character(),
                 `Mine Name` = col_character(),
                 `Mine State` = col_character(),
                 `Mine County` = col_character(),
                 `Mine Status` = col_character(),
                 `Mine Type` = col_character(),
                 `Company Type` = col_character(),
                 `Operation Type` = col_character(),
                 `Operating Company` = col_character(),
                 `Operating Company Address` = col_character(),
                 `Union Code` = col_character(),
                 `Production (short tons)` = col_number()
               )) %>%
  mutate(`Production (short tons)` = as.character(`Production (short tons)`))
for (i in 1994:2021) {
  print(i)
  if (i < 2001) {
    df_temp <- read_csv(file.path(ddir, "coalpublic - eia", paste0("coalpublic", i, ".csv")), skip = 3,
                        col_types = cols(
                          Year = col_double(),
                          `MSHA ID` = col_character(),
                          `Mine Name` = col_character(),
                          `Mine State` = col_character(),
                          `Mine County` = col_character(),
                          `Mine Status` = col_character(),
                          `Mine Type` = col_character(),
                          `Company Type` = col_character(),
                          `Operation Type` = col_character(),
                          `Operating Company` = col_character(),
                          `Operating Company Address` = col_character(),
                          `Union Code` = col_character()
                        )) %>%
      mutate(`Production (short tons)` = as.character(`Production (short tons)`))
  }
  
  if (i >= 2001) {
    df_temp <- read_csv(file.path(ddir, "coalpublic - eia", paste0("coalpublic", i, ".csv")), skip = 3,
                                   col_types = cols(
                                     Year = col_double(),
                                     `MSHA ID` = col_character(),
                                     `Mine Name` = col_character(),
                                     `Mine State` = col_character(),
                                     `Mine County` = col_character(),
                                     `Mine Status` = col_character(),
                                     `Mine Type` = col_character(),
                                     `Company Type` = col_character(),
                                     `Operation Type` = col_character(),
                                     `Operating Company` = col_character(),
                                     `Operating Company Address` = col_character(),
                                     `Union Code` = col_character(),
                                     `Average Employees` = col_character(),
                                     `Labor Hours` = col_character()
                                   )) %>%
      mutate(`Production (short tons)` = as.character(`Production (short tons)`)) %>%
      select(!c(`Average Employees`,`Labor Hours`))
  }

  df <- df %>%
    bind_rows(df_temp)
}

# Preparation plant:  A mining facility at which coal is crushed, screened, and mechanically cleaned.
# Drop preparation plants and mines, preparation plant combination facilities (i.e., look at mines only)
df_export <- df %>%
  mutate(`Union Code` = ifelse((is.na(`Union Code`) | `Union Code` == "-" | `Union Code` == "Non-Union"),
                               NA,
                               `Union Code`),
         union = ifelse(is.na(`Union Code`),
                        0,
                        1),
         `Production (short tons)` = as.numeric(`Production (short tons)`)
  ) %>%
  filter(`Operation Type` == "Mine only") %>%
  select(!c(`Coal Supply Region`, `Mine Basin`))

write_csv(df_export, file.path(ddir, "cleaned", "coalpublic_1993_2021_mines.csv"))
