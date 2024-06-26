# Import EIA data with mine union status

# Header ------------------------------------------------------------------

rm(list = ls())

root <- getwd()
while(basename(root) != "coal-mining") root <- dirname(root)

source(file.path(root, "scripts", "header_script.R"))

# Globals and file paths --------------------------------------------------

min_year <- 1993
max_year <- 2022

source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

input <- file.path(ddir, "EIA", "EIA-7A Data")

output <- file.path(ddir, "Raw Cleaned")

# Import and clean data ---------------------------------------------------

read_rename_coalpublic <- function(yr) {
  eia_raw <- readxl::read_excel(file.path(input, paste0("coalpublic", yr, ".xls")),
                                col_types = "text",
                                skip = 3) %T>%
    dplyr::glimpse()
  
  eia_renamed <- eia_raw %>%
    dplyr::mutate(year = as.numeric(Year),
                  msha_id = `MSHA ID`,
                  mine_name_eia = `Mine Name`,
                  mine_status_eia = `Mine Status`,
                  mine_type_eia = `Mine Type`,
                  operation_type_eia = `Operation Type`,
                  company_type_eia = `Company Type`,
                  union_code = `Union Code`) %>%
    dplyr::filter(as.numeric(year) == yr) %>%
    dplyr::select(year,
                  msha_id,
                  ends_with("_eia"),
                  union_code)
  
  eia_renamed
}

for (yr_i in min_year:max_year) {
  print(yr_i)
  
  eia_int <- read_rename_coalpublic(yr_i)
  
  if (yr_i == min_year) eia_combined <- eia_int
  if (yr_i > min_year) eia_combined %<>% dplyr::bind_rows(eia_int)
}

eia_out <- eia_combined %T>%
  dplyr::glimpse() %>%
  dplyr::filter(!is.na(msha_id)) %>%
  dplyr::mutate(union_code = ifelse(union_code %in% c("Non-Union", "-"),
                                    NA,
                                    union_code),
                union = !is.na(union_code),
                company_type_eia = ifelse(company_type_eia == "-",
                                          NA,
                                          company_type_eia),
                mine_status_eia = ifelse(mine_status_eia == "-",
                                         NA,
                                         mine_status_eia)) %T>%
  dplyr::glimpse()

readr::write_csv(eia_out, file.path(output, stringr::str_glue("01a eia annual mine union status, {min_year}-{max_year}.csv")))
