# Import MSHA data

# Header ------------------------------------------------------------------

rm(list = ls())

root <- getwd()
while(basename(root) != "coal-mining") root <- dirname(root)

source(file.path(root, "scripts", "header_script.R"))

# Globals and file paths --------------------------------------------------

source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

output <- file.path(ddir, "Raw Cleaned")

# Import data -------------------------------------------------------------

generate_file_list <- function(df, file_type) {
  df %>%
    dplyr::filter(`DATA SOURCE` == "MSHA",
                  grepl(file_type, `DATA FILE NAME`)) %>%
    dplyr::mutate(file_path_data = file.path(ddir, `DATA SOURCE`, `DATA NAME`, `DATA FILE NAME`),
                  file_path_data_dictionary = file.path(ddir, `DATA SOURCE`, `DATA NAME`, `DATA DICTIONARY FILE NAME`)) %>%
    dplyr::select(`DATA NAME`,
                  file_path_data,
                  file_path_data_dictionary)
}

mine_level_files_txt <- readxl::read_excel(file.path(ddir, "#Data Index.xlsx"),
                                       sheet = "Data") %>%
  generate_file_list("txt")

mine_level_files_csv <- readxl::read_excel(file.path(ddir, "#Data Index.xlsx"),
                                           sheet = "Data") %>%
  generate_file_list("csv") %$%
  file_path_data

mine_leve_files_excel <- readxl::read_excel(file.path(ddir, "#Data Index.xlsx"),
                                            sheet = "Data") %>%
  generate_file_list("xlsx") %$%
  file_path_data

# MSHA .txt files
for (file_path in mine_level_files_txt$file_path_data) {
  data_name <- mine_level_files_txt %>% 
    dplyr::filter(file_path_data == file_path) %$%
    `DATA NAME` %>%
    stringr::str_to_lower() %>%
    stringr::str_replace_all("\\.", "") %T>%
    print()

  df <- data.table::fread(file_path) %T>%
    dplyr::glimpse()
  
  # readr::write_csv(df, file.path(output, stringr::str_glue("01b msha {data_name}.csv")))
  saveRDS(df, file.path(output, stringr::str_glue("01b msha {data_name}.rds")))
}

# MSHA .csv files
df_csv <- readr::read_csv(mine_level_files_csv) %>%
  dplyr::mutate(MINE_ID = as.integer(`Mine ID`)) %T>%
  dplyr::glimpse()

readr::write_csv(df_csv, file.path(output, "01b msha 20 107(a) orders issued.csv"))

# MSHA Excel files
df_excel <- readxl::read_excel(mine_leve_files_excel,
                               sheet = "Data") %T>%
  dplyr::glimpse()

readr::write_csv(df_excel, file.path(output, "01b msha coal fatalities.csv"))

# Data dictionaries
definition_file <- read.delim(file.path(ddir, "MSHA", "1. Accident Injuries", "Accidents_Definition_File.txt"), sep = "|")
for (folder_name in list.files(path = file.path(ddir, "MSHA"))) {
  if (folder_name != "1. Accident Injuries") {
    for (file_name in list.files(path = file.path(ddir, "MSHA", folder_name))) {
      if (grepl("Definition_File", file_name)) {
        definition_file <- rbind(definition_file, read.delim(file.path(ddir, "MSHA", folder_name, file_name), sep = "|"))
      }
    }
  }
}

readr::write_csv(definition_file, file.path(output, "01b msha definition file.csv"))
