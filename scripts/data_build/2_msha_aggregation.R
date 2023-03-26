# Last updated: Mar 23, 2023

root <- getwd()
while(basename(root) != "coal-mining") { # this is the name of your project directory you want to use
  root <- dirname(root)
}
source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

library(tidyverse)
library(lubridate)
library(reshape2)

# MSHA data
# Downloaded from: https://www.msha.gov/data-and-reports/mine-data-retrieval-system
# Also access at: https://arlweb.msha.gov/OpenGovernmentData/ogimsha.asp
accidents_1 <- read.delim(file.path(ddir, "msha", "Accidents.txt"), sep = "|")
#conferences_4 <- read.delim(file.path(ddir, "msha", "Conferences.txt"), sep = "|")
#civilpenalties_5 <- read.delim(file.path(ddir, "msha", "CivilPenaltyDocketsDecisions.txt"), sep = "|")
#contractorprod_quarterly_6 <- read.delim(file.path(ddir, "msha", "ContractorProdQuarterly.txt"), sep = "|")
#contractorprod_yearly_7 <- read.delim(file.path(ddir, "msha", "ContractorProdYearly.txt"), sep = "|")
controlleroperatorhistory_8 <- read.delim(file.path(ddir, "msha", "ControllerOperatorHistory.txt"), sep = "|")
minesprod_yearly_9 <- read.delim(file.path(ddir, "msha", "MinesProdYearly.txt"), sep = "|")
minesprod_quarterly_10 <- read.delim(file.path(ddir, "msha", "MinesProdQuarterly.txt"), sep = "|")
#inspections_11 <- read.delim(file.path(ddir, "msha", "Inspections.txt"), sep = "|")
#mineaddresses_12 <- read.delim(file.path(ddir, "msha", "AddressOfRecord.txt"), sep = "|")
mines_13 <- read.delim(file.path(ddir, "msha", "Mines.txt"), sep = "|")
#healthsamples_15 <- read.delim(file.path(ddir, "msha", "PersonalHealthSamples.txt"), sep = "|")
violations_17 <- read.delim(file.path(ddir, "msha", "Violations.txt"), sep = "|")
contestedviolations_18 <- read.delim(file.path(ddir, "msha", "ContestedViolations.txt"), sep = "|")
assessedviolations_19 <- read.delim(file.path(ddir, "msha", "AssessedViolations.txt"), sep = "|")

definition_file <- read.delim(file.path(ddir, "msha", "Accidents_Definition_File.txt"), sep = "|")
for (file_name in list.files(path = file.path(ddir, "msha"))) {
  if (grepl("Definition_File", file_name) & file_name != "Accidents_Definition_File.txt") {
    definition_file <- rbind(definition_file, read.delim(file.path(ddir, "msha", file_name), sep = "|"))
  }
}
write_csv(definition_file, file.path(ddir, "cleaned", "msha_definition_file.csv"))

##----------##
# accidents_1 - done
##----------##
# The traumatic injury category comprises the following: 
# amputations; enucleations; fractures; chips; 
# dislocations; foreign bodies in eyes; cuts and lacerations; punctures; burns/ scalds; crushings; 
# chemical, electrical, and laser burns; and fatalities.
# For each tabulation, I include only injuries that occurred in the underground subunit of a mine.
# (Morantz (2013))

# Manually identify codes for traumatic injuries
# DEGREE_INJURY:
# (01) Fatality; (02) Permanent total or permanent partial disability; (03) Days away from work only; 
# (04) Days away from work and restricted activity
# unique_injury_codes <- unique(accidents_1$NATURE_INJURY_CD)
# injury_codes_names <- c()
# for (i in 1:length(unique_injury_codes)) {
#   injury_codes_names <- append(injury_codes_names, paste(paste0(i, ", ", unique_injury_codes[i],":"), unique(filter(accidents_1, NATURE_INJURY_CD == unique_injury_codes[i])$NATURE_INJURY)))
# }
# unique_injury_body_part_codes <- unique(accidents_1$INJ_BODY_PART_CD)
# body_parts_codes_names <- c()
# for (i in 1:length(unique_injury_body_part_codes)) {
#   body_parts_codes_names <- append(body_parts_codes_names, paste(paste0(i, ", ", unique_injury_body_part_codes[i],":"), unique(filter(accidents_1, INJ_BODY_PART_CD == unique_injury_body_part_codes[i])$INJ_BODY_PART)))
# }
# for (i in 1:(length(unique_injury_body_part_codes)-length(unique_injury_codes))) {
#   injury_codes_names <- append(injury_codes_names, NA)
# }
# injuries_body_parts_codes <- data_frame(injuries = injury_codes_names, body_parts = body_parts_codes_names)
# write_csv(injuries_body_parts_codes, file.path(ddir, "cleaned", "injuries_body_parts_codes.csv"))

# Define "traumatic injury" by 
# amputations; enucleations;
# NATURE_INJURY_CD == '100' (AMPUTATION OR ENUCLEATION)
# OR
# fractures; chips; 
# NATURE_INJURY_CD == '220' (FRACTURE,CHIP)
# OR
# dislocations; 
# NATURE_INJURY_CD == '200' (DISLOCATION)
# OR
# foreign bodies in eyes;
# NATURE_INJURY_CD == '320 (DUST IN EYES)
# OR
# cuts and lacerations; punctures
# NATURE_INJURY_CD == '180' (CUT,LACER,PUNCT-OPN WOUND)
# OR
# burns/ scalds; chemical, electrical, and laser burns;
# NATURE_INJURY_CD == '130' (BURN,CHEMICL-FUME,COMPOUN)
# NATURE_INJURY_CD == '301' (ELECT.ARC BURN-NOT CONTAC)
# NATURE_INJURY_CD == '120' (BURN OR SCALD (HEAT))
# NATURE_INJURY_CD == '360' (ELECTRIC BURN-CNTACT BURN)
# NATURE_INJURY_CD == '302' (LASER BURN)
# Note: NOT including NATURE_INJURY_CD == '304' (SUNBURN), 
# but aggregate these alone to see breakdown of union vs. non-union underground coal mine sunburns
# OR
# crushings; 
# NATURE_INJURY_CD == '170' (CRUSHING)
# OR
# and fatalities.
# DEGREE_INJURY_CD == '01' (Fatality)

# Description of the subunit code referring to the location within a mine where the accident/injury/illness occurred: 
# (01) Underground; (02) Surface at underground
accidents_1_temp <- accidents_1  %>%
  filter(SUBUNIT_CD == 1) %>%
  rename(year = CAL_YR,
         quarter = CAL_QTR) %>%
  mutate(fatality = ifelse(DEGREE_INJURY_CD == '01', 1, 0),
         traumatic_injury = ifelse(NATURE_INJURY_CD %in% c('100','220','200','320','180','130',
                                                           '301','120','360','302','170') |
                                     fatality == 1,
                                   1, 0),
         nontraumatic_injury = ifelse(traumatic_injury == 0, 1, 0),
         injury = 1)

accidents_cleaned_quarters <- accidents_1_temp %>%
  group_by(MINE_ID, year, quarter) %>%
  summarize(fatalities = sum(fatality, na.rm = TRUE),
            traumatic_injuries = sum(traumatic_injury, na.rm = TRUE),
            nontraumatic_injuries = sum(nontraumatic_injury, na.rm = TRUE),
            total_injuries = sum(injury, na.rm = TRUE)) %>%
  arrange(MINE_ID, year, quarter)

accidents_cleaned_years <- accidents_1_temp %>%
  group_by(MINE_ID, year) %>%
  summarize(fatalities = sum(fatality, na.rm = TRUE),
            traumatic_injuries = sum(traumatic_injury, na.rm = TRUE),
            nontraumatic_injuries = sum(nontraumatic_injury, na.rm = TRUE),
            total_injuries = sum(injury, na.rm = TRUE)) %>%
  arrange(MINE_ID, year)

##----------##
# controlleroperatorhistory_8 - done
##----------##
# Controller appears to be the (controlling) owner
# => Change of controller is akin to change of ownership?
# (https://www.npr.org/2014/11/12/363511746/delinquent-mines-about-the-data)
# "Operator" is defined in the Mine Act as:
# "Any owner, lessee, or other person who operates, controls, or supervises a coal or other mine 
# or any independent contractor performing services or construction at such mine."
# (https://www.msha.gov/msha-and-osha-memorandum)

# Controllers
controller_id_names <- controlleroperatorhistory_8 %>%
  select(CONTROLLER_ID, CONTROLLER_NAME) %>%
  unique() %>%
  group_by(CONTROLLER_ID) %>%
  summarize(CONTROLLER_NAMES = paste0(CONTROLLER_NAME, collapse = "; "))

controller_id_types <- controlleroperatorhistory_8 %>%
  select(CONTROLLER_ID, CONTROLLER_TYPE) %>%
  unique() %>%
  group_by(CONTROLLER_ID) %>%
  summarize(CONTROLLER_TYPES = paste0(CONTROLLER_TYPE, collapse = "; ")) %>%
  mutate(CONTROLLER_TYPES = ifelse(CONTROLLER_TYPES == "COMPANY; PERSON", 
                                   "PERSON; COMPANY", 
                                   CONTROLLER_TYPES))

controller_history_long <- controlleroperatorhistory_8 %>%
  dplyr::filter(COAL_METAL_IND == "C") %>%
  mutate(start_date = mdy(CONTROLLER_START_DT),
         start_year = year(start_date), 
         start_quarter = quarter(start_date),
         start_quarter_fraction = (as.numeric(start_quarter)-1.0)*0.25,
         start_year_quarter = as.numeric(start_year) + as.numeric(start_quarter_fraction)) %>%
  arrange(MINE_ID, start_date) %>%
  group_by(MINE_ID, start_year_quarter) %>%
  slice_tail() %>%
  ungroup() %>%
  dplyr::select(MINE_ID, CONTROLLER_ID, start_year_quarter)

controller_history_panel_quarters <- expand.grid(start_year_quarter = seq(from = min(controller_history_long$start_year_quarter), to = 2022.00, by = 0.25),
                                                 MINE_ID = unique(controller_history_long$MINE_ID),
                                                 stringsAsFactors = FALSE) %>%
  left_join(controller_history_long) %>% 
  group_by(MINE_ID) %>% 
  fill(c("CONTROLLER_ID")) %>%
  filter(!is.na(CONTROLLER_ID)) %>%
  as.data.frame() %>%
  mutate(controller_change = ifelse(MINE_ID == lag(MINE_ID) & CONTROLLER_ID != lag(CONTROLLER_ID),
                                    1, 0),
         start_year = floor(start_year_quarter)) %>%
  filter(start_year >= 2000) %>%
  ungroup() %>%
  left_join(controller_id_names, by = "CONTROLLER_ID") %>%
  left_join(controller_id_types, by = "CONTROLLER_ID")

controller_history_panel_years <- controller_history_panel_quarters %>%
  group_by(MINE_ID, start_year) %>%
  summarize(controller_change = max(controller_change, na.rm = TRUE),
            index = n(),
            CONTROLLER_ID = CONTROLLER_ID[which.max(index)]) %>%
  dplyr::select(!index) %>%
  left_join(controller_id_names, by = "CONTROLLER_ID") %>%
  left_join(controller_id_types, by = "CONTROLLER_ID")

# Operators
# Age of mine defined as years since the first operator began work at the mine (top censored at 1970)
operator_mine_age <- controlleroperatorhistory_8 %>%
  dplyr::filter(COAL_METAL_IND == "C") %>%
  mutate(start_date = mdy(OPERATOR_START_DT),
         start_year = year(start_date)) %>%
  group_by(MINE_ID) %>%
  summarize(first_year = min(start_year, na.rm = TRUE)) %>%
  mutate(first_year = ifelse(first_year < 1970, 1970, first_year))

operator_id_names <- controlleroperatorhistory_8 %>%
  select(OPERATOR_ID, OPERATOR_NAME) %>%
  unique() %>%
  group_by(OPERATOR_ID) %>%
  summarize(OPERATOR_NAMES = paste0(OPERATOR_NAME, collapse = "; "))

operator_history_long <- controlleroperatorhistory_8 %>%
  dplyr::filter(COAL_METAL_IND == "C") %>%
  mutate(start_date = mdy(OPERATOR_START_DT),
         start_year = year(start_date), 
         start_quarter = quarter(start_date),
         start_quarter_fraction = (as.numeric(start_quarter)-1.0)*0.25,
         start_year_quarter = as.numeric(start_year) + as.numeric(start_quarter_fraction)) %>%
  arrange(MINE_ID, start_date) %>%
  group_by(MINE_ID, start_year_quarter) %>%
  slice_tail() %>%
  ungroup() %>%
  dplyr::select(MINE_ID, OPERATOR_ID, start_year_quarter)

operator_history_panel_quarters <- expand.grid(start_year_quarter = seq(from = min(operator_history_long$start_year_quarter), to = 2022.00, by = 0.25),
                                               MINE_ID = unique(operator_history_long$MINE_ID),
                                               stringsAsFactors = FALSE) %>%
  left_join(operator_history_long) %>% 
  group_by(MINE_ID) %>% 
  fill(c("OPERATOR_ID")) %>%
  filter(!is.na(OPERATOR_ID)) %>%
  as.data.frame() %>%
  mutate(operator_change = ifelse(MINE_ID == lag(MINE_ID) & OPERATOR_ID != lag(OPERATOR_ID),
                                  1, 0),
         start_year = floor(start_year_quarter)) %>%
  filter(start_year >= 2000) %>%
  ungroup() %>%
  left_join(operator_id_names, by = "OPERATOR_ID")

operator_history_panel_years <- operator_history_panel_quarters %>%
  group_by(MINE_ID, start_year) %>%
  summarize(operator_change = max(operator_change, na.rm = TRUE),
            index = n(),
            OPERATOR_ID = OPERATOR_ID[which.max(index)]) %>%
  dplyr::select(!index) %>%
  left_join(operator_id_names, by = "OPERATOR_ID") 

controller_operator_panel_quarters <- controller_history_panel_quarters %>%
  full_join(operator_history_panel_quarters, by = c("MINE_ID", "start_year_quarter", "start_year")) %>%
  mutate(start_quarter = start_year_quarter - start_year,
         year = start_year, 
         quarter = ifelse(start_quarter == 0, 1,
                          ifelse(start_quarter == 0.25, 2,
                                 ifelse(start_quarter == 0.5, 3,
                                        4)))) %>%
  dplyr::select(-start_year_quarter, -start_year, -start_quarter) %>%
  left_join(operator_mine_age, by = "MINE_ID") %>%
  mutate(mine_age = year - first_year)

controller_operator_panel_years <- controller_history_panel_years %>%
  full_join(operator_history_panel_years, by = c("MINE_ID", "start_year")) %>%
  rename(year = start_year) %>%
  left_join(operator_mine_age, by = "MINE_ID") %>%
  mutate(mine_age = year - first_year)

##----------##
# minesprod_yearly_9 - done
##----------##
# Productivity: thousands of tons of coal produced per annual FTE (2,000 hours)
# Yearly FTEs are defined as 2,000 hours worked, and quarterly FTEs are defined as 500 hours worked
# Size measure: either 100 FTEs, 100 employees, or one million tons
# Drop any mine-quarters in which a mine reported zero coal production and/or zero hours worked
# (Morantz (2013))
# Continue restricting analysis to underground operation subunit codes
# Description of the subunit code referring to a location within a mine:  (01) Underground operation;  
# (02) Surface operation at underground mine;  (03) Strip, quarry or open pit;  (04) Auger (Coal only);  (05) Culm bank or refuse pile (Coal only);  
# (06) Dredge;  (12) Other surface (Metal/Non-Metal only);  (17) Independent shop or yard;  (30) Mill operation/preparation plant;  (99) Office workers at mine site. 
# Subunit indicator: 1 if mine contains a given subunit, 0 otherwise Subunit types include, e.g., “surface” and “mill or prep plant”
subunit_years <- minesprod_yearly_9 %>%
  rename(year = CALENDAR_YR) %>%
  dplyr::select(year, MINE_ID, SUBUNIT_CD) %>%
  distinct() %>%
  mutate(one = 1) %>%
  dcast(year + MINE_ID ~ paste0("subunit_", SUBUNIT_CD), value.var = "one") %>%
  mutate_all(~coalesce(.,0))

production_cleaned_years <- minesprod_yearly_9 %>%
  rename(year = CALENDAR_YR) %>%
  filter(SUBUNIT_CD == 1) %>%
  group_by(MINE_ID, year) %>%
  summarize(labor_hours = sum(ANNUAL_HRS, na.rm = TRUE),
            coal_production_tons = sum(ANNUAL_COAL_PROD, na.rm = TRUE),
            avg_employee_count = sum(AVG_ANNUAL_EMPL, na.rm = TRUE)) %>%
  mutate(zero_production_year = ifelse(labor_hours == 0 | coal_production_tons == 0, 
                                          1, 0),
         FTEs = labor_hours/2000,
         productivity = ifelse(zero_production_year == 1, 0, 2000*(coal_production_tons/labor_hours)),
         size_100FTEs = FTEs/100,
         size_100employees = avg_employee_count/100,
         size_1milliontons = coal_production_tons/1000000) %>%
  left_join(subunit_years, by = c("MINE_ID", "year"))

##----------##
# minesprod_quarterly_10 - done
##----------##
# Productivity: thousands of tons of coal produced per annual FTE (2,000 hours)
# Yearly FTEs are defined as 2,000 hours worked, and quarterly FTEs are defined as 500 hours worked
# Size measure: either 100 FTEs, 100 employees, or one million tons
# Drop any mine-quarters in which a mine reported zero coal production and/or zero hours worked
# (Morantz (2013))
# Continue restricting analysis to underground operation subunit codes
# Description of the subunit code referring to a location within a mine:  (01) Underground operation;  
# (02) Surface operation at underground mine;  (03) Strip, quarry or open pit;  (04) Auger (Coal only);  (05) Culm bank or refuse pile (Coal only);  
# (06) Dredge;  (12) Other surface (Metal/Non-Metal only);  (17) Independent shop or yard;  (30) Mill operation/preparation plant;  (99) Office workers at mine site. 
# Subunit indicator: 1 if mine contains a given subunit, 0 otherwise Subunit types include, e.g., “surface” and “mill or prep plant”
subunit_quarters <- minesprod_quarterly_10 %>%
  rename(year = CAL_YR,
         quarter = CAL_QTR) %>%
  dplyr::select(year, quarter, MINE_ID, SUBUNIT_CD) %>%
  distinct() %>%
  mutate(one = 1) %>%
  dcast(year + quarter + MINE_ID ~ paste0("subunit_", SUBUNIT_CD), value.var = "one") %>%
  mutate_all(~coalesce(.,0))

production_cleaned_quarters <- minesprod_quarterly_10 %>%
  rename(year = CAL_YR,
         quarter = CAL_QTR) %>%
  filter(SUBUNIT_CD == 1) %>%
  group_by(MINE_ID, year, quarter) %>%
  summarize(labor_hours = sum(HOURS_WORKED, na.rm = TRUE),
            coal_production_tons = sum(COAL_PRODUCTION, na.rm = TRUE),
            avg_employee_count = sum(AVG_EMPLOYEE_CNT, na.rm = TRUE)) %>%
  mutate(zero_production_quarter = ifelse(labor_hours == 0 | coal_production_tons == 0, 
                                                1, 0),
         FTEs = labor_hours/500,
         productivity = ifelse(zero_production_quarter == 1, 0, 2000*(coal_production_tons/labor_hours)),
         size_100FTEs = FTEs/100,
         size_100employees = avg_employee_count/100,
         size_1milliontons = coal_production_tons/1000000) %>%
  left_join(subunit_quarters, by = c("MINE_ID", "year", "quarter"))

##----------##
# inspections_11 - tbd
##----------##
# inspections_cleaned <- inspections_11 %>%
#   mutate(inspection_date = mdy(INSPECTION_END_DT),
#          year = year(inspection_date),
#          quarter = year(inspection_date)) %>%
#   group_by(MINE_ID, year, quarter) %>%
#   summarize(inspections = n()) %>%
#   ungroup()

##----------##
# mines_13 - done
##----------##
# Unique code abbreviation for the primary industry group code for a mine. (1) Coal(Anthracite) SIC 123100; 
# (2) Coal(Bituminous); (5) M/NM (Sand and Gravel); (6) M/NM (Stone); (7) NonMetal; (8) Metal. May contain null values.
# Focusing on underground mines that extract bituminous coal
# District dummies: 1 if mine is located in a given MSHA district, 0 otherwise
# (Morantz (2013))
mines_cleaned <- mines_13 %>%
  filter(PRIMARY_CANVASS_CD == '2') %>%
  rename(district = DISTRICT,
         msha_office_code = OFFICE_CD,
         inspector_office_dist_miles = MILES_FROM_OFFICE,
         avg_mine_height_in = AVG_MINE_HEIGHT,
         longitude = LONGITUDE,
         latitude = LATITUDE,
         state = STATE,
         county_fips = FIPS_CNTY_CD,
         county = FIPS_CNTY_NM) %>%
  mutate(bituminous = 1,
         pillar_recovery_used = ifelse(PILLAR_RECOVERY_USED == "Y", 1, 0),
         part_48_training_restriction = ifelse(PART48_TRAINING == "Y", 1, 0)) %>%
  dplyr::select(MINE_ID, bituminous, inspector_office_dist_miles, 
                avg_mine_height_in, longitude, latitude, pillar_recovery_used, 
                part_48_training_restriction, msha_office_code, district,
                county, county_fips, state)

##----------##
# violations_17 - done
##----------##
# MINE_TYPE: Mine type of the mine where the violation has been issued:  Facility, Surface or Underground.
# => keep only Underground
# Use date of inspection as relevant date
violations_semi_cleaned <- violations_17 %>%
  mutate(underground = ifelse(MINE_TYPE == "Underground", 1, 0),
         inspection_date = mdy(INSPECTION_END_DT),
         violation_date = mdy(VIOLATION_OCCUR_DT),
         year = year(violation_date),
         quarter = quarter(violation_date),
         ss_violation = ifelse(SIG_SUB == "Y", 1, 0)) %>%
  filter(!is.na(MINE_ID),
         underground == 1)

# https://www.msha.gov/miner-act
# The MINER Act was implemented through a series of rulemakings and policy letters
# Note for all Federal Register documents: 
# search text for [Removed] or Removed for sections of the code that were dropped with the new rule

# Refuge Alternatives for Underground Coal Mines
# The Refuge Alternatives Rule required operators of underground coal mines 
# to provide refuge alternatives to protect miners when a life-threatening event occurs 
# that makes escape impossible.
# https://www.federalregister.gov/documents/2008/12/31/E8-30669/refuge-alternatives-for-underground-coal-mines#sectno-reference-7.505
# Effective Date: The final rule is effective on March 2, 2009.
# No Compliance Dates more than one quarter beyond the Effective Date 
# except for annual expectations training rule 75.1504(c)(3) (December 31, 2009)
refuge_03022009_regex <- c("^7\\.[5][0-1][0-9]",
                   "^75\\.360\\([defgh]\\)",
                   "^75\\.1502\\([c]\\)\\((3|4|8|10|11|12)\\)",
                   "^75\\.1504\\([defgh]\\)",
                   "^75\\.1202\\-1\\(b\\)\\(4\\)",
                   "^75\\.1200\\-1\\(n\\)",
                   "^75\\.372\\(b\\)\\(11\\)",
                   "^75\\.221\\(a\\)\\(12\\)", 
                   "^75\\.313\\(f\\)",
                   "^75\\.1501\\(a\\)\\(1\\)",
                   "^75\\.1504\\(b\\)\\(346789\\)",
                   "^75\\.1504\\(c\\)",
                   "^75\\.1505\\([ab]\\)",
                   "^75\\.150[678]",
                   "^75\\.1600\\-3"
                   )

# Flame-Resistant Conveyor Belt, Fire Prevention and Detection, and Use of Air From the Belt Entry
# The Belt Air/Belt Flammability Rule strengthened requirements regarding the
# fire retardant properties of conveyor belts and the use of belt air for ventilation. 
# The rule incorporated the recommendations of a Technical Study Panel created by the MINER Act.
# https://www.federalregister.gov/documents/2008/12/31/E8-30639/flame-resistant-conveyor-belt-fire-prevention-and-detection-and-use-of-air-from-the-belt-entry
# Effective Date: The final rule is effective on December 31, 2008.
# No Compliance Dates more than one quarter beyond the Effective Date 
# except for: 
# 75.380(d)(7), 75.380(f), 75.381(e)(5), and 75.381(f) by June 30, 2009.
# 75.350(a)(2), 75.351(e)(2), 75.1103-4(a), 75.1108(a), and 75.1108(b) December 31, 2009.
# 75.1108(c) by December 31, 2018.
belts_fires_12312008_regex <- c("^6\\.2$",
                   "^6\\.20\\(a\\)\\(1\\)",
                   "^14\\.(1|2|3|4|5|6|7|8|9|10|11|20|21|22|23)",
                   "^18\\.(1|2|6|9|65)",
                   "^48\\.27\\(a\\)",
                   "^75\\.333\\(c\\)\\(4\\)",
                   "^75\\.350(\\(b\\)\\((3|7|8)\\)|\\(a\\)\\(2\\)|\\(d\\)\\(1\\))",
                   "^75\\.351\\((b\\)\\(2|e|q)\\)",
                   "^75\\.352\\((f|g)\\)",
                   "^75\\.371\\((jj|mm|nn|yy)\\)",
                   "^75\\.380\\((d\\)\\(7\\)\\((v|vi|f|vii)|f\\)\\(1)\\)",
                   "^75\\.381\\((c\\)\\(5\\)\\((v|vi|vii)|e)\\)",
                   "^75\\.1103\\-4\\((a|b)\\)",
                   "^75\\.1103\\-5\\((a|d|e|f|g|h)\\)",
                   "^75\\.1103\\-(6|8|10)",
                   "^75\\.1108",
                   "^75\\.1731"
                   )

# Sealing of Abandoned Areas
# https://www.federalregister.gov/documents/2008/05/14/E8-10662/sealing-of-abandoned-areas
# Effective Date: The corrections are effective May 14, 2008. 
sealing_05142008_regex <- c("^75\\.336\\((b\\)\\(1|c)\\)")

# Mine Rescue Team Equipment
# https://www.federalregister.gov/documents/2008/09/15/E8-21449/mine-rescue-team-equipment
# This final rule is effective on November 14, 2008. 
rescue_equip_11142008_regex <- c("^49\\.6",
                   "^49\\.16")

# Mine Rescue Teams - pt 2
# The Mine Rescue Teams Rule, later revised, set new standards to improve 
# the training, effectiveness and response times of teams.
# https://www.federalregister.gov/documents/2009/06/17/E9-14128/mine-rescue-teams
# Effective Date: June 17, 2009.
# Compliance Date exceptions:
# 49.50(a), Table 49.50-A, by December 14, 2009
# 49.50(a), Table 49.50-B, by June 17, 2010
rescue_teams_2_06172009_regex <- c("^49\\.11\\(b\\)",
                   "^49\\.20\\(b\\)\\((1|4)\\)")

# Mine Rescue Teams - pt 1
# https://www.federalregister.gov/documents/2008/02/08/08-551/mine-rescue-teams
# Effective date: February 8, 2008. 
# Compliance Date exceptions:
# 49.12(h) by May 8, 2008
# 49.12(f) and 75.1501(a)(2) by August 8, 2008
# 49.40 by November 10, 2008
# 49.18(b), 49.20(a), 49.20(b), 49.30, and 49.50 by February 9, 2009
rescue_teams_1_02082008_regex <- c("^49\\.(11|12|13|14|15|16|17|18|19|20|30|40|50|60)",
                   "^75\\.1501\\(a\\)")

# Emergency Mine Evacuation
# An Emergency Mine Evacuation Rule required mine operators to increase the availability 
# of emergency breathing devices, improve emergency evacuation and drill training, 
# and install lifelines for emergency evacuation.
# It also required immediate notification of an accident to MSHA.
# https://www.federalregister.gov/documents/2006/12/08/06-9608/emergency-mine-evacuation
# Effective Date: This rule is effective December 8, 2006. 
# Compliance Date exceptions:
# 48.3 and 75.1502 no later than February 6, 2007 
# 75.1504 no later than March 31, 2007
# 75.1504(c)(3) MSHA will notify mine operators of the availability of realistic SCSR training units by publishing a notice in the Federal Register. 
# 75.1714-6 and 75.1714-7 no later than February 6, 2007
# 75.1714-8 no later than March 31, 2007
evacuations_12082006_regex <- c("^3\\.1",
                   "^48\\.3\\(p\\)",
                   "^48\\.5\\(b\\)\\((2|5)\\)",
                   "^48\\.6\\(b\\)\\((5|12)\\)",
                   "^48\\.8\\(b\\)\\((4|8)\\)",
                   "^48\\.11\\(a\\)\\(4\\)\\((i|ii)\\)",
                   "^50\\.2\\(h\\)\\((3|6)\\)",
                   "^50\\.10",
                   "^75\\.380\\(d\\)\\(7\\)",
                   "^75\\.381\\(c\\)\\(5\\)",
                   "^75\\.150[2345]",
                   "^75\\.1714\\(b\\)\\",
                   "^75\\.1714\\-2\\((f|g\\)\\(5)\\)",
                   "^75\\.1714\\-[45678]")

# Add Compliance Date exceptions
# https://www.federalregister.gov/documents/2006/12/08/06-9608/emergency-mine-evacuation
# https://www.federalregister.gov/documents/2008/02/08/08-551/mine-rescue-teams
# https://www.federalregister.gov/documents/2009/06/17/E9-14128/mine-rescue-teams
regex_lists <- list(refuge_03022009 = refuge_03022009_regex,
                    belts_fires_12312008 = belts_fires_12312008_regex,
                    sealing_05142008 = sealing_05142008_regex,
                    rescue_equip_11142008 = rescue_equip_11142008_regex,
                    rescue_teams_2_06172009 = rescue_teams_2_06172009_regex,
                    rescue_teams_1_02082008 = rescue_teams_1_02082008_regex,
                    evacuations_12082006 = evacuations_12082006_regex,
                    evacuations_02062007 = c("^48\\.3",
                                             "^75\\.1502",
                                             "^75\\.1714\\-[67]"),
                    evacuations_03312007 = c("^75\\.1504",
                                             "^75\\.1714\\-8"),
                    rescue_teams_05082008 = c("^49\\.12\\(a\\)"),
                    rescue_teams_08082008 = c("^49\\.12\\(f\\)",
                                              "^75\\.1501\\(a\\)\\(2\\)"),
                    rescue_teams_11102008 = c("^49\\.40"),
                    rescue_teams_02092009 = c("^49\\.(18\\(b\\)|20\\((a|b)\\)|30|50)")
                    )
violations_semi_cleaned <- violations_semi_cleaned %>%
  mutate(violation_miner_act = 0,
         ss_violation_miner_act = 0)
for (i in 1:length(regex_lists)) {
  regex_list <- regex_lists[[i]]
  var_name <- names(regex_lists)[i]
  print(var_name)
  violations_semi_cleaned <- violations_semi_cleaned %>%
    mutate(violation_miner_act = ifelse(grepl(paste(regex_list, collapse = "|"), PART_SECTION), 1, violation_miner_act),
           ss_violation_miner_act = ifelse(violation_miner_act == 1 & ss_violation == 1, 1, ss_violation_miner_act),
           temp_var = ifelse(grepl(paste(regex_list, collapse = "|"), PART_SECTION), 1, 0),
           temp_var_serious = ifelse(temp_var == 1 & ss_violation == 1, 1, 0))
  colnames(violations_semi_cleaned)[length(violations_semi_cleaned)-1] <- var_name
  colnames(violations_semi_cleaned)[length(violations_semi_cleaned)] <- paste0(var_name,"_ss")
}

# All violations
violations_cleaned_years <- violations_semi_cleaned %>%
  group_by(MINE_ID, year) %>%
  summarize(violations = n(), 
            ss_violations = sum(ss_violation, na.rm = TRUE),
            
            violations_miner_act = sum(violation_miner_act, na.rm = TRUE),
            ss_violations_miner_act = sum(ss_violation_miner_act, na.rm = TRUE))

violations_cleaned_quarters <- violations_semi_cleaned %>%
  group_by(MINE_ID, year, quarter) %>%
  summarize(violations = n(), 
            ss_violations = sum(ss_violation, na.rm = TRUE),
            
            violations_miner_act = sum(violation_miner_act, na.rm = TRUE),
            ss_violations_miner_act = sum(ss_violation_miner_act, na.rm = TRUE))
            
            # violations_refuge_03022009 = sum(refuge_03022009, na.rm = TRUE),
            # violations_refuge_03022009_serious = sum(refuge_03022009_serious, na.rm = TRUE),
            # 
            # violations_belts_fires_12312008 = sum(belts_fires_12312008, na.rm = TRUE),
            # violations_belts_fires_12312008_serious = sum(belts_fires_12312008_serious, na.rm = TRUE),
            # 
            # violations_sealing_05142008 = sum(sealing_05142008, na.rm = TRUE),
            # violations_sealing_05142008_serious = sum(sealing_05142008_serious, na.rm = TRUE),
            # 
            # violations_rescue_equip_11142008 = sum(rescue_equip_11142008, na.rm = TRUE),
            # violations_rescue_equip_11142008_serious = sum(rescue_equip_11142008_serious, na.rm = TRUE),
            # 
            # violations_rescue_teams_2_06172009 = sum(rescue_teams_2_06172009, na.rm = TRUE),
            # violations_rescue_teams_2_06172009_serious = sum(rescue_teams_2_06172009_serious, na.rm = TRUE),
            # 
            # violations_rescue_teams_1_02082008 = sum(rescue_teams_1_02082008, na.rm = TRUE),
            # violations_rescue_teams_1_02082008_serious = sum(rescue_teams_1_02082008_serious, na.rm = TRUE),
            # 
            # violations_evacuations_12082006 = sum(evacuations_12082006, na.rm = TRUE),
            # violations_evacuations_12082006_serious = sum(evacuations_12082006_serious, na.rm = TRUE),
            # 
            # violations_evacuations_02062007 = sum(evacuations_02062007, na.rm = TRUE),
            # violations_evacuations_02062007_serious = sum(evacuations_02062007_serious, na.rm = TRUE),
            # 
            # violations_evacuations_03312007 = sum(evacuations_03312007, na.rm = TRUE),
            # violations_evacuations_03312007_serious = sum(evacuations_03312007_serious, na.rm = TRUE),
            # 
            # violations_rescue_teams_05082008 = sum(rescue_teams_05082008, na.rm = TRUE),
            # violations_rescue_teams_05082008_serious = sum(rescue_teams_05082008_serious, na.rm = TRUE),
            # 
            # violations_rescue_teams_08082008 = sum(rescue_teams_08082008, na.rm = TRUE),
            # violations_rescue_teams_08082008_serious = sum(rescue_teams_08082008_serious, na.rm = TRUE),
            # 
            # violations_rescue_teams_11102008 = sum(rescue_teams_11102008, na.rm = TRUE),
            # violations_rescue_teams_11102008_serious = sum(rescue_teams_11102008_serious, na.rm = TRUE),
            # 
            # violations_rescue_teams_02092009 = sum(rescue_teams_02092009, na.rm = TRUE),
            # violations_rescue_teams_02092009_serious = sum(rescue_teams_02092009_serious, na.rm = TRUE))

##----------##
# contestedviolations_18 - done
##----------##
contested_violations_years <- contestedviolations_18 %>%
  mutate(contest_date = mdy(CONTEST_DT),
         year = year(contest_date)) %>%
  group_by(MINE_ID, year) %>%
  summarize(contested_violations = n(),
            avg_proposed_penalty_amount = mean(PROPOSED_PENALTY_AMT, na.rm = TRUE))

contested_violations_quarters <- contestedviolations_18 %>%
  mutate(contest_date = mdy(CONTEST_DT),
         year = year(contest_date),
         quarter = quarter(contest_date)) %>%
  group_by(MINE_ID, year, quarter) %>%
  summarize(contested_violations = n(),
            avg_proposed_penalty_amount = mean(PROPOSED_PENALTY_AMT, na.rm = TRUE))

##----------##
# assessedviolations_19 - done
##----------##
# Penalty points: Thousands of penalty points in the previous calendar year (for fatality models) or previous four quarters (for non-fatality models)
penalty_points_years <- assessedviolations_19 %>%
  mutate(violation_date = mdy(OCCURRENCE_DT),
         year = year(violation_date)) %>%
  group_by(MINE_ID, year) %>%
  summarize(penalty_points = sum(PENALTY_POINTS, na.rm = TRUE)) %>%
  group_by(MINE_ID) %>%
  arrange(MINE_ID, year) %>%
  mutate(penalty_points_previousyear = lag(penalty_points)) %>%
  ungroup()

penalty_points_quarters <- assessedviolations_19 %>%
  mutate(violation_date = mdy(OCCURRENCE_DT),
         year = year(violation_date),
         quarter = quarter(violation_date)) %>%
  group_by(MINE_ID, year, quarter) %>%
  summarize(penalty_points = sum(PENALTY_POINTS, na.rm = TRUE)) %>%
  group_by(MINE_ID) %>%
  arrange(MINE_ID, year, quarter) %>%
  mutate(penalty_points_previousfourquarters = lag(penalty_points) + lag(lag(penalty_points)) + lag(lag(lag(penalty_points))) + lag(lag(lag(lag(penalty_points))))) %>%
  ungroup()

##----------##
# Combine into single dataset - done
##----------##
msha_panel_years <- accidents_cleaned_years %>%
  full_join(controller_operator_panel_years, by = c('MINE_ID', 'year')) %>%
  full_join(violations_cleaned_years, by = c('MINE_ID', 'year')) %>%
  full_join(production_cleaned_years, by = c('MINE_ID', 'year')) %>%
  full_join(contested_violations_years, by = c('MINE_ID', 'year')) %>%
  full_join(penalty_points_years, by = c('MINE_ID', 'year')) %>%
  full_join(mines_cleaned, by = c('MINE_ID'))

write_csv(msha_panel_years, file.path(ddir, "cleaned", "msha_panel_years.csv"))

msha_panel_quarters <- accidents_cleaned_quarters %>%
  full_join(controller_operator_panel_quarters, by = c('MINE_ID', 'year', 'quarter')) %>%
  full_join(violations_cleaned_quarters, by = c('MINE_ID', 'year', 'quarter')) %>%
  full_join(production_cleaned_quarters, by = c('MINE_ID', 'year', 'quarter')) %>%
  full_join(contested_violations_quarters, by = c('MINE_ID', 'year', 'quarter')) %>%
  full_join(penalty_points_quarters, by = c('MINE_ID', 'year', 'quarter')) %>%
  full_join(mines_cleaned, by = c('MINE_ID'))

write_csv(msha_panel_quarters, file.path(ddir, "cleaned", "msha_panel_quarters.csv"))

