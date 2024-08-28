# Create summary statistic figures

# Header ------------------------------------------------------------------

rm(list = ls())

root <- getwd()
while(basename(root) != "coal-mining") root <- dirname(root)

source(file.path(root, "scripts", "header_script.R"))

prop_functions <- new.env()
source(file.path(root, "scripts", "prop_functions.R"), local = prop_functions)

# Globals and file paths --------------------------------------------------

source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")

input <- file.path(ddir, "Output")

# Prepare data ------------------------------------------------------------

mine_quarter_panel <- haven::read_dta(file.path(input, "03 mine-quarter panel, union safety analysis.dta")) %>%
  dplyr::mutate(MINE_ID_active_less_30pct = as.double(((MINE_ID_active_qtrs / max_possible_active_qtrs) < .30)),
                traum_inj_per_ss_viol = ifelse(ss_violations > 0, 
                                               traumatic_injuries / ss_violations,
                                               NA)) %T>%
  dplyr::glimpse()

# Distinct mines
mine_quarter_panel %>%
  dplyr::select(MINE_ID) %>%
  dplyr::distinct() %>%
  nrow()

# Maps --------------------------------------------------------------------

map_df <- mine_quarter_panel %>%
  dplyr::select(MINE_ID, longitude, latitude, msha_office_code, district) %>%
  dplyr::distinct() %>%
  dplyr::filter(!is.na(latitude),
                !is.na(longitude),
                latitude < 50 & latitude > 30,
                latitude < 45 | longitude < -90) %>%
  usmap::usmap_transform(input_names = c("longitude", "latitude"),
                         output_names = c("x", "y")) %T>%
  dplyr::glimpse()

map_district <- usmap::plot_usmap(exclude = c("AK", "HI")) + 
  geom_point(data = map_df, 
             aes(x = x, 
                 y = y,
                 color = district), 
             size = 1,
             alpha = 0.5,
             show.legend = FALSE)
prop_functions$prop_save(map_district, file.path("../../output/figures/summary_stat_figures", "map_district"))

map_msha_office <- usmap::plot_usmap(exclude = c("AK", "HI")) + 
  geom_point(data = map_df, 
             aes(x = x, 
                 y = y,
                 color = msha_office_code), 
             size = 1,
             alpha = 0.5,
             show.legend = FALSE)
prop_functions$prop_save(map_msha_office, file.path("../../output/figures/summary_stat_figures", "map_msha_office"))

# Distributions of key variables (mine-quarter observation level) ---------

# Safety in union vs. nonunion mines
distn_union_traum_inj <- ggplot(mine_quarter_panel %>%
         dplyr::mutate(union = case_when(union == 0 ~ "Nonunion",
                                         union == 1 ~ "Union")) %>%
         dplyr::filter(traumatic_injury_rate <= quantile(traumatic_injury_rate, .99),
                       FTEs > 1), 
       aes(traumatic_injury_rate,
           fill = as.factor(union),
           color = as.factor(union))) +
  geom_density(size = 1,
               alpha = 0.1) +
  scale_fill_manual(labels = c("Nonunion", "Union"),
                    values = c("#014d64", "darkred")) +
  scale_color_manual(labels = c("Nonunion", "Union"),
                     values = c("#014d64", "darkred")) +
  labs(x = "Traumatic Injuries per 2,000 Hours Worked", 
       y = "Density") +
  prop_functions$prop_theme()
prop_functions$prop_save(distn_union_traum_inj, file.path("../../output/figures/summary_stat_figures", "distn_union_traum_inj"))

distn_union_ss_viol <- ggplot(mine_quarter_panel %>%
         dplyr::mutate(union = case_when(union == 0 ~ "Nonunion",
                                         union == 1 ~ "Union")) %>%
         dplyr::filter(ss_violation_rate <= quantile(ss_violation_rate, .99),
                       FTEs > 1), 
       aes(ss_violation_rate,
           fill = as.factor(union),
           color = as.factor(union))) +
  geom_density(size = 1,
               alpha = 0.1) +
  scale_fill_manual(labels = c("Nonunion", "Union"),
                    values = c("#014d64", "darkred")) +
  scale_color_manual(labels = c("Nonunion", "Union"),
                     values = c("#014d64", "darkred")) +
  labs(x = "S&S Violations per 2,000 Hours Worked", 
       y = "Density") +
  prop_functions$prop_theme()
prop_functions$prop_save(distn_union_ss_viol, file.path("../../output/figures/summary_stat_figures", "distn_union_ss_viol"))

# Safety in 90 pct vs. less than 30 pct active mines
safety_active_quarters_df <- mine_quarter_panel %>%
  dplyr::mutate(group_var = ifelse(MINE_ID_active_90pct,
                                   "Mines Active 90 Pct\nof Qtrs 2000-2022",
                                   NA),
                group_var = ifelse(MINE_ID_active_less_30pct,
                                   "Mines Active Less Than 30 Pct\nof Qtrs 2000-2022",
                                   group_var)) %>%
  dplyr::filter(!is.na(group_var)) %T>%
  dplyr::glimpse()

distn_lifetime_traum_inj <- ggplot(safety_active_quarters_df %>%
         dplyr::filter(traumatic_injury_rate <= quantile(traumatic_injury_rate, .99),
                       FTEs > 1), 
       aes(traumatic_injury_rate,
           fill = as.factor(group_var),
           color = as.factor(group_var))) +
  geom_density(size = 1,
               alpha = 0.1) +
  scale_fill_manual(labels = c("Mines Active Less Than 30 Pct\nof Qtrs 2000-2022", "Mines Active 90 Pct\nof Qtrs 2000-2022"),
                    values = c("#018571", "#dfc27d")) +
  scale_color_manual(labels = c("Mines Active Less Than 30 Pct\nof Qtrs 2000-2022", "Mines Active 90 Pct\nof Qtrs 2000-2022"),
                     values = c("#018571", "#dfc27d")) +
  labs(x = "Traumatic Injuries per 2,000 Hours Worked", 
       y = "Density") +
  prop_functions$prop_theme()
prop_functions$prop_save(distn_lifetime_traum_inj, file.path("../../output/figures/summary_stat_figures", "distn_lifetime_traum_inj"))

distn_lifetime_ss_viol <- ggplot(safety_active_quarters_df %>%
         dplyr::filter(ss_violation_rate <= quantile(ss_violation_rate, .99),
                       FTEs > 1), 
       aes(ss_violation_rate,
           fill = as.factor(group_var),
           color = as.factor(group_var))) +
  geom_density(size = 1,
               alpha = 0.1) +
  scale_fill_manual(labels = c("Mines Active Less Than 30 Pct\nof Qtrs 2000-2022", "Mines Active 90 Pct\nof Qtrs 2000-2022"),
                    values = c("#018571", "#dfc27d")) +
  scale_color_manual(labels = c("Mines Active Less Than 30 Pct\nof Qtrs 2000-2022", "Mines Active 90 Pct\nof Qtrs 2000-2022"),
                     values = c("#018571", "#dfc27d")) +
  labs(x = "S&S Violations per 2,000 Hours Worked", 
       y = "Density") +
  prop_functions$prop_theme()
prop_functions$prop_save(distn_lifetime_ss_viol, file.path("../../output/figures/summary_stat_figures", "distn_lifetime_ss_viol"))

# Number of mines and mine-quarters per group (union, operation lifetime)
mine_quarter_panel %>%
  dplyr::mutate(non_zero_ss_viols = (ss_violations > 0),
                non_zero_traum_inj = (traumatic_injuries > 0),
                total_mine_quarters = 1) %>%
  dplyr::summarize(total_mine_quarters = sum(total_mine_quarters),
                   union = sum(union),
                   non_zero_ss_viols = sum(non_zero_ss_viols),
                   non_zero_traum_inj = sum(non_zero_traum_inj),
                   MINE_ID_active_90pct = sum(MINE_ID_active_90pct),
                   MINE_ID_active_less_30pct = sum(MINE_ID_active_less_30pct))

# Injuries per violation
distn_inj_per_viol <- ggplot(mine_quarter_panel %>%
         dplyr::filter(traum_inj_per_ss_viol <= quantile(traum_inj_per_ss_viol, .99, na.rm = TRUE),
                       FTEs > 1), 
       aes(traum_inj_per_ss_viol)) +
  geom_density(size = 1,
               alpha = 0.1,
               color = "#014d64",
               fill = "#014d64") +
  labs(x = "Traumatic Injuries per S&S Violation | S&S Violations > 0", 
       y = "Density") +
  prop_functions$prop_theme()
prop_functions$prop_save(distn_inj_per_viol, file.path("../../output/figures/summary_stat_figures", "distn_inj_per_viol"))

distn_inj_per_viol_lifetime <- ggplot(safety_active_quarters_df %>%
         dplyr::filter(traum_inj_per_ss_viol <= quantile(traum_inj_per_ss_viol, .99, na.rm = TRUE),
                       FTEs > 1), 
       aes(traum_inj_per_ss_viol,
           fill = as.factor(group_var),
           color = as.factor(group_var))) +
  geom_density(size = 1,
               alpha = 0.1) +
  scale_fill_manual(labels = c("Mines Active Less Than 30 Pct\nof Qtrs 2000-2022", "Mines Active 90 Pct\nof Qtrs 2000-2022"),
                    values = c("#018571", "#dfc27d")) +
  scale_color_manual(labels = c("Mines Active Less Than 30 Pct\nof Qtrs 2000-2022", "Mines Active 90 Pct\nof Qtrs 2000-2022"),
                     values = c("#018571", "#dfc27d")) +
  labs(x = "Traumatic Injuries per S&S Violation | S&S Violations > 0", 
       y = "Density") +
  prop_functions$prop_theme()
prop_functions$prop_save(distn_inj_per_viol_lifetime, file.path("../../output/figures/summary_stat_figures", "distn_inj_per_viol_lifetime"))
