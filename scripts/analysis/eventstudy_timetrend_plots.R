# Last updated: Mar 25, 2023

root <- getwd()
while(basename(root) != "coal-mining") { # this is the name of your project directory you want to use
  root <- dirname(root)
}
source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")
outputdir <- file.path(dir, "coal-mining", "output", "figures")

library(tidyverse)
library(texreg)
library(mfx)
library(estimatr)
library(did)
library(ggthemes)
library(readxl)
library(usmap)

# https://arlweb.msha.gov/stats/centurystats/coalstats.asp
coal_fatalities <- read_xlsx(file.path(ddir, "MSHA", "coal_fatalities_1900_2022.xlsx")) %>%
  mutate(fatality_rate = 2000*(Fatalities/Miners))

mine_panel_quarters_import <- read_csv(file.path(ddir, "cleaned", "mine_panel_quarters.csv"))
mine_panel_years_import <- read_csv(file.path(ddir, "cleaned", "mine_panel_years.csv"))

mines_after_2015_df <- mine_panel_quarters_import %>%
  dplyr::filter(zero_production_quarter == 0, year >= 2015) %>%
  dplyr::select(MINE_ID) %>%
  distinct() %>%
  mutate(active_after2015 = 1)

mine_panel_quarters <- mine_panel_quarters_import %>%
  left_join(mines_after_2015_df, by = "MINE_ID") %>%
  mutate(contested_violations = ifelse(is.na(contested_violations), 0, contested_violations)) %>%
  #dplyr::filter(zero_production_quarter == 0) %>%
  group_by(year_quarter, county_fips) %>%
  mutate(active_mines = n()) %>%
  ungroup() %>%
  group_by(year_quarter, county_fips, CONTROLLER_ID) %>%
  mutate(same_controller = n(),
         controller_size_100employees_county = sum(size_100employees, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(pct_same_controller_county = same_controller/active_mines) %>%
  group_by(year_quarter, county_fips) %>%
  mutate(largest_pct_controller = max(pct_same_controller_county, na.rm = TRUE),
         size_100employees_county = sum(size_100employees, na.rm = TRUE),
         controller_share_county = 100*controller_size_100employees_county/size_100employees_county,
         hhi_county = sum(controller_share_county^2, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year_quarter, CONTROLLER_ID) %>%
  mutate(controller_size_100employees_natl = sum(size_100employees, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(year_quarter) %>%
  mutate(size_100employees_natl = sum(size_100employees, na.rm = TRUE),
         controller_share_natl = 100*controller_size_100employees_natl/size_100employees_natl,
         hhi_natl = sum(controller_share_natl^2, na.rm = TRUE)) %>%
  ungroup()

mine_panel_years <- mine_panel_years_import

##----------##
# Event studies
##----------##

y_var <- ...
for (t in seq(from = 0, to = 15, by = 2)) {
  min_year <- 2000 + t
  max_year <- min_year + 5
  
  mine_panel_quarters_first_treatment <- mine_panel_quarters %>%
    filter(controller_change == 1,
           year >= min_year,
           year <= max_year) %>%
    group_by(MINE_ID) %>%
    summarize(first_controller_change_year_quarter = min(year_quarter, na.rm = TRUE))
  mine_panel_quarters_es <- mine_panel_quarters %>%
    left_join(mine_panel_quarters_first_treatment, by = "MINE_ID") %>%
    mutate(first_controller_change_year_quarter = ifelse(is.na(first_controller_change_year_quarter), 
                                                         0, first_controller_change_year_quarter)) %>%
    filter(year >= min_year,
           year <= max_year)
  out <- att_gt(yname = y_var,
                gname = "first_controller_change_year_quarter",
                idname = "MINE_ID",
                tname = "year_quarter",
                xformla = ~1,
                data = mine_panel_quarters_es,
                est_method = "reg",
                allow_unbalanced_panel = TRUE
  )
  es <- aggte(out, type = "dynamic", na.rm = TRUE)
  
  name <- paste("plot", t, sep = "_")
  p <- ggdid(es) +
    ggtitle(name)
  assign(name, p)
}
plot_0
plot_2
plot_4
plot_6
plot_8
plot_10
plot_12
plot_14

# Also tried on the LHS:
# union -
# violation_rate - 
# ss_violation_rate -
# productivity -

# traumatic_injury_rate ~ operator_change - saved
# traum_inj_rate_quarter_es2 <- plot_2 +
#   xlim(-1.1, 1.3) +
#   ggtitle("Event study of first operator change: traumatic injury rate (2002-2007, quarters)")
# ggsave(file.path(outputdir, 'traum_inj_rate_quarter_es2.png'), 
#        plot = traum_inj_rate_quarter_es2, width = 8, height = 6)

# avg_employee_count ~ controller_change - saved
# avg_employee_count_quarter_es2 <- plot_2 +
#   xlim(-1.1, 1.1) +
#   ggtitle("Event study of first controller change: average mine employee count (2002-2007, quarters)")
# ggsave(file.path(outputdir, 'avg_employee_count_quarter_es2.png'), 
#        plot = avg_employee_count_quarter_es2, width = 8, height = 6)

# zero_production_quarter ~ controller_change - saved 
# zero_production_quarter_es0 <- plot_0 +
#   xlim(-1.1, 1.1) +
#   ggtitle("Event study of first controller change: zero-production quarter (2000-2005, quarters)")
# ggsave(file.path(outputdir, 'zero_production_quarter_es0.png'), 
#        plot = zero_production_quarter_es0, width = 8, height = 6)
# 
# zero_production_quarter_es4 <- plot_4 +
#   xlim(-1.1, 1.1) +
#   ggtitle("Event study of first controller change: zero-production quarter (2004-2009, quarters)")
# ggsave(file.path(outputdir, 'zero_production_quarter_es4.png'), 
#        plot = zero_production_quarter_es4, width = 8, height = 6)
# 
# zero_production_quarter_es6 <- plot_6 +
#   xlim(-1.1, 1.1) +
#   ggtitle("Event study of first controller change: zero-production quarter (2006-2011, quarters)")
# ggsave(file.path(outputdir, 'zero_production_quarter_es6.png'), 
#        plot = zero_production_quarter_es6, width = 8, height = 6)

# hhi_county ~ controller_change - saved
# hhi_county_quarter_es2 <- plot_2 +
#   xlim(-1.1, 2.1) +
#   ggtitle("Event study of first controller change: county HHI (2002-2007, quarters)")
# ggsave(file.path(outputdir, 'hhi_county_quarter_es2.png'), 
#        plot = hhi_county_quarter_es2, width = 8, height = 6)
# 
# hhi_county_quarter_es4 <- plot_4 +
#   xlim(-1.1, 2.1) +
#   ggtitle("Event study of first controller change: county HHI (2004-2009, quarters)")
# ggsave(file.path(outputdir, 'hhi_county_quarter_es4.png'), 
#        plot = hhi_county_quarter_es4, width = 8, height = 6)
# 
# hhi_county_quarter_es6 <- plot_6 +
#   xlim(-1.1, 2.1) +
#   ggtitle("Event study of first controller change: county HHI (2006-2011, quarters)")
# ggsave(file.path(outputdir, 'hhi_county_quarter_es6.png'), 
#        plot = hhi_county_quarter_es6, width = 8, height = 6)
# 
# hhi_county_quarter_es8 <- plot_8 +
#   xlim(-1.1, 2.1) +
#   ggtitle("Event study of first controller change: county HHI (2008-2013, quarters)")
# ggsave(file.path(outputdir, 'hhi_county_quarter_es8.png'), 
#        plot = hhi_county_quarter_es8, width = 8, height = 6)
# 
# hhi_county_quarter_es10 <- plot_10 +
#   xlim(-1.1, 2.1) +
#   ggtitle("Event study of first controller change: county HHI (2010-2015, quarters)")
# ggsave(file.path(outputdir, 'hhi_county_quarter_es10.png'), 
#        plot = hhi_county_quarter_es10, width = 8, height = 6)

##----------##
# Time trend plots
##----------##

time_trend_df <- mine_panel_quarters %>%
  filter(year >= 2000,
         year <= 2021,
         zero_production_quarter == 0) %>%
  mutate(violations_nonminer_act = violations - violations_miner_act,
         ss_violations_nonminer_act = ss_violations - ss_violations_miner_act) %>%
  group_by(year_quarter, union) %>%
  summarize(total_injury_rate = 2000*(sum(total_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            traumatic_injury_rate = 2000*(sum(traumatic_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            violation_rate = 2000*(sum(violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            ss_violation_rate = 2000*(sum(ss_violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            violation_miner_act_rate = 2000*(sum(violations_miner_act, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            ss_violation_miner_act_rate = 2000*(sum(ss_violations_miner_act, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            violation_nonminer_act_rate = 2000*(sum(violations_nonminer_act, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            ss_violation_nonminer_act_rate = 2000*(sum(ss_violations_nonminer_act, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            labor_hours = sum(labor_hours, na.rm = TRUE),
            total_avg_employee_count = sum(avg_employee_count, na.rm = TRUE),
            coal_production_tons = sum(coal_production_tons, na.rm = TRUE),
            productivity = 2000*(coal_production_tons/labor_hours),
            operator_changes = sum(operator_change, na.rm = TRUE),
            controller_changes = sum(controller_change, na.rm = TRUE),
            active_mines = n())

violations_miner_act_DID <- time_trend_df %>%
  mutate(violation_rate = ifelse(union == 0,
                                 violation_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 0)$violation_rate,
                                 violation_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 1)$violation_rate),
         violation_miner_act_rate = ifelse(union == 0,
                                           violation_miner_act_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 0)$violation_miner_act_rate,
                                           violation_miner_act_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 1)$violation_miner_act_rate),
         violation_nonminer_act_rate = ifelse(union == 0,
                                              violation_nonminer_act_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 0)$violation_nonminer_act_rate,
                                              violation_nonminer_act_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 1)$violation_nonminer_act_rate),
         ss_violation_rate = ifelse(union == 0,
                                    ss_violation_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 0)$ss_violation_rate,
                                    ss_violation_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 1)$ss_violation_rate),
         ss_violation_miner_act_rate = ifelse(union == 0,
                                              ss_violation_miner_act_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 0)$ss_violation_miner_act_rate,
                                              ss_violation_miner_act_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 1)$ss_violation_miner_act_rate),
         ss_violation_nonminer_act_rate = ifelse(union == 0,
                                                 ss_violation_nonminer_act_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 0)$ss_violation_nonminer_act_rate,
                                                 ss_violation_nonminer_act_rate - filter(time_trend_df, year_quarter == 2006.75 & union == 1)$ss_violation_nonminer_act_rate))

time_trend_df_after2015 <- mine_panel_quarters %>%
  filter(year >= 2000,
         year <= 2021,
         zero_production_quarter == 0,
         active_after2015 == 1) %>%
  mutate(violations_nonminer_act = violations - violations_miner_act,
         ss_violations_nonminer_act = ss_violations - ss_violations_miner_act) %>%
  group_by(year_quarter, union) %>%
  summarize(total_injury_rate = 2000*(sum(total_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            traumatic_injury_rate = 2000*(sum(traumatic_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            violation_rate = 2000*(sum(violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            ss_violation_rate = 2000*(sum(ss_violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            violation_miner_act_rate = 2000*(sum(violations_miner_act, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            ss_violation_miner_act_rate = 2000*(sum(ss_violations_miner_act, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            violation_nonminer_act_rate = 2000*(sum(violations_nonminer_act, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            ss_violation_nonminer_act_rate = 2000*(sum(ss_violations_nonminer_act, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            labor_hours = sum(labor_hours, na.rm = TRUE),
            total_avg_employee_count = sum(avg_employee_count, na.rm = TRUE),
            coal_production_tons = sum(coal_production_tons, na.rm = TRUE),
            productivity = 2000*(coal_production_tons/labor_hours),
            operator_changes = sum(operator_change, na.rm = TRUE),
            controller_changes = sum(controller_change, na.rm = TRUE),
            active_mines = n())

violations_miner_act_DID_active_after2015 <- time_trend_df_after2015 %>%
  mutate(violation_rate = ifelse(union == 0,
                                 violation_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 0)$violation_rate,
                                 violation_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 1)$violation_rate),
         violation_miner_act_rate = ifelse(union == 0,
                                           violation_miner_act_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 0)$violation_miner_act_rate,
                                           violation_miner_act_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 1)$violation_miner_act_rate),
         violation_nonminer_act_rate = ifelse(union == 0,
                                              violation_nonminer_act_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 0)$violation_nonminer_act_rate,
                                              violation_nonminer_act_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 1)$violation_nonminer_act_rate),
         ss_violation_rate = ifelse(union == 0,
                                    ss_violation_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 0)$ss_violation_rate,
                                    ss_violation_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 1)$ss_violation_rate),
         ss_violation_miner_act_rate = ifelse(union == 0,
                                              ss_violation_miner_act_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 0)$ss_violation_miner_act_rate,
                                              ss_violation_miner_act_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 1)$ss_violation_miner_act_rate),
         ss_violation_nonminer_act_rate = ifelse(union == 0,
                                                 ss_violation_nonminer_act_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 0)$ss_violation_nonminer_act_rate,
                                                 ss_violation_nonminer_act_rate - filter(time_trend_df_after2015, year_quarter == 2006.75 & union == 1)$ss_violation_nonminer_act_rate))

hhi_time_trend_natl <- mine_panel_quarters %>%
  group_by(year_quarter, CONTROLLER_ID) %>%
  summarize(controller_size_100employees_natl = sum(size_100employees, na.rm = TRUE)) %>%
  group_by(year_quarter) %>%
  mutate(controller_share_natl = 100*controller_size_100employees_natl/sum(controller_size_100employees_natl, na.rm = TRUE)) %>%
  summarize(hhi_natl = sum(controller_share_natl^2, na.rm = TRUE))

hhi_time_trend_df <- mine_panel_quarters %>%
  group_by(year_quarter, county_fips, CONTROLLER_ID) %>%
  summarize(controller_size_100employees_county = sum(size_100employees, na.rm = TRUE)) %>%
  group_by(year_quarter, county_fips) %>%
  mutate(controller_share_county = 100*controller_size_100employees_county/sum(controller_size_100employees_county, na.rm = TRUE)) %>%
  summarize(hhi_county = sum(controller_share_county^2, na.rm = TRUE),
            county_size_100employees = sum(controller_size_100employees_county, na.rm = TRUE)) %>%
  group_by(year_quarter) %>%
  summarize(hhi_county_avg = sum(hhi_county*county_size_100employees, na.rm = TRUE)/sum(county_size_100employees, na.rm = TRUE)) %>%
  left_join(hhi_time_trend_natl, by = "year_quarter")
  
# HHI (local vs. national) - saved
hhi_time_trend_fig <- ggplot(data = hhi_time_trend_df, aes(x = year_quarter)) +
  geom_hline(yintercept = 2500, color = "gray") +
  annotate("text", x = 2003, y = 2600, label = "HHI = 2,500", color = "gray", hjust = 1, vjust = 0) +
  geom_line(aes(y = hhi_county_avg, 
                color = "County mean (weighted by num. miners)"), linetype = 5) +
  geom_line(aes(y = hhi_natl, 
                color = "National"), linetype = 2) +
  scale_y_continuous() +
  scale_x_continuous(breaks = seq(1900,2021,10)) +
  scale_color_manual(values = c(
    "County mean (weighted by num. miners)" = '#014d64',
    "National" = 'darkred')) +
  ylab("HHI") +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) +
  xlab("Quarter") +
  labs(color = "") +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "longdash")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(face = "italic")) +
  ggtitle('Underground, Bituminous Coal Mine Labor Market HHI, Local and National: 2000-2021')
ggsave(file.path(outputdir, 'hhi_time_trend_fig.png'), 
       plot = hhi_time_trend_fig, width = 8, height = 6)

# Fatalities - saved
coal_fatalities_fig <- ggplot(data = coal_fatalities, aes(x = Year)) +
  geom_line(aes(y = Fatalities, 
                color = "Fatality count"), linetype = 5) +
  geom_line(aes(y = 300*fatality_rate, 
                color = "Fatalities per 2,000 miners"), linetype = 2) +
  scale_y_continuous(breaks = seq(0,4000,500),
                     name = "Fatality count",
                     sec.axis = sec_axis(~ ./300, 
                                         breaks = seq(0,10,1),
                                         name = "Fatalities per 2,000 miners")) +
  scale_x_continuous(breaks = seq(1900,2021,10)) +
  scale_color_manual(values = c(
    "Fatality count" = '#014d64',
    "Fatalities per 2,000 miners" = 'darkred')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "longdash")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(face = "italic")) +
  ggtitle('Coal Fatalities: 1900-2021')
ggsave(file.path(outputdir, 'coal_fatalities.png'), 
       plot = coal_fatalities_fig, width = 8, height = 6)

# Maps of active coal mines (2000-2021, 2000, 2021) - saved
mine_geo_codes <- mine_panel_quarters %>%
  filter(year >= 2000,
         year <= 2021,
         zero_production_quarter == 0) %>%
  dplyr::select(longitude, latitude) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  distinct() %>%
  filter(latitude<50 & latitude >30,
         latitude < 45 | longitude < -90)
transformed_data <- usmap_transform(mine_geo_codes)
us_mines_map_2000_2021 <- plot_usmap() + 
  labs(title = "Active coal mines in sample, 2000-2021") + 
  geom_point(data = transformed_data, aes(x = longitude.1, y = latitude.1), color = "black", size = 0.5)
ggsave(file.path(outputdir, 'us_mines_map_2000_2021.png'), 
       plot = us_mines_map_2000_2021, width = 8, height = 6)

mine_geo_codes <- mine_panel_quarters %>%
  filter(year == 2000,
         zero_production_quarter == 0) %>%
  dplyr::select(longitude, latitude) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  distinct() %>%
  filter(latitude<50 & latitude >30,
         latitude < 45 | longitude < -90)
transformed_data <- usmap_transform(mine_geo_codes)
us_mines_map_2000 <- plot_usmap() + 
  labs(title = "Active coal mines in sample, 2000") + 
  geom_point(data = transformed_data, aes(x = longitude.1, y = latitude.1), color = "black", size = 0.5)
ggsave(file.path(outputdir, 'us_mines_map_2000.png'), 
       plot = us_mines_map_2000, width = 8, height = 6)

mine_geo_codes <- mine_panel_quarters %>%
  filter(year == 2021,
         zero_production_quarter == 0) %>%
  dplyr::select(longitude, latitude) %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  distinct() %>%
  filter(latitude<50 & latitude >30,
         latitude < 45 | longitude < -90)
transformed_data <- usmap_transform(mine_geo_codes)
us_mines_map_2021 <- plot_usmap() + 
  labs(title = "Active coal mines in sample, 2021") + 
  geom_point(data = transformed_data, aes(x = longitude.1, y = latitude.1), color = "black", size = 0.5)
ggsave(file.path(outputdir, 'us_mines_map_2021.png'), 
       plot = us_mines_map_2021, width = 8, height = 6)

# Active mines (union vs. nonunion); make Panel A in same figure with maps of active coal mines - saved
active_mines_union_fig <- ggplot(time_trend_df) + 
  geom_line(data=subset(time_trend_df, union == 0),aes(x=year_quarter,
                                                                            y=active_mines,
                                                                            color='Nonunion'), linetype = 5) +
  geom_line(data=subset(time_trend_df, union == 1),aes(x=year_quarter,
                                                                            y=active_mines,
                                                                            color='Union'), linetype = 2) +
  ylab('Number of active underground mines') +
  scale_y_continuous(breaks = seq(0,500,50)) +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "Nonunion" = '#014d64',
    "Union" = 'darkred')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "longdash")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Active Underground Bituminous Coal Mines: 2000-2021')
ggsave(file.path(outputdir, 'active_mines_union_fig.png'), 
       plot = active_mines_union_fig, width = 8, height = 6)

# Labor hours and number of mine employees - saved
underground_labor_hours_employees_fig <- ggplot(time_trend_df) +
  geom_line(data=subset(time_trend_df, union == 1), aes(x = year_quarter,
                                                        y = labor_hours/1000000,
                                                        color = "Labor Hrs: Union"), 
            linetype = 5) +
  geom_line(data=subset(time_trend_df, union == 0), aes(x = year_quarter,
                                                        y = labor_hours/1000000,
                                                        color = "Labor Hrs: Nonunion"), 
            linetype = 5) +
  geom_line(data=subset(time_trend_df, union == 1), aes(x = year_quarter,
                                                        y = total_avg_employee_count/1500,
                                                        color = "Num employees: Union"), 
            linetype = 1) +
  geom_line(data=subset(time_trend_df, union == 0), aes(x = year_quarter,
                                                        y = total_avg_employee_count/1500,
                                                        color = "Num employees: Nonunion"), 
            linetype = 1) +
  scale_y_continuous(breaks = seq(0,20,2.5),
                     name = "Total underground miner labor hours (millions)",
                     sec.axis = sec_axis(~ .*1.5, 
                                         breaks = seq(0,30,2),
                                         name = "Total average mine employees (thousands)")) +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "Labor Hrs: Union" = 'darkred',
    "Labor Hrs: Nonunion" = '#014d64',
    "Num employees: Union" = 'darkred',
    "Num employees: Nonunion" = '#014d64')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "dashed","solid", "solid")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(face = "italic")) +
  ggtitle('Labor Hours and Number of Mine Employees: 2000-2021')
ggsave(file.path(outputdir, 'underground_labor_hours_employees_fig.png'), 
       plot = underground_labor_hours_employees_fig, width = 8, height = 6)

# Controller and operator changes - saved
operator_controller_change_fig <- ggplot(time_trend_df) + 
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.5) +
  geom_line(data=subset(time_trend_df, union == 0),aes(x=year_quarter,
                                                               y=controller_changes,
                                                               color='Controller changes: Nonunion'), linetype = 1) +
  geom_line(data=subset(time_trend_df, union == 1),aes(x=year_quarter,
                                                       y=controller_changes,
                                                       color='Controller changes: Union'), linetype = 1) +
  geom_line(data=subset(time_trend_df, union == 0),aes(x=year_quarter,
                                                               y=operator_changes,
                                                               color='Operator changes: Nonunion'), linetype = 5) +
  geom_line(data=subset(time_trend_df, union == 1),aes(x=year_quarter,
                                                               y=operator_changes,
                                                               color='Operator changes: Union'), linetype = 5) +
  ylab('Number of changes of controller, operator') +
  scale_y_continuous(breaks = seq(0,80,5)) +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "Controller changes: Nonunion" = '#014d64',
    "Controller changes: Union" = 'darkred',
    "Operator changes: Nonunion" = '#014d64',
    "Operator changes: Union" = 'darkred')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid","longdash", "longdash")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Mine Controller and Operator Changes: 2000-2021')
ggsave(file.path(outputdir, 'operator_controller_change_fig.png'), 
       plot = operator_controller_change_fig, width = 10, height = 6)

# Production and productivity - saved
productivity_fig <- ggplot(time_trend_df) +
  geom_line(data=subset(time_trend_df, union == 1), aes(x = year_quarter,
                                                                y = coal_production_tons/1000000,
                                                                color = "Production: Union"), 
            linetype = 5) +
  geom_line(data=subset(time_trend_df, union == 0), aes(x = year_quarter,
                                                                y = coal_production_tons/1000000,
                                                                color = "Production: Nonunion"), 
            linetype = 5) +
  geom_line(data=subset(time_trend_df, union == 1), aes(x = year_quarter,
                                                                y = productivity/400,
                                                                color = "Productivity: Union"), 
            linetype = 1) +
  geom_line(data=subset(time_trend_df, union == 0), aes(x = year_quarter,
                                                                y = productivity/400,
                                                                color = "Productivity: Nonunion"), 
            linetype = 1) +
  scale_y_continuous(name = "Production (total short tons mined underground, millions)",
                     breaks = seq(0,50,5),
                     sec.axis = sec_axis(~ .*400, 
                                         breaks = seq(0,20000,4000),
                                         name = "Productivity (2000*short tons mined underground/underground labor hours)")) +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "Production: Union" = 'darkred',
    "Production: Nonunion" = '#014d64',
    "Productivity: Union" = 'darkred',
    "Productivity: Nonunion" = '#014d64')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("dashed", "dashed","solid", "solid")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(face = "italic")) +
  ggtitle('Mine Production and Labor Productivity: 2000-2021')
ggsave(file.path(outputdir, 'productivity_fig.png'), 
       plot = productivity_fig, width = 8, height = 6)

# Rates of total and traumatic injuries (union vs. nonunion) - saved
total_traum_inj_union_fig <- ggplot(time_trend_df) + 
  geom_line(data=subset(time_trend_df, union == 0),aes(x=year_quarter,
                                                               y=traumatic_injury_rate,
                                                               color='Traum. Inj. Rate: Nonunion'), linetype = 1) +
  geom_line(data=subset(time_trend_df, union == 1),aes(x=year_quarter,
                                                       y=traumatic_injury_rate,
                                                       color='Traum. Inj. Rate: Union'), linetype = 1) +
  geom_line(data=subset(time_trend_df, union == 0),aes(x=year_quarter,
                                                               y=total_injury_rate,
                                                               color='Total Inj. Rate: Nonunion'), linetype = 5) +
  geom_line(data=subset(time_trend_df, union == 1),aes(x=year_quarter,
                                                               y=total_injury_rate,
                                                               color='Total Inj. Rate: Union'), linetype = 5) +
  ylab('Injuries per 2,000 hours worked') +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "Traum. Inj. Rate: Nonunion" = '#014d64',
    "Traum. Inj. Rate: Union" = 'darkred',
    "Total Inj. Rate: Nonunion" = '#014d64',
    "Total Inj. Rate: Union" = 'darkred')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("longdash", "longdash", "solid", "solid")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Rates of Total and Traumatic Injuries: 2000-2021')
ggsave(file.path(outputdir, 'total_traum_inj_union_fig.png'), 
       plot = total_traum_inj_union_fig, width = 8, height = 6)

# Rates of total and S&S violations (union vs. nonunion) - saved
total_serious_violations_union_fig <- ggplot(time_trend_df) + 
  geom_line(data=subset(time_trend_df, union == 0),aes(x=year_quarter,
                                                               y=ss_violation_rate,
                                                               color='S&S Viol. Rate: Nonunion'), linetype = 1) +
  geom_line(data=subset(time_trend_df, union == 1),aes(x=year_quarter,
                                                       y=ss_violation_rate,
                                                       color='S&S Viol. Rate: Union'), linetype = 1) +
  geom_line(data=subset(time_trend_df, union == 0),aes(x=year_quarter,
                                                               y=violation_rate,
                                                               color='Total Viol. Rate: Nonunion'), linetype = 5) +
  geom_line(data=subset(time_trend_df, union == 1),aes(x=year_quarter,
                                                               y=violation_rate,
                                                               color='Total Viol. Rate: Union'), linetype = 5) +
  ylab('Violations per 2,000 hours worked') +
  scale_y_continuous(breaks = seq(0,2.5,0.25)) +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "S&S Viol. Rate: Nonunion" = '#014d64',
    "S&S Viol. Rate: Union" = 'darkred',
    "Total Viol. Rate: Nonunion" = '#014d64',
    "Total Viol. Rate: Union" = 'darkred')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid","longdash", "longdash")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Rates of Total and "Significant and Substantial" (S&S) MSHA Violations: 2000-2021')
ggsave(file.path(outputdir, 'total_serious_violations_union_fig.png'), 
       plot = total_serious_violations_union_fig, width = 8, height = 6)

# Rates of total and S&S violations (union vs. nonunion), normalized to 0 when first MINER Act regulations promulgated - saved
did_total_serious_violations_union_fig <- ggplot(violations_miner_act_DID) + 
  geom_vline(xintercept = 2006.75, color = "red") +
  geom_line(data=subset(violations_miner_act_DID, union == 0),aes(x=year_quarter,
                                                       y=ss_violation_rate,
                                                       color='S&S Viol. Rate: Nonunion'), linetype = 1) +
  geom_line(data=subset(violations_miner_act_DID, union == 1),aes(x=year_quarter,
                                                       y=ss_violation_rate,
                                                       color='S&S Viol. Rate: Union'), linetype = 1) +
  geom_line(data=subset(violations_miner_act_DID, union == 0),aes(x=year_quarter,
                                                       y=violation_rate,
                                                       color='Total Viol. Rate: Nonunion'), linetype = 5) +
  geom_line(data=subset(violations_miner_act_DID, union == 1),aes(x=year_quarter,
                                                       y=violation_rate,
                                                       color='Total Viol. Rate: Union'), linetype = 5) +
  ylab('Violations per 2,000 hours worked') +
  scale_y_continuous() +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "S&S Viol. Rate: Nonunion" = '#014d64',
    "S&S Viol. Rate: Union" = 'darkred',
    "Total Viol. Rate: Nonunion" = '#014d64',
    "Total Viol. Rate: Union" = 'darkred')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid","longdash", "longdash")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Rates of Total and "Significant and Substantial" (S&S) MSHA Violations: 2000-2021')
ggsave(file.path(outputdir, 'did_total_serious_violations_union_fig.png'), 
       plot = did_total_serious_violations_union_fig, width = 8, height = 6)

# Rates of total and S&S violations (union vs. nonunion), normalized to 0 when first MINER Act regulations promulgated - saved
# Version restricted to only Mines active after 2015 (active_after2015 == 1)
did_total_serious_violations_union_after2015_fig <- ggplot(violations_miner_act_DID_active_after2015) + 
  geom_vline(xintercept = 2006.75, color = "red") +
  geom_line(data=subset(violations_miner_act_DID_active_after2015, union == 0),aes(x=year_quarter,
                                                                  y=ss_violation_rate,
                                                                  color='S&S Viol. Rate: Nonunion'), linetype = 1) +
  geom_line(data=subset(violations_miner_act_DID_active_after2015, union == 1),aes(x=year_quarter,
                                                                  y=ss_violation_rate,
                                                                  color='S&S Viol. Rate: Union'), linetype = 1) +
  geom_line(data=subset(violations_miner_act_DID_active_after2015, union == 0),aes(x=year_quarter,
                                                                  y=violation_rate,
                                                                  color='Total Viol. Rate: Nonunion'), linetype = 5) +
  geom_line(data=subset(violations_miner_act_DID_active_after2015, union == 1),aes(x=year_quarter,
                                                                  y=violation_rate,
                                                                  color='Total Viol. Rate: Union'), linetype = 5) +
  ylab('Violations per 2,000 hours worked') +
  scale_y_continuous() +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "S&S Viol. Rate: Nonunion" = '#014d64',
    "S&S Viol. Rate: Union" = 'darkred',
    "Total Viol. Rate: Nonunion" = '#014d64',
    "Total Viol. Rate: Union" = 'darkred')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid","longdash", "longdash")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Rates of Total and "Significant and Substantial" (S&S) MSHA Violations, Mines active after 2015: 2000-2021')
ggsave(file.path(outputdir, 'did_total_serious_violations_union_after2015_fig.png'), 
       plot = did_total_serious_violations_union_after2015_fig, width = 10, height = 6)

# Rates of total and S&S MINER Act violations (union vs. nonunion), normalized to 0 when first MINER Act regulations promulgated - saved
did_miner_act_violations_fig <- ggplot(violations_miner_act_DID) +
  geom_vline(xintercept = 2006.75, color = "red") +
  geom_line(data=subset(violations_miner_act_DID, union == 0),aes(x=year_quarter,
                                                                  y=ss_violation_miner_act_rate,
                                                                  color='S&S Viol. Rate: Nonunion'), linetype = 1) +
  geom_line(data=subset(violations_miner_act_DID, union == 1),aes(x=year_quarter,
                                                                  y=ss_violation_miner_act_rate,
                                                                  color='S&S Viol. Rate: Union'), linetype = 1) +
  geom_line(data=subset(violations_miner_act_DID, union == 0),aes(x=year_quarter,
                                                                  y=violation_miner_act_rate,
                                                                  color='Total Viol. Rate: Nonunion'), linetype = 5) +
  geom_line(data=subset(violations_miner_act_DID, union == 1),aes(x=year_quarter,
                                                                  y=violation_miner_act_rate,
                                                                  color='Total Viol. Rate: Union'), linetype = 5) +
  ylab('Violations per 2,000 hours worked') +
  scale_y_continuous() +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "S&S Viol. Rate: Nonunion" = '#014d64',
    "S&S Viol. Rate: Union" = 'darkred',
    "Total Viol. Rate: Nonunion" = '#014d64',
    "Total Viol. Rate: Union" = 'darkred')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid","longdash", "longdash")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(face = "italic")) +
  ggtitle('Rates of Total and "Significant and Substantial" (S&S) MINER Act Violations: 2000-2021')
ggsave(file.path(outputdir, 'did_miner_act_violations_fig.png'), 
       plot = did_miner_act_violations_fig, width = 8, height = 6)

# Rates of total and S&S non-MINER Act violations (union vs. nonunion), normalized to 0 when first MINER Act regulations promulgated - saved
did_nonminer_act_violations_fig <- ggplot(violations_miner_act_DID) +
  geom_vline(xintercept = 2006.75, color = "red") +
  geom_line(data=subset(violations_miner_act_DID, union == 0),aes(x=year_quarter,
                                                                  y=ss_violation_nonminer_act_rate,
                                                                  color='S&S Viol. Rate: Nonunion'), linetype = 1) +
  geom_line(data=subset(violations_miner_act_DID, union == 1),aes(x=year_quarter,
                                                                  y=ss_violation_nonminer_act_rate,
                                                                  color='S&S Viol. Rate: Union'), linetype = 1) +
  geom_line(data=subset(violations_miner_act_DID, union == 0),aes(x=year_quarter,
                                                                  y=violation_nonminer_act_rate,
                                                                  color='Total Viol. Rate: Nonunion'), linetype = 5) +
  geom_line(data=subset(violations_miner_act_DID, union == 1),aes(x=year_quarter,
                                                                  y=violation_nonminer_act_rate,
                                                                  color='Total Viol. Rate: Union'), linetype = 5) +
  ylab('Violations per 2,000 hours worked') +
  scale_y_continuous() +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "S&S Viol. Rate: Nonunion" = '#014d64',
    "S&S Viol. Rate: Union" = 'darkred',
    "Total Viol. Rate: Nonunion" = '#014d64',
    "Total Viol. Rate: Union" = 'darkred')) +
  labs(color = '') +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "solid","longdash", "longdash")))) +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.title.y.right = element_text(angle = 90),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(face = "italic")) +
  ggtitle('Rates of Total and "Significant and Substantial" (S&S) non-MINER Act Violations: 2000-2021')
ggsave(file.path(outputdir, 'did_nonminer_act_violations_fig.png'), 
       plot = did_nonminer_act_violations_fig, width = 8, height = 6)

# Real wages and total employees

