# Last updated: Apr 6, 2023

root <- getwd()
while(basename(root) != "coal-mining") { # this is the name of your project directory you want to use
  root <- dirname(root)
}
source(file.path(root, "data.R"))
ddir <- file.path(dir, "data")
outputdir <- file.path(dir, "coal-mining", "output")

library(tidyverse)
library(estimatr)
library(Quandl)
library(reshape2)
library(binsreg)
library(texreg)
library(tidycensus)
library(lubridate)

# Time trends
# Coal consumption
energy_time_trends <- read_csv(file.path(ddir, "eia energy trends", "eia_energy_use_collected_variables.csv")) %>%
  melt(na.rm = FALSE, id = 'Variable') %>%
  rename(year = variable, 
         variable = Variable) %>%
  mutate(year = 1979+as.numeric(year))

world_us_coal_production_fig <- ggplot(energy_time_trends) + 
  geom_line(data=subset(energy_time_trends, variable == "World Coal production (quad Btu)"),aes(x=year,
                                                       y=value,
                                                       color='World'), linetype = 1) +
  geom_line(data=subset(energy_time_trends, variable == "US Coal production (quad Btu)"),aes(x=year,
                                                       y=value,
                                                       color='United States'), linetype = 1) +
  scale_y_continuous(breaks = seq(0,200,20)) +
  ylab('Coal production (quad Btu)') +
  xlab("Year") +
  scale_color_manual(values = c(
    "World" = '#014d64',
    "United States" = 'darkred')) +
  labs(color = '',
       caption = "Source: EIA.") +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('Coal Production: 1980-2021')
world_us_coal_production_fig
ggsave(file.path(outputdir, 'figures', 'world_us_coal_production_fig.png'), 
       plot = world_us_coal_production_fig, width = 8, height = 6)

us_coal_production_fig <- ggplot(energy_time_trends) + 
  geom_line(data=subset(energy_time_trends, variable == "US Bituminous production (Mst)"),aes(x=year,
                                                                                                y=value,
                                                                                                color='Bituminous coal'), linetype = 1) +
  geom_line(data=subset(energy_time_trends, variable == "US Coal production (Mst)"),aes(x=year,
                                                                                             y=value,
                                                                                             color='All coal'), linetype = 1) +
  scale_y_continuous() +
  ylab('Coal production (Mst)') +
  xlab("Year") +
  scale_color_manual(values = c(
    "Bituminous coal" = '#014d64',
    "All coal" = 'darkred')) +
  labs(color = '',
       caption = "Source: EIA.") +
  theme(axis.line = element_line(color = "black"), panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle('United States Coal Production: 1980-2021')
us_coal_production_fig
ggsave(file.path(outputdir, 'figures', 'us_coal_production_fig.png'), 
       plot = us_coal_production_fig, width = 8, height = 6)

# https://data.nasdaq.com/tools/r
# https://data.nasdaq.com/data/BP/COAL_PRICES-coal-prices
coal_price_data <- Quandl("BP/COAL_PRICES") %>%
  mutate(Year = year(ymd(Date))) %>%
  dplyr::filter(Year >= 1990,
         Year <= 2021)

coal_spot_price_fig <- ggplot(data = coal_price_data, aes(x = Year)) +
  geom_line(aes(y = `US Central Appalachian Coal Spot Price Index`, 
                color = "US Central Appalachian Coal Spot Price Index"), linetype = 1) +
  scale_y_continuous(breaks = seq(0,140,20),
                     name = "USD per Ton") +
  scale_x_continuous(breaks = seq(1990,2021,5)) +
  scale_color_manual(values = c(
    "US Central Appalachian Coal Spot Price Index" = '#014d64')) +
  labs(color = '',
       caption = "Source: Nasdaq Data Link.") +
  guides(color = guide_legend(override.aes = list(linetype = c("solid")))) +
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
  ggtitle('US Coal Price Index: 1990-2021')
coal_spot_price_fig
ggsave(file.path(outputdir, 'figures', 'coal_spot_price_fig.png'), 
       plot = coal_spot_price_fig, width = 8, height = 6)

# County-level OLS regressions 
# Desired dataset: underground bituminous mines with year_quarter x county 
#                  with county HHI, county ss violation rate, county traum injury rate, real wage
# https://walker-data.com/tidycensus/reference/fips_codes.html
data(fips_codes)

fips_codes <- fips_codes %>%
  mutate(county_fips = as.numeric(county_code), #sub("^0+", "", as.character(county_code)),
         full_fips = paste0(as.character(state_code), as.character(county_code)))

mine_panel_quarters_import <- read_csv(file.path(ddir, "cleaned", "mine_panel_quarters.csv")) %>%
  left_join(fips_codes, by = c("state", "county_fips"))
  
df_wages_adjusted <- read_csv(file.path(ddir, "cleaned", "naics_2121_county_wages_2000_2021.csv")) %>%
  dplyr::select(-year, -quarter)

mine_panel_quarters_wages <- mine_panel_quarters_import %>%
  rename(area_fips = full_fips) %>%
  left_join(df_wages_adjusted, by = c("area_fips", "year_quarter")) %>%
  dplyr::filter(zero_production_quarter == 0) %>%
  mutate(mine_id_factor = factor(MINE_ID),
         coal_production_tons_mil = coal_production_tons/1000000)

mines_after_2015_df <- mine_panel_quarters_import %>%
  dplyr::filter(zero_production_quarter == 0, year >= 2015) %>%
  dplyr::select(MINE_ID) %>%
  distinct() %>%
  mutate(active_after2015 = 1)

mine_panel_quarters_wages <- mine_panel_quarters_wages %>%
  left_join(mines_after_2015_df, by = "MINE_ID") %>%
  mutate(active_after2015 = ifelse(is.na(active_after2015), 0, 1))

mines_2000_2010_df <- mine_panel_quarters_wages %>%
  filter(year >= 2000, 
         year <= 2010) %>%
  group_by(MINE_ID, district) %>%
  summarize(total_injury_rate = 2000*(sum(total_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            traumatic_injury_rate = 2000*(sum(traumatic_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            violation_rate = 2000*(sum(violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            ss_violation_rate = 2000*(sum(ss_violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            labor_hours = sum(labor_hours, na.rm = TRUE),
            coal_production_tons = sum(coal_production_tons, na.rm = TRUE),
            productivity = 2000*(coal_production_tons/labor_hours),
            size_100employees = mean(size_100employees, na.rm = TRUE),
            controller_size_100employees = mean(controller_size_100employees, na.rm = TRUE),
            ln_controller_size_100employees = log(controller_size_100employees), 
            mine_age = mean(mine_age, na.rm = TRUE),
            penalty_points_previousfourquarters = mean(penalty_points_previousfourquarters, na.rm = TRUE)) %>%
  left_join(mines_after_2015_df, by = "MINE_ID") %>%
  mutate(active_after2015 = ifelse(is.na(active_after2015), 0, 1)) %>%
  mutate(mine_id_factor = factor(MINE_ID),
         coal_production_tons_mil = coal_production_tons/1000000)

# Violations and injuries
injury_violation_rates_fig <- binsreg(y = filter(mine_panel_quarters_wages, !is.na(traumatic_injury_rate) & !is.na(ss_violation_rate), ss_violation_rate < 6.15)$traumatic_injury_rate, 
        x = filter(mine_panel_quarters_wages, !is.na(traumatic_injury_rate) & !is.na(ss_violation_rate), ss_violation_rate < 6.15)$ss_violation_rate,
        polyreg = 1)$bins_plot +
  ylab("Traumatic Injuries per 2,000 hours worked") + 
  xlab("S&S Violation per 2,000 hours worked (top 1 pct trimmed)") +
  ggtitle('Injury and Violation Rates in Active Underground Bituminous Coal Mines Binscatter: 2000-2021')
injury_violation_rates_fig
ggsave(file.path(outputdir, 'figures', 'injury_violation_rates_fig.png'), 
       plot = injury_violation_rates_fig, width = 9, height = 6)

# Relationship between violations and injuries
irr_traminj_ssviol_1 <- extract(negbinirr(traumatic_injuries ~ ss_violations*size_100employees + 
          + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
          + factor(year_quarter) + factor(district) 
          + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
          data = mine_panel_quarters_wages),
          include.deviance = FALSE,
          include.aic = FALSE,
          include.bic = FALSE)

irr_traminj_ssviol_2 <- extract(negbinirr(traumatic_injuries ~ ss_violations*size_100employees + union + union:size_100employees 
          + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
          + factor(year_quarter) + factor(district) 
          + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
          data = mine_panel_quarters_wages),
include.deviance = FALSE,
include.aic = FALSE,
include.bic = FALSE)

irr_traminj_ssviol_table <- texreg(list(irr_traminj_ssviol_1, irr_traminj_ssviol_2), 
                                   digits = 3, include.ci = FALSE, #stars = numeric(0),
                                   custom.coef.map = list('ss_violations'= " S\\&S Viol. "),
                                   custom.header = list("Traum. Inj." = 1:2),
                                   custom.model.names = c("(1)","(2)"),
                                   custom.gof.rows = list("Union Control" = c("NO", "YES")),
                                   caption = "Relationship of Violations and Injuries",
                                   label = "irr_traminj_ssviol")
irr_traminj_ssviol_table
write.table(irr_traminj_ssviol_table, file.path(outputdir, 'tables', 'irr_traminj_ssviol_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Characteristics of mines that stayed open vs. closed
mean(filter(mine_panel_quarters_wages, active_after2015 == 0)$ss_violation_rate, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 1)$ss_violation_rate, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 0)$traumatic_injury_rate, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 1)$traumatic_injury_rate, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 0)$coal_production_tons_mil, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 1)$coal_production_tons_mil, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 0)$productivity, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 0, year <= 2010)$productivity, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 1)$productivity, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 1, year <= 2010)$productivity, na.rm = TRUE)
mean(filter(mine_panel_quarters_wages, active_after2015 == 1, year > 2010)$productivity, na.rm = TRUE)

mean(filter(mines_2000_2010_df, active_after2015 == 0)$ss_violation_rate, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 1)$ss_violation_rate, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 0)$traumatic_injury_rate, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 1)$traumatic_injury_rate, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 0)$coal_production_tons_mil, na.rm = TRUE)
sd(filter(mines_2000_2010_df, active_after2015 == 0)$coal_production_tons_mil, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 1)$coal_production_tons_mil, na.rm = TRUE)
sd(filter(mines_2000_2010_df, active_after2015 == 1)$coal_production_tons_mil, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 0)$productivity, na.rm = TRUE)
sd(filter(mines_2000_2010_df, active_after2015 == 0)$productivity, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 1)$productivity, na.rm = TRUE)
sd(filter(mines_2000_2010_df, active_after2015 == 1)$productivity, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 0)$size_100employees, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 1)$size_100employees, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 0)$controller_size_100employees, na.rm = TRUE)
mean(filter(mines_2000_2010_df, active_after2015 == 1)$controller_size_100employees, na.rm = TRUE)

# Injuries and production are actually inversely correlated
irr_traminj_coalprod_1 <- extract(negbinirr(traumatic_injuries ~ coal_production_tons_mil*size_100employees + 
            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
          + factor(year_quarter) + factor(district) 
          + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
          data = mine_panel_quarters_wages),
          include.deviance = FALSE,
          include.aic = FALSE,
          include.bic = FALSE)

irr_traminj_coalprod_2 <- extract(negbinirr(traumatic_injuries ~ coal_production_tons_mil*size_100employees + union + union:size_100employees
            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
          + factor(year_quarter) + factor(district) 
          + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
          data = mine_panel_quarters_wages),
include.deviance = FALSE,
include.aic = FALSE,
include.bic = FALSE)

irr_traminj_coalprod_3 <- extract(negbinirr(traumatic_injuries ~ coal_production_tons_mil*size_100employees + 
            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
          + factor(year_quarter) + factor(district) 
          + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
          data = filter(mine_panel_quarters_wages, active_after2015 == 0)),
include.deviance = FALSE,
include.aic = FALSE,
include.bic = FALSE)

irr_traminj_coalprod_4 <- extract(negbinirr(traumatic_injuries ~ coal_production_tons_mil*size_100employees + union + union:size_100employees
            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
          + factor(year_quarter) + factor(district) 
          + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
          data = filter(mine_panel_quarters_wages, active_after2015 == 0)),
include.deviance = FALSE,
include.aic = FALSE,
include.bic = FALSE)

irr_traminj_coalprod_5 <- extract(negbinirr(traumatic_injuries ~ coal_production_tons_mil*size_100employees + 
            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
          + factor(year_quarter) + factor(district) 
          + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
          data = filter(mine_panel_quarters_wages, active_after2015 == 1)),
include.deviance = FALSE,
include.aic = FALSE,
include.bic = FALSE)

irr_traminj_coalprod_6 <- extract(negbinirr(traumatic_injuries ~ coal_production_tons_mil*size_100employees + union + union:size_100employees 
            + ln_controller_size_100employees + mine_age + productivity + penalty_points_previousfourquarters 
          + factor(year_quarter) + factor(district) 
          + subunit_1 + subunit_2 + subunit_3 + subunit_4 + subunit_5 + subunit_6 + subunit_12 + subunit_17 + subunit_30 + subunit_99, 
          data = filter(mine_panel_quarters_wages, active_after2015 == 1)),
include.deviance = FALSE,
include.aic = FALSE,
include.bic = FALSE)

irr_traminj_coalprod_table <- texreg(list(irr_traminj_coalprod_1, irr_traminj_coalprod_2,
                                          irr_traminj_coalprod_3, irr_traminj_coalprod_4,
                                          irr_traminj_coalprod_5, irr_traminj_coalprod_6), 
                                   digits = 3, include.ci = FALSE, #stars = numeric(0),
                                   custom.coef.map = list('coal_production_tons_mil'= " Coal Prod. "),
                                   custom.header = list("Traumatic Injuries" = 1:6),
                                   custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)"),
                                   custom.gof.rows = list("Union Control" = c("NO", "YES", "NO", "YES", "NO", "YES"),
                                                          "Active After 2015" = c("", "", "NO", "NO", "YES", "YES"),
                                                          "Full Sample" = c("YES", "YES", "", "", "", "")),
                                   caption = "Effect of Coal Produced on Injuries",
                                   label = "irr_traminj_coalprod")
irr_traminj_coalprod_table
write.table(irr_traminj_coalprod_table, file.path(outputdir, 'tables', 'irr_traminj_coalprod_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Mines with low injuries and high production are more likely to be active after 2015 
mine_ols1 <- lm_robust(active_after2015 ~ traumatic_injury_rate, 
                       data = mines_2000_2010_df, se_type = "stata")
summary(mine_ols1)

mine_ols2 <- lm_robust(active_after2015 ~ traumatic_injury_rate*size_100employees 
                       + ln_controller_size_100employees + mine_age + coal_production_tons_mil  + penalty_points_previousfourquarters 
                       + factor(district), 
                       data = mines_2000_2010_df, se_type = "stata")
summary(mine_ols2)

mine_ols3 <- lm_robust(active_after2015 ~ coal_production_tons_mil, 
                       data = mines_2000_2010_df, se_type = "stata")
summary(mine_ols3)

mine_ols4 <- lm_robust(active_after2015 ~ coal_production_tons_mil*size_100employees 
                       + ln_controller_size_100employees + mine_age + penalty_points_previousfourquarters 
                       + factor(district), 
                       data = mines_2000_2010_df, se_type = "stata")
summary(mine_ols4)

closure_linprob_table <- texreg(list(mine_ols1, mine_ols2, mine_ols3, mine_ols4), 
                              custom.coef.map = list('traumatic_injury_rate' = " Trau. Inj. Rate ",
                                                     'coal_production_tons_mil' = " Coal Prod. "),
                              custom.header = list("Active After 2015" = 1:4),
                              custom.model.names = c("(1)","(2)","(3)","(4)"),
                              digits = 3, include.ci = FALSE, #stars = numeric(0),
                              caption = "Effect of Production and Injuries on Mine Closure",
                              label = "closure_linprob",
                              custom.gof.rows = list("Covariates" = c("NO", "YES", "NO", "YES"))
)
closure_linprob_table
write.table(closure_linprob_table, file.path(outputdir, 'tables', 'closure_linprob_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# Productivity and production
mine_ols5 <- lm_robust(productivity ~ size_100employees, 
                       data = mine_panel_quarters_wages, se = "stata")
summary(mine_ols5)

mine_ols6 <- lm_robust(productivity ~ size_100employees + factor(MINE_ID) + factor(year_quarter), 
                       data = mine_panel_quarters_wages, se = "stata")
summary(mine_ols6)

productivity_size_regs_table <- texreg(list(mine_ols5, mine_ols6), 
                            custom.coef.map = list('traumatic_injury_rate' = " Trau. Inj. Rate ",
                                                   'coal_production_tons_mil' = " Coal Prod. ",
                                                   'size_100employees' = " Size "),
                            custom.header = list("Productivity" = 1:2),
                            digits = 3, include.ci = FALSE, #stars = numeric(0),
                            custom.model.names = c("(1)","(2)"),
                            caption = "Effect of Mine Size on Productivity",
                            label = "productivity_size_regs",
                            custom.gof.rows = list("County FE" = c("NO", "YES"),
                                                   "Quarter FE" = c("NO", "YES"))
)
productivity_size_regs_table
write.table(productivity_size_regs_table, file.path(outputdir, 'tables', 'productivity_size_regs_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# HHI regressions
county_violations_injuries_df <- mine_panel_quarters_import %>%
  filter(year >= 2000,
         year <= 2021,
         zero_production_quarter == 0) %>%
  group_by(year_quarter, full_fips) %>%
  summarize(total_injury_rate = 2000*(sum(total_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            traumatic_injury_rate = 2000*(sum(traumatic_injuries, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            violation_rate = 2000*(sum(violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            ss_violation_rate = 2000*(sum(ss_violations, na.rm = TRUE)/sum(labor_hours, na.rm = TRUE)),
            labor_hours = sum(labor_hours, na.rm = TRUE),
            total_avg_employee_count = sum(avg_employee_count, na.rm = TRUE),
            coal_production_tons = sum(coal_production_tons, na.rm = TRUE),
            productivity = 2000*(coal_production_tons/labor_hours),
            operator_changes = sum(operator_change, na.rm = TRUE),
            controller_changes = sum(controller_change, na.rm = TRUE),
            active_mines = n())

# instrument for HHI: inverse number of employers in other labor markets 
national_instrument <- county_violations_injuries_df %>%
  group_by(year_quarter) %>%
  summarize(total_mines_natl = sum(active_mines))

county_violations_injuries_df <- county_violations_injuries_df %>%
  left_join(national_instrument, by = "year_quarter") %>%
  mutate(instrument = 1/(total_mines_natl - active_mines))

county_hhi_df <- mine_panel_quarters_import %>%
  group_by(year_quarter, full_fips, CONTROLLER_ID) %>%
  summarize(controller_size_100employees_county = sum(size_100employees, na.rm = TRUE)) %>%
  group_by(year_quarter, full_fips) %>%
  mutate(controller_share_county = 100*controller_size_100employees_county/sum(controller_size_100employees_county, na.rm = TRUE)) %>%
  summarize(hhi_county = sum(controller_share_county^2, na.rm = TRUE),
            county_size_100employees = sum(controller_size_100employees_county, na.rm = TRUE))

county_df <- county_violations_injuries_df %>%
  left_join(county_hhi_df, by = c("year_quarter", "full_fips")) %>%
  rename(area_fips = full_fips) %>%
  left_join(df_wages_adjusted, by = c("area_fips", "year_quarter")) %>%
  #filter(avg_wkly_wage_real != 0) %>%
  mutate(ln_avg_wkly_wage_real = log(avg_wkly_wage_real),
         ln_hhi_county = log(hhi_county),
         ln_productivity = log(productivity),
         ln_ss_violation_rate = log(ss_violation_rate), 
         ss_violation_rate = ifelse(is.na(ss_violation_rate), 0, ss_violation_rate),
         traumatic_injury_rate = ifelse(is.na(traumatic_injury_rate), 0, traumatic_injury_rate),
         ln_instrument = log(instrument),
         third_factor = paste0(year_quarter,area_fips))

summary(county_df$hhi_county)

summary(lm_robust(ln_hhi_county ~ ln_instrument, fixed_effects = ~factor(area_fips)+factor(year_quarter), 
                  data = county_df, se_type = "stata"))
summary(lm_robust(ln_hhi_county ~ ln_instrument + factor(area_fips) + factor(year_quarter) + total_avg_employee_count + active_mines, 
                  data = county_df, se_type = "stata"))
summary(lm_robust(ln_hhi_county ~ ln_instrument + factor(area_fips) + factor(year_quarter)+total_avg_employee_count, 
                  data = county_df, se_type = "stata"))

summary(iv_robust(ss_violation_rate ~ ln_hhi_county | 
                    ln_instrument, 
                  fixed_effects = ~factor(area_fips)+factor(year_quarter),
                  data = county_df, se_type = "stata"))

summary(lm_robust(productivity ~ ln_instrument, 
                  fixed_effects = ~factor(area_fips)+factor(year_quarter),
                  data = county_df, se_type = "stata"))

# Productivity
reg3 <- lm_robust(ln_productivity ~ ln_hhi_county, 
                  data = county_df, se_type = "stata")
summary(reg3)
reg4 <- lm_robust(ln_productivity ~ ln_hhi_county, 
                  fixed_effects = ~factor(area_fips)+factor(year_quarter),
                  data = county_df, se_type = "stata")
summary(reg4)

# SS violations
reg5 <- lm_robust(ss_violation_rate ~ ln_hhi_county, 
                  data = county_df, se_type = "stata")
summary(reg5)
reg6 <- lm_robust(ss_violation_rate ~ ln_hhi_county, 
                  fixed_effects = ~factor(area_fips)+factor(year_quarter),
                  data = county_df, se_type = "stata")
summary(reg6)

# Traum. Injuries
reg7 <- lm_robust(traumatic_injury_rate ~ ln_hhi_county, 
                  data = county_df, se_type = "stata")
summary(reg7)
reg8 <- lm_robust(traumatic_injury_rate ~ ln_hhi_county, 
                  fixed_effects = ~factor(area_fips)+factor(year_quarter),
                  data = county_df, se_type = "stata")
summary(reg8)

county_regs_table <- texreg(list(reg3, reg4, reg5, reg6, reg7, reg8), 
                            custom.coef.map = list('ln_hhi_county'= " ln(County HHI) "),
                            custom.header = list("ln(Productivity)" = 1:2,
                                                 "S\\&S Viol. Rate" = 3:4, "Traum. Inj. Rate" = 5:6),
                            digits = 3, include.ci = FALSE, #stars = numeric(0),
                            custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)"),
                            caption = "County-Level Regression Estimates",
                            label = "ols_hhi_county",
                            custom.gof.rows = list("County FE" = c("NO", "YES", "NO", "YES", "NO", "YES"),
                                                   "Quarter FE" = c("NO", "YES", "NO", "YES", "NO", "YES"))
                            )
county_regs_table
write.table(county_regs_table, file.path(outputdir, 'tables', 'county_regs_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# IV first stage
firststage <- lm_robust(ln_hhi_county ~ ln_instrument, 
                  fixed_effects = ~factor(area_fips)+factor(year_quarter),
                  data = county_df, se_type = "stata")
summary(firststage)
firststage_table <- texreg(list(firststage), 
                            custom.coef.map = list('ln_instrument'= " ln(1/Active Mines in Other Counties) "),
                            digits = 3, include.ci = FALSE, #stars = numeric(0),
                            custom.model.names = c(" ln(County HHI) "),
                            caption = "First-Stage IV Regression Estimate",
                            label = "firststage_table",
                            custom.gof.rows = list("County FE" = c("YES"),
                                                   "Quarter FE" = c("YES"))
)
firststage_table
write.table(firststage_table, file.path(outputdir, 'tables', 'firststage_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

# IV
iv1 <- iv_robust(ln_productivity ~ ln_hhi_county | 
                    ln_instrument, 
                  fixed_effects = ~factor(area_fips)+factor(year_quarter),
                  data = county_df, se_type = "stata")
summary(iv1)

iv2 <- iv_robust(ss_violation_rate ~ ln_hhi_county | 
                           ln_instrument, 
                         fixed_effects = ~factor(area_fips)+factor(year_quarter),
                         data = county_df, se_type = "stata")
summary(iv2)

iv3 <- iv_robust(traumatic_injury_rate ~ ln_hhi_county | 
                           ln_instrument, 
                         fixed_effects = ~factor(area_fips)+factor(year_quarter),
                         data = county_df, se_type = "stata")
summary(iv3)

iv_table <- texreg(list(iv1, iv2, iv3), 
                            custom.coef.map = list('ln_hhi_county'= " ln(County HHI) "),
                            digits = 3, include.ci = FALSE, #stars = numeric(0),
                            custom.model.names = c("(ln(Productivity))","(S\\&S Viol. Rate)","(Traum. Inj. Rate)"),
                            caption = "County-Level IV Regression Estimates",
                            label = "iv_table",
                            custom.gof.rows = list("County FE" = c("YES", "YES", "YES"),
                                                   "Quarter FE" = c("YES", "YES", "YES"))
)
iv_table
write.table(iv_table, file.path(outputdir, 'tables', 'iv_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)


ols_iv_presentation_table <- texreg(list(reg4, iv1, reg8, iv3, reg6, iv2), 
                                    custom.coef.map = list('ln_hhi_county'= " ln(County HHI) "),
                                    digits = 3, include.ci = FALSE, #stars = numeric(0),
                                    custom.gof.rows = list("County FE" = c("YES", "YES", "YES", "YES", "YES", "YES"),
                                                           "Quarter FE" = c("YES", "YES", "YES", "YES", "YES", "YES")),
                                    custom.header = list(" ln(Productivity) " = 1:2,
                                                         " Traum. Inj. Rate " = 3:4,
                                                         " S\\&S Viol. Rate " = 5:6),
                                    custom.model.names = c("(OLS)","(IV)",
                                                           "(OLS)","(IV)",
                                                           "(OLS)","(IV)")
)
ols_iv_presentation_table
write.table(ols_iv_presentation_table, file.path(outputdir, 'tables', 'ols_iv_presentation_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

