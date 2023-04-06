# Last updated: Apr 5, 2023

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
  labs(color = '') +
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
  labs(color = '') +
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
  mutate(mine_id_factor = factor(MINE_ID))

injury_violation_rates_fig <- binsreg(y = filter(mine_panel_quarters_wages, !is.na(traumatic_injury_rate) & !is.na(ss_violation_rate), ss_violation_rate < 6.15)$traumatic_injury_rate, 
        x = filter(mine_panel_quarters_wages, !is.na(traumatic_injury_rate) & !is.na(ss_violation_rate), ss_violation_rate < 6.15)$ss_violation_rate,
        polyreg = 1)$bins_plot +
  ylab("Traumatic Injuries per 2,000 hours worked") + 
  xlab("S&S Violation per 2,000 hours worked (top 1 pct trimmed)") +
  ggtitle('Injury and Violation Rates in Active Underground Bituminous Coal Mines Binscatter: 2000-2021')
injury_violation_rates_fig
ggsave(file.path(outputdir, 'figures', 'injury_violation_rates_fig.png'), 
       plot = injury_violation_rates_fig, width = 9, height = 6)

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
  left_join(df_wages_adjusted, by = c("area_fips", "year_quarter"))
  
# Wage
reg1 <- lm_robust(avg_wkly_wage_real ~ hhi_county, 
                  data = county_df, se_type = "stata")
summary(reg1)
reg2 <- lm_robust(avg_wkly_wage_real ~ hhi_county + factor(area_fips) + factor(year_quarter), 
                  data = county_df, se_type = "stata")
summary(reg2)

# Productivity
reg3 <- lm_robust(productivity ~ hhi_county, 
                  data = county_df, se_type = "stata")
summary(reg3)
reg4 <- lm_robust(productivity ~ hhi_county + factor(area_fips) + factor(year_quarter), 
                  data = county_df, se_type = "stata")
summary(reg4)

# SS violations
reg5 <- lm_robust(ss_violation_rate ~ hhi_county, 
                  data = county_df, se_type = "stata")
summary(reg5)
reg6 <- lm_robust(ss_violation_rate ~ hhi_county + factor(area_fips) + factor(year_quarter), 
                  data = county_df, se_type = "stata")
summary(reg6)

# Traum. Injuries
reg7 <- lm_robust(traumatic_injury_rate ~ hhi_county, 
                  data = county_df, se_type = "stata")
summary(reg7)
reg8 <- lm_robust(traumatic_injury_rate ~ hhi_county + factor(area_fips) + factor(year_quarter), 
                  data = county_df, se_type = "stata")
summary(reg8)

county_regs_table <- texreg(list(reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8), 
                            custom.coef.map = list('hhi_county'= " County HHI "),
                            custom.header = list("Avg. Weekly Wage" = 1:2, "Productivity" = 3:4,
                                                 "S\\&S Viol. Rate" = 5:6, "Traum. Inj. Rate" = 7:8),
                            digits = 3, include.ci = FALSE, #stars = numeric(0),
                            caption = "County-Level Regression Estimates",
                            label = "ols_hhi_county",
                            custom.gof.rows = list("County FE" = c("NO", "YES", "NO", "YES", "NO", "YES", "NO", "YES"),
                                                   "Quarter FE" = c("NO", "YES", "NO", "YES", "NO", "YES", "NO", "YES"))
                            )
county_regs_table
write.table(county_regs_table, file.path(outputdir, 'tables', 'county_regs_table.tex'), 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

