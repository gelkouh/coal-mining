# Last updated: Jan 22, 2023
# Diff-in-diff figures for MINER Act-related violations & injuries in unionized vs. non-unionized mines 

library(tidyverse)
library(reshape2)

ddir <- "/Users/gelkouh/Library/CloudStorage/OneDrive-Personal/Documents/School/UChicago/Thesis/MINING/data"
outputdir <- "/Users/gelkouh/Library/CloudStorage/OneDrive-Personal/Documents/School/UChicago/Thesis/MINING/output"

mine_panel <- read_csv(file.path(ddir, "cleaned", "mine_quarter_panel.csv"))

##-----##
# LABOR HOURS
##-----##

labor_hours_DID <- mine_panel %>%
  group_by(year_quarter, union) %>%
  summarize(total_labor_hours_mil = sum(labor_hours)/1000000) %>%
  filter(year_quarter >= 2002, 
         year_quarter <= 2016.4)

labor_hours_DID <- labor_hours_DID %>%
  mutate(total_labor_hours_mil = ifelse(union == 0,
                                        total_labor_hours_mil - filter(labor_hours_DID, year_quarter == 2006.1 & union == 0)$total_labor_hours_mil,
                                        total_labor_hours_mil - filter(labor_hours_DID, year_quarter == 2006.1 & union == 1)$total_labor_hours_mil))

labor_hours_DID_fig <- ggplot(data = labor_hours_DID) +
  geom_line(data = filter(labor_hours_DID, union == 1), 
            aes(x = year_quarter, 
                y = total_labor_hours_mil, 
                color = "union"), linetype = 5) +
  geom_line(data = filter(labor_hours_DID, union == 0), 
            aes(x = year_quarter, 
                y = total_labor_hours_mil, 
                color = "non-union"), linetype = 2) +
  geom_vline(xintercept = 2006, color = 'red', size = 0.5) +
  expand_limits(x = c(2002,2016), y = c(0,0.2)) +
  scale_x_continuous(breaks = seq(2002,2016,2)) +
  ylab('Total labor hours (millions)') +
  xlab('') +
  scale_color_manual(values = c(
    "non-union" = '#014d64',
    "union" = 'darkred')) +
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
  ggtitle('Trends in labor hours before and after MINER Act (2006)')

ggsave(file.path(outputdir, 'labor_hours_DID.pdf'), 
       plot = labor_hours_DID_fig, width = 8, height = 6)

##-----##
# PRODUCTIVITY
##-----##

labor_productivity_DID <- mine_panel %>%
  group_by(year_quarter, union) %>%
  summarize(total_labor_hours_mil = sum(labor_hours),
            labor_productivity_numerator = sum(labor_productivity_x_labor_hours)) %>%
  mutate(weighted_productivity = labor_productivity_numerator/total_labor_hours_mil) %>%
  filter(year_quarter >= 2002, 
         year_quarter <= 2016.4)

labor_productivity_DID <- labor_productivity_DID %>%
  mutate(weighted_productivity = ifelse(union == 0,
                                        weighted_productivity - filter(labor_productivity_DID, year_quarter == 2006.1 & union == 0)$weighted_productivity,
                                        weighted_productivity - filter(labor_productivity_DID, year_quarter == 2006.1 & union == 1)$weighted_productivity))

labor_productivity_DID_fig <- ggplot(data = labor_productivity_DID) +
  geom_line(data = filter(labor_productivity_DID, union == 1), 
            aes(x = year_quarter, 
                y = weighted_productivity, 
                color = "union"), linetype = 5) +
  geom_line(data = filter(labor_productivity_DID, union == 0), 
            aes(x = year_quarter, 
                y = weighted_productivity, 
                color = "non-union"), linetype = 2) +
  geom_vline(xintercept = 2006, color = 'red', size = 0.5) +
  expand_limits(x = c(2002,2016), y = c(0,0.2)) +
  scale_x_continuous(breaks = seq(2002,2016,2)) +
  ylab('Average tons of coal per labor hour (weighted by labor hours)') +
  xlab('') +
  scale_color_manual(values = c(
    "non-union" = '#014d64',
    "union" = 'darkred')) +
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
  ggtitle('Trends in productivity before and after MINER Act (2006)')

ggsave(file.path(outputdir, 'labor_productivity_DID.pdf'), 
       plot = labor_productivity_DID_fig, width = 8, height = 6)

##-----##
# CITATIONS AND ACCIDENTS
##-----##

citations_accidents_DID <- mine_panel %>%
  group_by(year_quarter, union) %>%
  summarize(total_labor_hours = sum(labor_hours),
            violation_rate_labor_hours_numer = sum(violation_rate_x_labor_hours),
            serious_injury_rate_labor_hours_numer = sum(serious_injury_rate_x_labor_hours)) %>%
  mutate(weighted_violation_rate = violation_rate_labor_hours_numer/total_labor_hours,
         weighted_serious_injury_rate = serious_injury_rate_labor_hours_numer/total_labor_hours)  %>%
  filter(year_quarter >= 2002, 
         year_quarter <= 2016.4)

citations_accidents_DID <- citations_accidents_DID %>%
  mutate(weighted_violation_rate = ifelse(union == 0,
                                          weighted_violation_rate - filter(citations_accidents_DID, year_quarter == 2006.4 & union == 0)$weighted_violation_rate,
                                          weighted_violation_rate - filter(citations_accidents_DID, year_quarter == 2006.4 & union == 1)$weighted_violation_rate),
         weighted_serious_injury_rate = ifelse(union == 0,
                                               weighted_serious_injury_rate - filter(citations_accidents_DID, year_quarter == 2006.4 & union == 0)$weighted_serious_injury_rate,
                                               weighted_serious_injury_rate - filter(citations_accidents_DID, year_quarter == 2006.4 & union == 1)$weighted_serious_injury_rate),
         year_quarter = ifelse(year_quarter %% 1 == .1, year_quarter - .1,
                               ifelse(year_quarter %% 1 == 2, year_quarter - .2 + .25,
                                      ifelse(year_quarter %% 1 == 3, year_quarter - .3 + .5,
                                             year_quarter - .4 + .75))))

citations_DID_fig <- ggplot(data = citations_accidents_DID) +
  geom_line(data = filter(citations_accidents_DID, union == 1), 
            aes(x = year_quarter, 
                y = weighted_violation_rate, 
                color = "union"), linetype = 5) +
  geom_line(data = filter(citations_accidents_DID, union == 0), 
            aes(x = year_quarter, 
                y = weighted_violation_rate, 
                color = "non-union"), linetype = 2) +
  geom_vline(xintercept = 2006.75, color = 'red', size = 0.5) +
  expand_limits(x = c(2002,2016), y = c(0,0.2)) +
  scale_x_continuous(breaks = seq(2002,2016,2)) +
  ylab('Average violations per 2,000 labor hours (weighted by labor hours)') +
  xlab('') +
  scale_color_manual(values = c(
    "non-union" = '#014d64',
    "union" = 'darkred')) +
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
  ggtitle('Trends in MSHA violations before and after first MINER Act regulations promulgated')

ggsave(file.path(outputdir, 'violations_all_DID.png'), 
       plot = citations_DID_fig, width = 8, height = 6)

injuries_DID_fig <- ggplot(data = citations_accidents_DID) +
  geom_line(data = filter(citations_accidents_DID, union == 1), 
            aes(x = year_quarter, 
                y = weighted_serious_injury_rate, 
                color = "union"), linetype = 5) +
  geom_line(data = filter(citations_accidents_DID, union == 0), 
            aes(x = year_quarter, 
                y = weighted_serious_injury_rate, 
                color = "non-union"), linetype = 2) +
  geom_vline(xintercept = 2006.4, color = 'red', size = 0.5) +
  expand_limits(x = c(2002,2016), y = c(0,0.2)) +
  scale_x_continuous(breaks = seq(2002,2016,2)) +
  ylab('Average serious injuries per 2,000 labor hours (weighted by labor hours)') +
  xlab('') +
  scale_color_manual(values = c(
    "non-union" = '#014d64',
    "union" = 'darkred')) +
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
  ggtitle('Trends in serious injuries before and after MINER Act (2006)')

ggsave(file.path(outputdir, 'injuries_DID.pdf'), 
       plot = injuries_DID_fig, width = 8, height = 6)

