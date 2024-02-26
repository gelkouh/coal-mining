# Diff-in-diff figures for above/below surface share of mines unionized

# TAKEAWAY: 
# No apparent difference in trend in share of mines unionized before/after MINER Act for above vs. below ground mines
# Instead,
#   A. plot mine closings (going Active -> Not Active) in unionized vs. non-unionized (underground) mines, normalized to 0 in 2006
#   B. plot accident trends of above vs. below ground non-unionized mines AND IN A SEPARATE PLOT above vs. below ground unionized mines
#      (i.e., use mines on the other side of the earth's surface with the same unionization status as control/treatment group)
#   C. look at whether workplace safety regulation differentially impacted productivity and safety of union vs non-union mines 
#      (change in productivity should be smoother for unionized underground mines if they are already protecting their workers vs. non-unionized mines)
#   D. Usually restrict sample to mines that do not change unionization status in the sample (and put others in appendix a la Morantz (2013))

library(tidyverse)
library(reshape2)

ddir <- "/Users/gelkouh/Library/CloudStorage/OneDrive-Personal/Documents/School/UChicago/Thesis/MINING/data"

df_unionization <- read_csv(file.path(ddir, ...),
                            col_types = cols(`Average Employees` = col_character(),
                                             `Labor Hours` = col_character(),
                                             `Coal Supply Region` = col_character())) %>%
  select(Year, `MSHA ID`, `Mine State`, `Mine County`, `Mine Status`, `Mine Type`, `Average Employees`, `drop`, `union`) %>%
  rename(year = `Year`, 
         MSHA_MINE_ID = `MSHA ID`,
         state = `Mine State`,
         county = `Mine County`,
         status = `Mine Status`,
         type = `Mine Type`,
         avg_employees = `Average Employees`) %>%
  mutate(state_county = paste(state, county, sep = "_"),
         counter = 1) %>%
  filter(status == "Active", (type == "Surface" | type == "Underground"))

df_diff_in_diff_figure_1 <- df_unionization %>%
  mutate(avg_employees = ifelse(is.na(avg_employees), 0, avg_employees),
         union_weighted = union*as.numeric(avg_employees)) %>%
  group_by(year, type, union) %>%
  filter(year >= 2002, 
         year <= 2016) %>%
  summarize(sum_union = sum(union),
            sum_mines = sum(counter),
            sum_union_weighted = sum(as.numeric(union_weighted)),
            sum_weights = sum(as.numeric(avg_employees))) %>%
  group_by(year, type) %>%
  mutate(pct_union_weighted = sum_union_weighted / sum(sum_weights),
         pct_union = sum_union / sum(sum_mines)) %>%
  filter(union == 1) %>%
  ungroup() 

df_diff_in_diff_figure <- df_diff_in_diff_figure_1 %>%
  mutate(pct_union_weighted = ifelse(type == "Underground",
                                     pct_union_weighted - filter(df_diff_in_diff_figure_1, year == 2006 & type == "Underground")$pct_union_weighted,
                                     pct_union_weighted - filter(df_diff_in_diff_figure_1, year == 2006 & type == "Surface")$pct_union_weighted),
         pct_union = ifelse(type == "Underground",
                            pct_union - filter(df_diff_in_diff_figure_1, year == 2006 & type == "Underground")$pct_union,
                            pct_union - filter(df_diff_in_diff_figure_1, year == 2006 & type == "Surface")$pct_union))
  
ggplot(data = filter(df_diff_in_diff_figure, type == "Surface")) +
  geom_line(data = filter(df_diff_in_diff_figure, type == "Underground"), 
            aes(x = year, 
                y = pct_union_weighted, 
                color = 'Underground'), linetype = 5) +
  geom_line(data = filter(df_diff_in_diff_figure, type == "Surface"), 
            aes(x = year, 
                y = pct_union_weighted, 
                color = 'Surface'), linetype = 2) +
  geom_vline(xintercept = 2006, color = 'red', size = 0.5) +
  expand_limits(x = c(2002,2016), y = c(0,0.2)) +
  scale_x_continuous(breaks = seq(2002,2016,2)) +
  ylab('Share of mines unionized (weighted by number of employees)') +
  xlab('') +
  scale_color_manual(values = c(
    'Underground' = '#014d64',
    'Surface' = 'darkred')) +
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
  ggtitle('Trends in Unionization before and after MINER Act (2006)')

ggplot(data = filter(df_diff_in_diff_figure, type == "Surface")) +
  geom_line(data = filter(df_diff_in_diff_figure, type == "Underground"), 
            aes(x = year, 
                y = pct_union, 
                color = 'Underground'), linetype = 5) +
  geom_line(data = filter(df_diff_in_diff_figure, type == "Surface"), 
            aes(x = year, 
                y = pct_union, 
                color = 'Surface'), linetype = 2) +
  geom_vline(xintercept = 2006, color = 'red', size = 0.5) +
  expand_limits(x = c(2002,2016), y = c(0,0.2)) +
  scale_x_continuous(breaks = seq(2002,2016,2)) +
  ylab('Share of mines unionized (unadjusted)') +
  xlab('') +
  scale_color_manual(values = c(
    'Underground' = '#014d64',
    'Surface' = 'darkred')) +
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
  ggtitle('Trends in Unionization before and after MINER Act (2006)')
