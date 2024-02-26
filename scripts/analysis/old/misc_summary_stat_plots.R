# MINER Act violations
ggplot(data = filter(mine_panel, year > 2006), aes(x = miner_act_violations, fill = union)) + 
  geom_histogram(position = "dodge") + 
  facet_grid(union ~ ., margins = TRUE, scales = "free") +
  scale_x_log10() +
  scale_fill_manual(values = c("#014d64","darkred","gray")) +
  xlab("Number of violations") +
  ylab("Number of mines") +
  ggtitle("Number of total MINER Act violations per mine per quarter by union status (2007-2021)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5))
ggplot(data = filter(mine_panel, year > 2006), aes(x = miner_act_violations_serious, fill = union)) + 
  geom_histogram(position = "dodge") + 
  facet_grid(union ~ ., margins = TRUE, scales = "free") +
  scale_x_log10() +
  scale_fill_manual(values = c("#014d64","darkred","gray")) +
  xlab("Number of S&S MINER Act violations") +
  ylab("Number of mines") +
  ggtitle("Number of S&S MINER Act violations per mine per quarter by union status (2007-2021)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5))

# Distribution of driving distances to MSHA offices
driving_distance_union_fig <- ggplot(inspector_offices_union_df) +
  geom_histogram(data=subset(inspector_offices_union_df, union == 1), 
                 aes(x = inspector_office_dist_miles), bins = 40, 
                 fill = "darkred", alpha = 0.9, position = "dodge") +
  xlab("Miles") +
  ylab("Number of mines") +
  ggtitle("Driving distance to the mine from inspection office (Union)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5))
driving_distance_under100_union_fig <- ggplot(inspector_offices_union_df) +
  geom_histogram(data=subset(inspector_offices_union_df, union == 1 & inspector_office_dist_miles < 100), 
                 aes(x = inspector_office_dist_miles), bins = 40, 
                 fill = "darkred", alpha = 0.9, position = "dodge") +
  xlab("Miles") +
  ylab("Number of mines") +
  ggtitle("Driving distance to the mine from inspection office, < 100 miles (Union)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5))
driving_distance_nonunion_fig <- ggplot(inspector_offices_union_df) +
  geom_histogram(data=subset(inspector_offices_union_df, union == 0), 
                 aes(x = inspector_office_dist_miles), bins = 40, 
                 fill = "#014d64", alpha = 0.9, position = "dodge") +
  xlab("Miles") +
  ylab("Number of mines") +
  ggtitle("Driving distance to the mine from inspection office (Nonunion)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5))
driving_distance_under100_nonunion_fig <- ggplot(inspector_offices_union_df) +
  geom_histogram(data=subset(inspector_offices_union_df, union == 0 & inspector_office_dist_miles < 100), 
                 aes(x = inspector_office_dist_miles), bins = 40, 
                 fill = "#014d64", alpha = 0.9, position = "dodge") +
  xlab("Miles") +
  ylab("Number of mines") +
  ggtitle("Driving distance to the mine from inspection office, < 100 miles (Nonunion)") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        panel.background = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5, size = 10, color = 'black'),
        axis.text.x = element_text(size = 10, color = 'black'),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust = 0.5))

ggsave(file.path(outputdir, 'driving_distance_union_fig.png'), 
       plot = driving_distance_union_fig, width = 8, height = 6)
ggsave(file.path(outputdir, 'driving_distance_under100_union_fig.png'), 
       plot = driving_distance_under100_union_fig, width = 8, height = 6)
ggsave(file.path(outputdir, 'driving_distance_nonunion_fig.png'), 
       plot = driving_distance_nonunion_fig, width = 8, height = 6)
ggsave(file.path(outputdir, 'driving_distance_under100_nonunion_fig.png'), 
       plot = driving_distance_under100_nonunion_fig, width = 8, height = 6)

# Price data over time
contracts_quarterly <- contracts %>%
  mutate(quarter = quarter(date),
         quarter_fraction = (as.numeric(quarter)-1.0)*0.25,
         year_quarter = as.numeric(year) + as.numeric(quarter_fraction)) %>%
  filter(contract_type %in% c("Contract", "Spot")) %>%
  group_by(year_quarter, union, contract_type) %>%
  summarize(numerator = sum(QTY_COAL_PURCH_x_PRICE_PER_TON, na.rm = TRUE),
            denominator = sum(QTY_COAL_PURCH, na.rm = TRUE),
            weighted_avg_price_per_ton = numerator/denominator,
            num_contracts = n())

contracts_yearly <- contracts %>%
  mutate(quarter = quarter(date),
         quarter_fraction = (as.numeric(quarter)-1.0)*0.25,
         year_quarter = as.numeric(year) + as.numeric(quarter_fraction)) %>%
  filter(contract_type %in% c("Contract", "Spot")) %>%
  group_by(year, union, contract_type) %>%
  summarize(numerator = sum(QTY_COAL_PURCH_x_PRICE_PER_TON, na.rm = TRUE),
            denominator = sum(QTY_COAL_PURCH, na.rm = TRUE),
            weighted_avg_price_per_ton = numerator/denominator,
            num_contracts = n(),
            qty_coal_purchased = mean(QTY_COAL_PURCH, na.rm = TRUE))

avg_contract_price_q_fig <- ggplot(contracts_quarterly) + 
  geom_line(data=subset(filter(contracts_quarterly, contract_type == "Contract"), union == 0),aes(x=year_quarter,
                                                               y=weighted_avg_price_per_ton,
                                                               color='Contract: Nonunion'), linetype = 1) +
  geom_line(data=subset(filter(contracts_quarterly, contract_type == "Spot"), union == 0),aes(x=year_quarter,
                                                               y=weighted_avg_price_per_ton,
                                                               color='Spot: Nonunion'), linetype = 5) +
  geom_line(data=subset(filter(contracts_quarterly, contract_type == "Contract"), union == 1),aes(x=year_quarter,
                                                               y=weighted_avg_price_per_ton,
                                                               color='Contract: Union'), linetype = 1) +
  geom_line(data=subset(filter(contracts_quarterly, contract_type == "Spot"), union == 1),aes(x=year_quarter,
                                                               y=weighted_avg_price_per_ton,
                                                               color='Spot: Union'), linetype = 5) +
  ylab('Average pice per ton (weighted using contract quantity, adjusted with PPI)') +
  scale_x_continuous(labels = function(x) {
    paste0(x, "q1")
  }) + 
  xlab("Quarter") +
  scale_color_manual(values = c(
    "Contract: Nonunion" = '#014d64',
    "Spot: Nonunion" = '#014d64',
    "Contract: Union" = 'darkred',
    "Spot: Union" = 'darkred')) +
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
  ggtitle('Price of Bituminous Coal from Underground Mines: 2000-2021')
avg_contract_price_y_fig <- ggplot(contracts_yearly) + 
  geom_line(data=subset(filter(contracts_yearly, contract_type == "Contract"), union == 0),aes(x=year,
                                                                                                  y=weighted_avg_price_per_ton,
                                                                                                  color='Contract: Nonunion'), linetype = 1) +
  geom_line(data=subset(filter(contracts_yearly, contract_type == "Spot"), union == 0),aes(x=year,
                                                                                              y=weighted_avg_price_per_ton,
                                                                                              color='Spot: Nonunion'), linetype = 5) +
  geom_line(data=subset(filter(contracts_yearly, contract_type == "Contract"), union == 1),aes(x=year,
                                                                                                  y=weighted_avg_price_per_ton,
                                                                                                  color='Contract: Union'), linetype = 1) +
  geom_line(data=subset(filter(contracts_yearly, contract_type == "Spot"), union == 1),aes(x=year,
                                                                                              y=weighted_avg_price_per_ton,
                                                                                              color='Spot: Union'), linetype = 5) +
  ylab('Average pice per ton (weighted using contract quantity, adjusted with PPI)') +
  #scale_y_continuous(breaks = seq(0,80,5)) +
  xlab("Year") +
  scale_color_manual(values = c(
    "Contract: Nonunion" = '#014d64',
    "Spot: Nonunion" = '#014d64',
    "Contract: Union" = 'darkred',
    "Spot: Union" = 'darkred')) +
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
  ggtitle('Price of Bituminous Coal from Underground Mines: 2000-2021')

ggsave(file.path(outputdir, 'avg_contract_price_y_fig.png'), 
       plot = avg_contract_price_y_fig, width = 8, height = 6)

avg_contract_qty_y_fig <- ggplot(contracts_yearly) + 
  geom_line(data=subset(filter(contracts_yearly, contract_type == "Contract"), union == 0),aes(x=year,
                                                                                               y=qty_coal_purchased,
                                                                                               color='Contract: Nonunion'), linetype = 1) +
  geom_line(data=subset(filter(contracts_yearly, contract_type == "Spot"), union == 0),aes(x=year,
                                                                                           y=qty_coal_purchased,
                                                                                           color='Spot: Nonunion'), linetype = 5) +
  geom_line(data=subset(filter(contracts_yearly, contract_type == "Contract"), union == 1),aes(x=year,
                                                                                               y=qty_coal_purchased,
                                                                                               color='Contract: Union'), linetype = 1) +
  geom_line(data=subset(filter(contracts_yearly, contract_type == "Spot"), union == 1),aes(x=year,
                                                                                           y=qty_coal_purchased,
                                                                                           color='Spot: Union'), linetype = 5) +
  ylab('Average contract quantity (tons)') +
  scale_y_continuous(breaks = seq(0,175,25)) +
  xlab("Year") +
  scale_color_manual(values = c(
    "Contract: Nonunion" = '#014d64',
    "Spot: Nonunion" = '#014d64',
    "Contract: Union" = 'darkred',
    "Spot: Union" = 'darkred')) +
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
  ggtitle('Contract Quantity of Bituminous Coal from Underground Mines: 2000-2021')

ggsave(file.path(outputdir, 'avg_contract_qty_y_fig.png'), 
       plot = avg_contract_qty_y_fig, width = 8, height = 6)

##----------##
# Summary statistics and tables
##----------##

nrow(mine_panel)
nrow(contracts)

mine_panel %>%
mutate(union_code = ifelse("United Mine Workers Of America" == union_code, "United Mine Workers of America", union_code),
         union_code = ifelse("United Mine Workers of America" == union_code, "UMWA", union_code),
         union_code = ifelse(is.na(union_code), "Nonunion", union_code)) %>%
select(MINE_ID, union_code) %>%
  distinct() %>%
  group_by(union_code) %>%
  summarize(num_mines = n())

mean(filter(contracts, union == 0, contract_type == "Contract")$QTY_COAL_PURCH)
mean(filter(contracts, union == 1, contract_type == "Contract")$QTY_COAL_PURCH)

