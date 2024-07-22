# header


# Summary statistics
# https://cran.r-project.org/web/packages/vtable/vignettes/sumtable.html
labs_all <- c('Year', 'Quarter', 'Union Status (=1 for unionized)',
              'Mine Age', 'Size (100 FTEs)', 'Controller Size (100 FTEs)',
              'Coal Production (short tons)', 'Underground Labor Hours', 'Productivity (tons/2,000 hrs)',
              'Total Injuries', "Traumatic Injuries", "Total Violations", "S\\&S Violations",
              'Active Mines in County', 'County HHI')
st(filter(mine_panel_quarters, year >= 2000, year <= 2021, zero_production_quarter == 0), 
   vars = c("year", "quarter", "union",
            "mine_age", "size_100FTEs", "controller_size_100FTEs",
            "coal_production_tons", "labor_hours", "productivity", 
            "total_injuries", "traumatic_injuries", "violations", "ss_violations",
            "active_mines", "hhi_county"),
   labels = labs_all,
   out='latex',
   file=file.path(outputdir, 'tables', 'sumstat_all.tex'),
   title = "Active Underground Bituminous Mine-Quarters, 2000-2021")

labs_union <- c('Year', 'Quarter',
                'Mine Age', 'Size (100 FTEs)', 'Controller Size (100 FTEs)',
                'Coal Production (short tons)', 'Underground Labor Hours', 'Productivity (tons/2,000 hrs)',
                'Total Injuries', "Traumatic Injuries", "Total Violations", "S\\&S Violations",
                'Active Mines in County', 'County HHI')
st(filter(mine_panel_quarters, year >= 2000, year <= 2021, zero_production_quarter == 0), 
   group = "union",
   group.long = TRUE,
   vars = c("year", "quarter",
            "mine_age", "size_100FTEs", "controller_size_100FTEs",
            "coal_production_tons", "labor_hours", "productivity", 
            "total_injuries", "traumatic_injuries", "violations", "ss_violations",
            "active_mines", "hhi_county"),
   labels = labs_union,
   out='latex',
   file=file.path(outputdir, 'tables', 'sumstat_all_union.tex'),
   title = "Active Underground Bituminous Mine-Quarters by Union Status, 2000-2021")

labs_mineract <- c('Year', 'Quarter', 'Union Status (=1 for unionized)',
                   'Mine Age', 'Size (100 FTEs)', 'Controller Size (100 FTEs)',
                   'Coal Production (short tons)', 'Underground Labor Hours', 'Productivity (tons/2,000 hrs)',
                   'Total Injuries', "Traumatic Injuries", "Total Violations", "S\\&S Violations",
                   "MINER Act Violations", "S\\&S MINER Act Violations",
                   'Active Mines in County', 'County HHI')
st(filter(mine_panel_quarters, year >= 2007, year <= 2021, zero_production_quarter == 0), 
   vars = c("year", "quarter", "union",
            "mine_age", "size_100FTEs", "controller_size_100FTEs",
            "coal_production_tons", "labor_hours", "productivity", 
            "total_injuries", "traumatic_injuries", "violations", "ss_violations",
            "violations_miner_act", "ss_violations_miner_act",
            "active_mines", "hhi_county"),
   labels = labs_mineract,
   out='latex',
   file=file.path(outputdir, 'tables', 'sumstat_mineract.tex'),
   title = "Active Underground Bituminous Mine-Quarters, 2007-2021")

labs_mineract_union <- c('Year', 'Quarter',
                         'Mine Age', 'Size (100 FTEs)', 'Controller Size (100 FTEs)',
                         'Coal Production (short tons)', 'Underground Labor Hours', 'Productivity (tons/2,000 hrs)',
                         'Total Injuries', "Traumatic Injuries", "Total Violations", "S\\&S Violations",
                         "MINER Act Violations", "S\\&S MINER Act Violations",
                         'Active Mines in County', 'County HHI')
st(filter(mine_panel_quarters, year >= 2007, year <= 2021, zero_production_quarter == 0), 
   group = "union",
   group.long = TRUE,
   vars = c("year", "quarter",
            "mine_age", "size_100FTEs", "controller_size_100FTEs",
            "coal_production_tons", "labor_hours", "productivity", 
            "total_injuries", "traumatic_injuries", "violations", "ss_violations",
            "violations_miner_act", "ss_violations_miner_act",
            "active_mines", "hhi_county"),
   labels = labs_mineract_union,
   out='latex',
   file=file.path(outputdir, 'tables', 'sumstat_mineract_union.tex'),
   title = "Active Underground Bituminous Mine-Quarters by Union Status, 2007-2021")

