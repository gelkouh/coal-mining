# Create union safety regression analysis results figures

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

output <- file.path(ddir, "Output")
input <- file.path(output, "_temp")

# Prepare data ------------------------------------------------------------

reg_output <- haven::read_dta(file.path(input, "15 union safety regression output.dta")) %T>%
  dplyr::glimpse()

reg_output_cleaned <- reg_output %>%
  dplyr::filter(grepl("1.union", var) & !grepl("c.size_100FTEs", var)) %>%
  dplyr::mutate(irr = coef) %>%
  dplyr::select(lhs_var, persistence_level, year_range, irr, starts_with("ci")) %T>%
  dplyr::glimpse()


# Create figures ----------------------------------------------------------

for (lhs_var_i in c("traumatic_injuries", "ss_violations")) {
  for (persistence_level_pct in c(70, 75, 80, 90)) {
    plot_data <- reg_output_cleaned %>%
      dplyr::filter(lhs_var == lhs_var_i,
                    persistence_level %in% c("MINE_ID_active_anypct", stringr::str_glue("MINE_ID_active_{persistence_level_pct}pct")),
                    !(year_range %in% c("2000_2010", "2011_2022"))) %>%
      dplyr::mutate(year_range = stringr::str_replace(year_range, "_", "-"),
                    persistence_level = case_when(persistence_level == "MINE_ID_active_anypct" ~ "All Mines",
                                                  persistence_level == stringr::str_glue("MINE_ID_active_{persistence_level_pct}pct") ~ stringr::str_glue("Mines Active in {persistence_level_pct} Pct\nof Quarters 2000-2022")))
    
    irr_plot <- ggplot(plot_data,
                       aes(year_range, irr)) +
      geom_hline(yintercept = 1, linewidth = 0.75) +
      scale_y_continuous(breaks = seq(0, max(plot_data$ci_upper)+0.25, 0.25),
                         limits = c(0, max(plot_data$ci_upper)+0.25)) +
      geom_text(aes("2000-2004",1,label = "IRR = 1", vjust = -0.5, hjust = 2), family = "serif") +
      # geom_col(aes(fill = persistence_level), position = position_dodge(0.8), alpha = 0.9, width = 0.75) +
      # scale_fill_manual(values = c("#018571", "#dfc27d")) +
      geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, group = persistence_level), 
                    position = position_dodge(0.8), width = 0.2) +
      geom_point(aes(color = persistence_level, shape = persistence_level), alpha = 0.9, position = position_dodge(0.8), size = 5) +
      scale_color_manual(values = c("#018571", "#dfc27d")) +
      labs(x = NULL, 
           y = "IRR Coefficients on Union Indicator Variables") +
      prop_functions$prop_theme()
    
    prop_functions$prop_save(irr_plot, file.path("../../output/figures/irr_figures_union_premium", stringr::str_glue("irr_figure_MINE_ID_active_{persistence_level_pct}pct_{lhs_var_i}")))
  }
}
