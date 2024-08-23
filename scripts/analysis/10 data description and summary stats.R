#distn of safety in union vs. non-union mines 
#distn of safety in 90 pct vs. less than 40 pct mines
#distn accidents per violation 
#map mines colored by district
#map mines colored by office

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
  labs(title = "Active Underground Bituminous Coal Mines in Sample: 2000-2021") + 
  geom_point(data = transformed_data, aes(x = longitude.1, y = latitude.1), color = "black", size = 0.5)
ggsave(file.path(outputdir, 'us_mines_map_2000_2021.png'), 
       plot = us_mines_map_2000_2021, width = 8, height = 6)
