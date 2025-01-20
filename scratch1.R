

data %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP)) %>%
  dplyr::filter(PAR > 400,
                Hour < 14, Hour >= 11,
                month(TIMESTAMP) > 5 & month(TIMESTAMP) < 8,
                `F` < 3.25) %>%
  group_by(Plot, ID, Date, Year, Species) %>%
  summarise(F_avg = mean(`F`, na.rm = TRUE),
            soil_ec_avg = mean(soil_ec_15cm), 
            soil_vwc_avg = mean(soil_vwc_15cm),
            n = n()) %>%
  ungroup() -> sun_summer

test <- left_join(sun_summer, vwc)

ggplot(sun_summer, aes(soil_vwc_avg, F_avg, color = Plot)) +
  geom_point() + geom_smooth(method = lm) +
  facet_grid(Species~Year) +
  theme_light()

ggplot(sun_summer, aes(soil_ec_avg, F_avg, color = Species)) +
  geom_point() + geom_smooth(method = lm) +
  facet_grid(Year~Plot, scales = "free") +
  theme_light()

test %>%
  dplyr::select(Plot, Date, Year, soil_vwc_avg, vwc5_avg, vwc30_avg) %>%
  pivot_longer(cols = c("soil_vwc_avg", "vwc5_avg", "vwc30_avg"), names_to = "SM",
               values_to = "value") -> test2

ggplot(test2, aes(Date, value, color = SM)) +
  geom_point() + geom_smooth() +
  facet_grid(Plot~Year, scales = "free_x") +
  theme_light()
  