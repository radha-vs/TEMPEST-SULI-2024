
vwc5 <- tmp_vwc %>%
  filter(research_name == "soil_vwc_5cm") %>%
  group_by(TIMESTAMP, Plot) %>%
  drop_na(Value) %>%
  summarize(soil_vwc_5cm = mean(Value))

vwc30 <- tmp_vwc %>%
  filter(research_name == "soil_vwc_30cm") %>%
  group_by(TIMESTAMP, Plot) %>%
  drop_na(Value) %>%
  summarize(soil_vwc_30cm = mean(Value))

#rm(tmp_vwc)

tmp_vwc2 <- full_join(vwc5, vwc30)

tmp_vwc2 %>%
  mutate(Date = as_date(date(TIMESTAMP)),
         Hour = hour(TIMESTAMP),
         Year = year(TIMESTAMP),
         Plot = substr(Plot,1,1),
         Plot = case_when(Plot == "C" ~ "Control",
                          Plot == "F" ~ "Freshwater",
                          Plot == "S" ~ "Saltwater", )) %>%
  filter(Hour < 14, Hour >= 11,
         month(TIMESTAMP) > 5 & month(TIMESTAMP) < 8) %>%
  group_by(Plot, Date, Year) %>%
  summarise(vwc5_avg = mean(soil_vwc_5cm),
            vwc30_avg = mean(soil_vwc_30cm),
            n = n()) %>%
  ungroup() -> vwc

ggplot(vwc, aes(Date, vwc5_avg, color = Plot)) +
  geom_point() + geom_smooth() +
  facet_grid(.~Year, scales = "free_x") +
  theme_light()

ggplot(vwc, aes(Date, vwc30_avg, color = Plot)) +
  geom_point() + geom_smooth() +
  facet_grid(.~Year, scales = "free_x") +
  theme_light()

ggplot(tmp_vwc2[tmp_vwc2$soil_vwc_5cm < 1,],
       aes(TIMESTAMP, soil_vwc_5cm,
           color = Plot)) +
  geom_point() +
  facet_wrap(.~year(TIMESTAMP),scales = "free_x")

ggplot(tmp_vwc2[tmp_vwc2$soil_vwc_30cm < 1,],
       aes(TIMESTAMP, soil_vwc_30cm,
           color = Plot)) +
  geom_point() +
  facet_wrap(.~year(TIMESTAMP),scales = "free_x")
