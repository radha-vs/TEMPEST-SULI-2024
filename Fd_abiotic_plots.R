#Plotting Fd against abiotic factors for April 24 - May 2 2024, 11 AM - 12 PM 

#Packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

full_data <- readRDS("Full_042424_050224.rds")

full_data %>%
  select(TIMESTAMP, Plot, Grid.Cell, ID, Species, F_tot, soil_vwc_15cm, PAR, TEMP) %>%
  mutate(Hour = hour(TIMESTAMP)) %>%
  filter(Hour >= 11, Hour <= 12) %>%
  group_by(TIMESTAMP) -> full_data

#Plot of Fd vs soil vwc 
full_data %>%
  filter(soil_vwc_15cm < 0.8) %>%
  group_by(TIMESTAMP, Plot, Species) %>%
  summarize(avg_vwc = mean(soil_vwc_15cm), 
            avg_fd = mean(F_tot)) %>%
  ggplot(aes(x = avg_vwc, y = avg_fd, color = Species, group = Plot)) +
  geom_point(aes(fill = Plot, colour = Plot), size = 2.5)+
  geom_smooth(method = lm, formula = y ~ x, aes(fill = Plot, color = Plot)) +
  stat_poly_eq(formula = y ~ x,
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~"))) +
  facet_wrap(.~Species, ncol = 1, scales = "free")



full_data %>%
  filter(soil_vwc_15cm < 0.8) %>%
  group_by(TIMESTAMP, Plot, Species) %>%
  summarize(avg_vwc = mean(soil_vwc_15cm), 
            avg_fd = mean(F_tot)) %>%
  filter(Species == "Tulip Poplar") %>%
  ggplot(aes(x = avg_vwc, y = avg_fd, color = Species)) +
  geom_point(aes(fill = Plot, colour = Plot), size = 2.5)+
  geom_smooth(method = lm, formula = y ~ x, aes(fill = Plot, color = Plot))

#Plot of Fd vs PAR
full_data %>%
  filter(Plot == "Freshwater") %>%
  group_by(TIMESTAMP, Species) %>% 
  drop_na(F_tot, PAR) %>%
  summarize(avg_par = mean(PAR), 
            avg_fd = mean(F_tot)) %>%
  ggplot(aes(x = avg_par, y = avg_fd, color = Species)) + 
  geom_jitter()

ggplot(full_data, aes(x = TIMESTAMP, y = soil_vwc_15cm, color = Plot)) +
  geom_point(aes(fill = Plot, colour = Plot), size = 2.5)

full_data %>%
  group_by(TIMESTAMP, Plot) %>%
  summarize(count = length(soil_vwc_15cm))


all_data <- readRDS("sapflow_abiotic_complete.rds")

all_data %>%
  filter(research_name == "soil_vwc_15cm") %>%
  group_by(TIMESTAMP, Plot) %>%
  summarize(count = length(research_name)) -> vwc15_summary

vwc15_summary %>%
  filter(month(TIMESTAMP) == 4) -> april_summary

all_data %>%
  filter(research_name == "wx_") %>%
  group_by(TIMESTAMP, Plot) %>%
  summarize(count = length(research_name)) -> vwc15_summary
