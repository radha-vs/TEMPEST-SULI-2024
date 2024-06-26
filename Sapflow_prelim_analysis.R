#Sapflow Initial Analysis
#Radha Srinivasan (Based on Stephanie Pennington's sapflow.rmd script)
#2024-06-18

#Conversion of sapflow voltage difference to sap flux density (Fd) via Granier (1985) equation
#Average values of Fd for last two weeks of April 2024 from 9-10 AM calculated and plotted
#2-way ANOVA test for statistical significance between treatments and plots conducted

#load in packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)

#load in sapflow data (already bound into single csv from 
#create-time-series.R script) 

readRDS("sapflow_abiotic_complete.rds") -> sapflow_data

#start and end times for data
DATA_BEGIN <- as_datetime("2024-04-17 00:00:00")
DATA_END <- as_datetime("2024-04-30 23:59:59")

#These values are from 2022's TEMPEST day 1 event
#Edit if needed to highlight any specific timeframe of data
#EVENT_START <- as_datetime("2022-06-22 05:30:00")
#EVENT_STOP <- as_datetime("2022-06-22 14:30:00")

#Tidy up data
sapflow_data <- sapflow_data %>% 
  mutate(Value = as.numeric(Value),
         Date = date(TIMESTAMP),
         Hour = hour(TIMESTAMP)) %>% 
  filter(#TIMESTAMP >= DATA_BEGIN, TIMESTAMP <= DATA_END, 
         Value >= 0.01, Value <=1, !grepl("D", Sensor_ID)) 

#Add species column to sapflow_data
#Create new dataframe 
species <- read.csv("TEMPEST_TreeChamberInstallation_11272023.csv")
sapflow_data <- merge(species, sapflow_data, by.x = "ID", by.y = "Sensor_ID", all.x = TRUE)

#Creating just one plot column for simplicity
sapflow_data %>% 
  drop_na(Value) %>%
  mutate(Plot = Plot.y) %>%
  select(TIMESTAMP, Date, Hour, ID, Species, Plot, Location, Value) -> sf_dat

#Ignore this lol
  #mutate(plot = substr(ID,1,1),
         #plot = case_when(plot == "C" ~ "Control",
                          #plot == "S" ~ "Saltwater",
                          #plot == "F" ~ "Freshwater")) -> sf_dat

#From Steph: For dTmax, we're using the max sapflow method, which needs to meet the following criteria:
# (1) Be between the hours of midnight and 5am
# (2) Be the maximum sapflow value

#Seems like using 12-5 am results in a fair amount of NA values
#There are times during the day with high dT values, so we'll just use the overall dTmax

#Filter accordingly: 
sf_dat %>% 
  #filter(Hour >= 0, Hour <= 5) %>% 
  group_by(Date, Plot, Species, ID) %>% 
  summarise(dTmax = max(Value, na.rm = TRUE),
            dTmax_Timestamp = TIMESTAMP[which.max(Value)]) -> sapflow_dtmax

#Graph sapflow data with dtmax calculation to double check correct calculation: 
sf_dat %>% 
  filter(Plot == "C") %>% 
  ggplot(aes(x = TIMESTAMP, y = Value, group = Species, color= as.factor(Species))) + 
  geom_line() + ggtitle("dTmax should maximum value each day") +
  geom_point(data = filter(sapflow_dtmax, Plot == "C"), aes(x = dTmax_Timestamp, y = dTmax), color = "black") +
  facet_wrap(ID~Plot, scales = "free") 
#dTmax should be the max Value for the 24 hour day
#dTmax values are on the peaks of the daily values so looks good!


#Granier 1985 equation + convert to g/m^2/hour
#Fd is sap flux density (m^3/m^2 * s)
sf_dat %>% 
  left_join(sapflow_dtmax, by = c("Date", "Plot", "Species", "ID")) %>% 
  mutate(Fd = 360000 * (0.00011899) * (((dTmax / Value) - 1)^1.231)) -> sfd_data

#Graph to double check:
ggplot(data = sfd_data, aes(x = TIMESTAMP, y = (Fd), group = Species, color = as.factor(Species))) + 
  geom_line() + ggtitle("dTmax should now be minimum value each day") +
  geom_point(data = sapflow_dtmax, aes(x = dTmax_Timestamp, y = dTmax), color = "black") +
  facet_wrap(~Plot, ncol = 1) 
#Looks good! 
#Voltage difference and flow rate are inversely proportional; dtMax is defined as the lowest Fd for
#a 24 hr period. 


#when is max Fd?
sfd_data %>% 
  group_by(Date, Plot, Species, ID) %>% 
  summarise(Fdmax = max(Fd, na.rm = TRUE),
            Fdmax_time = TIMESTAMP[which.max(Fd)]) -> maxFd_time

hist(hour(maxFd_time$Fdmax_time))

#Does it vary seasonally?
ggplot(maxFd_time, aes(x = hour(Fdmax_time), fill = Species)) +
  geom_histogram() + ggtitle("Monthly Max Fd Time")+
  facet_wrap(.~month(Fdmax_time))
#not a lot

#Hours 11-12 PM for most recent data averaged by plot and species
#Note: This is an average every 15 minutes from 11-12 PM, if you just wanted an average for the hour as a whole lmk!
sfd_data %>% 
  filter(Hour <=12, Hour >= 11) %>% 
  group_by(Plot, Date, Species) %>% 
  summarise(Fd_avg = mean(Fd, na.rm = TRUE),
            Fd_error = sd(Fd, na.rm = TRUE)) %>% 
  mutate(Fd_avg = round(Fd_avg, digits = 3),
         Fd_error = round(Fd_error, digits = 3)) -> sfd_plot_avg

ggplot(sfd_plot_avg[sfd_plot_avg$Date >= "2024-04-24",]) + 
  geom_line(aes (x = Date, y = Fd_avg, color = Species)) +
  geom_errorbar(aes(ymin = Fd_avg - Fd_error, ymax = Fd_avg + Fd_error,
                    x = Date, color = Species)) +
  facet_wrap(~Plot, ncol = 1) #+
  #scale_x_continuous(breaks = pretty(sfd_data$TIMESTAMP, n = 10))

#ANOVA test to determine if difference in Fd of different treatments and species are statistically significant) 

sfd_data %>%
  filter(Hour <= 12, Hour >= 11,
         Date >= "2024-04-24") -> sfd_data_pm

sapflow_aov <- aov(Fd ~ Species * Plot, data = sfd_data_pm)

summary(sapflow_aov)
#The p-value is <<0.05. 
#The differences in Fd between species and treatments are statistically significant. 

ggplot(sfd_data_pm) + 
  geom_boxplot(aes (x = Plot, y = Fd, fill = Species)) +
  scale_x_discrete(labels = c("C" = "Control",
                              "F" = "Freshwater",
                              "S" = "Saltwater")) +
  theme_light()
