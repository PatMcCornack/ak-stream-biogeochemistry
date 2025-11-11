## Purpose: Plot discharge from multiple years on same plots
## Inputs: Aggregated discharge data from each year
## Outputs: Discharge plots
## Author: Pat McCornack
## Date: 08/01/2025

library(tidyverse)
library(here)
library(ggplot2)
library(lubridate)

### EDIT HERE ####
## Define Paths
plots_outdir <- 'Plots/aggregated_years/discharge'

## Define functions ----
plot_year <- function(q_year, sitename) {
  q_year <- q_year %>% filter(site == sitename)
    p <- ggplot(data=q_year) + 
      geom_point(aes(x=datetime, y=discharge_L.s, color=method)) +
      scale_x_datetime(date_labels='%Y-%m-%d', date_breaks='1 week') +
      labs(
        title = paste(sitename, "Discharge"),
        x = "Date",
        y = "Discharge (L/s)",
        color = "Method"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
        plot.subtitle = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_blank()
      )
    return(p)
}

## Read in data ----
# Each year requires slightly different processing
setwd(here('data/all_years'))
col_names <- c('site', 'datetime', 'method', 'discharge_L.s')

# Read in 2020
q_2020 <- read.csv('Q_summary_2020.csv') %>% 
  mutate(Date = mdy(Date)) %>% 
  filter(!is.na(Method) & Method != "") %>%
  mutate(Time = ifelse(is.na(Time) | Time == "", "12:00:00", Time)) %>%   # Replace empty Times with dummy
  mutate(datetime=paste(Date, Time)) %>%
  select('Site', 'datetime', 'Method', 'MeasuredQ_Ls') %>% 
  setNames(col_names) %>%
  filter(method != 'YSI') %>%
  mutate(method = ifelse(method == 'Wading rod', 'wading_rod', method),
         datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M')) 


# Read in 2021
q_2021 <- read.csv('Q_summary_2021.csv') %>% 
  filter(!is.na(Method) & Method != "") %>%
  mutate(Time = ifelse(is.na(Time) | Time == "", "12:00:00", Time)) %>%   # Replace empty Times with dummy
  mutate(datetime=paste(Date, Time)) %>%
  select('Site', 'datetime', 'Method', 'MeasuredQ_Ls') %>% 
  setNames(col_names) %>%
  filter(method != 'YSI') %>%
  mutate(method = ifelse(method == 'Wading rod', 'wading_rod', method),
         datetime = as.POSIXct(datetime, format='%m/%d/%Y %H:%M')) 

# Read in 2022
q_2022 <- read.csv('Q_summary_2022.csv') %>% 
  filter(!is.na(Method) & Method != "") %>%
  mutate(Time = ifelse(is.na(Time) | Time == "", "12:00:00", Time)) %>%   # Replace empty Times with dummy
  mutate(datetime=paste(Date, Time)) %>%
  select('Site', 'datetime', 'Method', 'MeasuredQ_Ls') %>% 
  setNames(col_names) %>%
  filter(method != 'YSI') %>%
  mutate(method = ifelse(method == 'Velocity Meter', 'wading_rod', method),
         method = ifelse(method == 'Wading Rod', 'wading_rod', method),
         datetime = as.POSIXct(datetime, format='%m/%d/%Y %H:%M'))  

# Read in 2024
q_2024 <- read.csv('Q_summary_2024.csv') %>% 
  mutate(datetime=paste(Date, '12:00:00')) %>%
  select('Site', 'datetime', 'Method', 'MeasuredQ_Ls') %>% 
  setNames(col_names) %>%
  filter(method != 'YSI') %>%
  mutate(method = ifelse(method == 'Flow Tracker', 'flowtracker', method),  # For consistency
         method = ifelse(method == 'Velocity Meter', 'wading_rod', method),
         datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S'))
  
# Read in 2025
q_2025 <- read.csv('Q_summary_2025.csv') %>% 
  select(site, datetime, method, discharge_L.s) %>%
  filter(method != 'slug') %>%
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S'))

# Join all Q data
q_df <- bind_rows(q_2020, q_2021, q_2022, q_2024, q_2025) %>%
  mutate(year = year(datetime), 
         month_day = as.Date(format(datetime, "%m-%d"), format='%m-%d'))  # Used to stack data in plot

setwd(here())
write.csv(q_df, 'data/all_years/Q_summary.csv')

## Plot data ----
sites <- c('POKE', 'MOOS', 'FRCH', 'ELDO', 'VAUL')

# 2024
for(site in sites) {
  p <- plot_year(q_2024, site)
  print(p)
}

# Aggregated years
for(sitename in sites){
  q_site_df <- q_df %>% filter(site==sitename)
  p <- ggplot(data=q_site_df) + 
        geom_point(aes(x=month_day, y=discharge_L.s, color=factor(year), shape=method)) +
        scale_x_date(date_labels="%b %d", date_breaks='1 week') +
        labs(
          title = paste(sitename, "Discharge"),
          x = "Date",
          y = "Discharge (L/s)",
          color = "Year",
          shape = "Method"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right",
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_blank()
        )
  
  ggsave(paste0(sitename, '_Q.jpg'), 
         plot=p,
         path = plots_outdir,
         width = 8, height = 5)
}



