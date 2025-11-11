## Purpose: Make C-Q plots for data* for daily scale values across all years and stratified by year. 
##          *no3, fDOM, spcond, turb, abs254
## Author: Pat McCornack
## Date: 08/26/25
## Credits: This script was adapted from NO3_Q_relationships.R written by Anna Wright.
##
## NOTES: 
#  1. I've marked variables that may need to be changed based on file structure with >>EDIT HERE<<.

## TO DO: 
#  1. Look into pulling data from another repo without referencing local copy.


library(tidyverse)
library(here)

# Define Directory paths ----
## Data directories
sensor_dir <- here('interpolated_obs')
Q_dir <- here('../DoD_Discharge_copy/Predicted_Discharge')  # >>EDIT HERE<< This may need to changed depending on local file structure
                                                            # because it's referencing another repo

## Plot directories
plot_dir <- here('plots/C-Q_plots_PM')


# Read in and preprocess data ----

## Read Sensor data ----
# Join together sensor data from all years
sensor_files <- list.files(sensor_dir, pattern='.csv', full.names=TRUE)
sensor_df <- bind_rows(lapply(sensor_files, read.csv))

# Preprocess sensor data
sensor_df <- sensor_df %>% 
  arrange(site.ID, datetimeAK) %>%  # Sort by location/time
  mutate(datetimeAK = as.POSIXct(datetimeAK, format="%Y-%m-%d %H:%M:%S", tz='America/Anchorage')) %>%  # Cast to datetime type
  select(datetimeAK, Site, Temp.C.mn, SpCond.uScm.int, no3.uM.lab.int, fDOM.QSU.int, Turb.FNU.int, abs254.int)  %>% # Subset to variables of interest
  rename(site = Site, 
         datetime = datetimeAK)


## Read Discharge data ----
# Join together discharge data from all years
### Define functions to process data - years not consistently structured ----
process_2020_2021 <- function(fpath, col_names=c('datetime', 'site', 'q_ls')) {
  # Process files from 2020 and 2021
  df <- read.csv(fpath) %>%
    mutate(DateTime = as.POSIXct(DateTime, format="%Y-%m-%d %H:%M:%S", tz='America/Anchorage')) %>%
    select(DateTime, Site, Q) %>%
    set_names(col_names) %>%
    filter(!is.na(q_ls)) 
  
  return(df)
}

process_2019_2022 <- function(fpath, col_names=c('datetime', 'site', 'q_ls')) {
  # Process files from 2019 and 2022
  df <- read.csv(fpath) %>%
    mutate(DateTimeAK = as.POSIXct(DateTimeAK, format="%Y-%m-%d %H:%M:%S", tz='America/Anchorage')) %>%
    pivot_longer(cols=c(FRCH:VAUL), names_to='Site', values_to='Q_Ls') %>% 
    select(DateTimeAK, Site, Q_Ls) %>%
    set_names(col_names) %>%
    filter(!is.na(q_ls)) 
}

### Read in / process Q data ----
Q_2019 <- process_2019_2022(file.path(Q_dir, '2019/Predicted_Q_2019_gapfill.csv'))
Q_2020 <- process_2020_2021(file.path(Q_dir, '2020/Q_2020.csv'))
Q_2021 <- process_2020_2021(file.path(Q_dir, '2021/Q_2021.csv'))
Q_2022 <- process_2019_2022(file.path(Q_dir, '2022/Predicted_Q_2022_gapfill.csv'))

Q_df <- bind_rows(Q_2019, Q_2020, Q_2021, Q_2022)  # Join all discharge data
  

## Join C-Q ----
cq_df <- full_join(sensor_df, Q_df, by=c('datetime', 'site')) %>%
  mutate(year = year(datetime),
         month = month(datetime),
         hour = hour(datetime))

min_year <- min(cq_df$year)
max_year <- max(cq_df$year)


# Aggregated across Years ----

## Process aggregated data ----
### Aggregate data ----
cq_hourly <- cq_df %>% 
  group_by(month, site, hour) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE)),  # Take mean of every column
            n = n(),  # Get counts of each group (site, year, hour)
            .groups='drop') %>%
  filter(n > 10)

### Normalize the data ----

# Function to normalize
normalize.data <- function(x, na.rm = TRUE){(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}

vars <- colnames(cq_hourly)[-c(1:3, 11, 12)]  # variables to normalize

cq_hourly_norm <- cq_hourly %>%
  group_by(site) %>%
  mutate(across(vars, ~normalize.data(.x, ))) %>%  # apply normalize function to each variable
  ungroup()


## Plot ----

# Dictionaries for plot labels
title_dict <- c(
  "FRCH" = "A) French 33%",
  "MOOS" = "B) Moose 38%",
  "POKE" = "C) Poker 25%",
  "STRT"= "D) Stuart 31%",
  "VAUL" = "E) Vault 58%"
)

var_dict <- c(
  "SpCond.uScm.int" = "SPC (uS/cm)",
  "no3.uM.lab.int" = "NO3 (uM)",
  "fDOM.QSU.int" = "fDOM (QSU)",
  "Turb.FNU.int" = "Turbidity (FNU)",
  "abs254.int" = "abs254"
)


### By month for all sites ----
# Intersite comparisons

months <- unique(cq_hourly_norm['month'])
months <- as.vector(unlist(months))

for(m in months){
  df <- cq_hourly_norm %>% 
    filter(month == m)
  
  for(v in names(var_dict)){  # For each measurement
    p <- ggplot(df, aes(x=q_ls, y=!!sym(v), color=hour)) +
          geom_point(size=3) +
          labs(title = paste(month.name[m], var_dict[v]),
               subtitle = paste0('Normalized Values ', min_year, '-', max_year),
               x = 'Q (L/s)',
               y = var_dict[v]) +
          facet_wrap(~ site, scales = 'free', labeller = labeller(site=title_dict)) +
          theme_light() +
          theme(text = element_text(size=24))
    #print(p)
    
    fname <- paste0('aggregated_years/', strsplit(v, "\\.")[[1]][1], '_', m , '_', month.name[m], '_norm.png')  # var_m_norm.png
    ggsave(fname, p, path=plot_dir, width=15, height=12)
  }
}


### By site for all months ----
# Intermonth comparisons

sites <- unique(cq_hourly_norm['site'])
sites <- as.vector(unlist(sites))

for(s in sites){
  df <- cq_hourly_norm %>% 
    filter(site == s) %>%
    mutate(month_label = factor(month.name[month], levels = month.name))
  
  for(v in names(var_dict)){  # For each measurement
    p <- ggplot(df, aes(x=q_ls, y=!!sym(v), color=hour)) +
      geom_point(size=3) +
      labs(title = paste(s, var_dict[v]),
           subtitle = paste0('Normalized Values ', min_year, '-', max_year),
           x = 'Q (L/s)',
           y = var_dict[v]) +
      facet_wrap(~ month_label, scales = 'free') +
      theme_light() +
      theme(text = element_text(size=24))
    #print(p)
    
    fname <- paste0('aggregated_years/', strsplit(v, "\\.")[[1]][1], '_intermonth_', s, '_norm.png')  # var_y_norm.png
    ggsave(fname, p, path=plot_dir, width=15, height=12)
  }
}

# Separated by Individual Years ----

## Process aggregated data ----
### Aggregate data ----
cq_hourly <- cq_df %>% 
  group_by(year, month, site, hour) %>% 
  summarise(across(where(is.numeric), ~ mean(.x, na.rm=TRUE)),  # Take mean of every column
            n = n(),  # Get counts of each group (site, year, hour)
            .groups='drop') %>%
  filter(n > 10)

### Normalize the data ----

# Function to normalize
normalize.data <- function(x, na.rm = TRUE){(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}

vars <- colnames(cq_hourly)[-c(1:4, 12)]  # variables to normalize

cq_hourly_norm <- cq_hourly %>%
  group_by(site) %>%
  mutate(across(vars, ~normalize.data(.x, ))) %>%  # apply normalize function to each variable
  ungroup()


## Plot ----

# Dictionaries for plot labels
title_dict <- c(
  "FRCH" = "A) French 33%",
  "MOOS" = "B) Moose 38%",
  "POKE" = "C) Poker 25%",
  "STRT"= "D) Stuart 31%",
  "VAUL" = "E) Vault 58%"
)

var_dict <- c(
  "SpCond.uScm.int" = "SPC (uS/cm)",
  "no3.uM.lab.int" = "NO3 (uM)",
  "fDOM.QSU.int" = "fDOM (QSU)",
  "Turb.FNU.int" = "Turbidity (FNU)",
  "abs254.int" = "abs254"
)

years <- unique(cq_hourly_norm['year'])
years <- as.vector(unlist(years))

### By month for all sites ----
# Intersite comparisons

months <- unique(cq_hourly_norm['month'])
months <- as.vector(unlist(months))

for(y in years){
  for(m in months){
    df <- cq_hourly_norm %>% 
      filter(month == m,
             year == y)
    
    if(nrow(df) == 0){  # Skip if no values for that month/year
      next
    }
    
    for(v in names(var_dict)){  # For each measurement
      p <- ggplot(df, aes(x=q_ls, y=!!sym(v), color=hour)) +
        geom_point(size=3) +
        labs(title = paste(month.name[m], y, var_dict[v]),
             subtitle = 'Normalized Values',
             x = 'Q (L/s)',
             y = var_dict[v]) +
        facet_wrap(~ site, scales = 'free', labeller = labeller(site=title_dict)) +
        theme_light() +
        theme(text = element_text(size=24))
      #print(p)
      
      fname <- paste0('stratified_yearly/', strsplit(v, "\\.")[[1]][1], '_', y , '_', m, '_norm.png')  # var_m_norm.png
      ggsave(fname, p, path=plot_dir, width=15, height=12)
    }
  }
}

### By site for all months ----
# Intermonth comparisons

sites <- unique(cq_hourly_norm['site'])
sites <- as.vector(unlist(sites))
for(y in years){
  for(s in sites){
    df <- cq_hourly_norm %>% 
      filter(site == s,
             year == y) %>%
      mutate(month_label = factor(month.name[month], levels = month.name[5:10]))
    
    for(v in names(var_dict)){  # For each measurement
      p <- ggplot(df, aes(x=q_ls, y=!!sym(v), color=hour)) +
        geom_point(size=3) +
        labs(title = paste(s, y, var_dict[v]),
             subtitle = 'Normalized Values',
             x = 'Q (L/s)',
             y = var_dict[v]) +
        facet_wrap(~ month_label, scales = 'free', drop=FALSE) +
        theme_light() +
        theme(text = element_text(size=24))
      #print(p)
      
      fname <- paste0('stratified_yearly/', strsplit(v, "\\.")[[1]][1], '_intermonth_', s, '_', y, '_norm.png')  # var_y_norm.png
      ggsave(fname, p, path=plot_dir, width=15, height=12)
    }
  }
}

