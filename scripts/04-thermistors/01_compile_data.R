## Purpose: Compile all thermistor data to single csv, plot the raw data, and identify any gaps in the data. 
## Inputs: Individual csv files of thermistor data for sites/years.
## Outputs: A single compiled thermistor datase, time series plots, a csv of missing data. 
## Notes: This does not download data from drive - you must manually upload any new thermistor data. 
## Author: Pat McCornack
## Date: (08/07/2025)

library(tidyverse)
library(here)

# Functions ----

# Read in and process given file
process_file <- function(filename) {
  name <- substr(filename, 1, 5)  # e.g. POKEH
  sitename <- substr(name, 1, 4)
  loc <- substr(name, 5, 5)
  
  col_names <- c('datetime', 'temp_5cm_f', 'temp_50cm_f')
  tz <- 'GMT - 8'  # True of all 2025 files, per datetime column name
  
  df <- read.csv(filename, skip=1) %>%
    select(2:4) %>%
    setNames(col_names) %>%
    mutate(site = sitename,
           location = loc,
           datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p'), tz="America/Anchorage",  # AKDT per datetime original column name
           temp_5cm_c = (temp_5cm_f - 32) * 5/9,
           temp_50cm_c = (temp_50cm_f - 32) * 5/9
    )
  
  return(df)
}


# Extract data ----
# Compile all files together
setwd(here('data/hobo_to_csv'))
file_list <- list.files(pattern='.csv', recursive = TRUE)
therm_df <- bind_rows(lapply(file_list, process_file))

therm_df <- therm_df %>% 
  mutate(
   temp_50cm_c = ifelse(temp_50cm_c >= 90, NA, temp_50cm_c)  # Replace values ≥ 90 with NA  - VAUL_H probe failed late 24
  ) %>%
  distinct(datetime, site, location, .keep_all=TRUE)  # Get rid of any duplicates
  
write.csv(therm_df, here('data/compiled_thermistor.csv'))


# Plotting ----
# Plot raw data to look for gaps
out_dir <- here('plots/thermistor_raw')

for(s in c("POKE", "VAUL", "FRCH", "MOOS", "STRT")){
  for(l in c("V", "H")){
    plot_df <- therm_df %>% filter(site == s,
                             location == l)
  
    p <- ggplot(plot_df) +
          geom_point(aes(x=datetime, y=temp_5cm_c), color='blue', size=1) +
          geom_point(aes(x=datetime, y=temp_50cm_c), color='orange', size=1) +
          labs(title=s) +
          theme_minimal()
    
    ggsave(plot=p,
           filename=paste0(s, '_', l, '_thermistor.png'),
           path=out_dir,
           width=14,
           height=8,
           bg='white'
           )
    
  }
}


# Find gaps in data ----
missing_periods <- therm_df %>%
  group_by(site, location) %>%
  arrange(datetime, .by_group = TRUE) %>%
  mutate(
    prev_time = lag(datetime),
    gap_days  = as.numeric(difftime(datetime, prev_time, units = "days"))
  ) %>%
  filter(!is.na(gap_days) & gap_days > 1) %>%
  transmute(
    site,
    location,
    gap_start = prev_time,   # now this won't be NA
    gap_end   = datetime,
    gap_days
  )

write.csv(missing_periods, here('data/missing_data.csv'))

