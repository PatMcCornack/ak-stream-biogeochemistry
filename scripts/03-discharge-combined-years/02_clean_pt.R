## Purpose: Clean the compiled pressure transducer data. 
## Inputs: Compiled PT data
## Outputs: Cleaned compiled PT data and plots 
## Author: Pat McCornack
## Date: 08/05/2025

# TO DO: Many of the datasets seem to be misaligned with the atmospheric data, but not all. The cause of this
#        is likely either a timezone problem or the PT/shuttle time was not synced when launched/gathered. Need to: 
#        1. Check timezone of atmospheric dataset
#        2. Misalignments are corrected using lag(). This method assumes all data are spaced 5 minutes apart.
#           Need to add in NA rows for year/site combinations to ensure consistent 5 minute spacing - i.e.
#           gaps in the data resulting in jumps of >5 minutes will screw up the correction. 
#        - PM, 08/07/25


library(tidyverse)
library(here)
library(lubridate)
library(pracma)
library(zoo)


## Functions ----
plot_pt <- function(df, s, y){
  plot_df <- df %>% filter(site == s, year(datetime) == y) %>%
    mutate(logger_sn = ifelse(is.na(logger_sn), 'No SN', as.character(logger_sn)))
  
  # Extract first and last points
  start_time <- plot_df %>% slice(1) %>% pull(datetime)
  end_time <- plot_df %>% slice_tail(n=1) %>% pull(datetime)
  
  p <- ggplot(data = plot_df) + 
    geom_point(aes(x=datetime, y=abs_pressure_kpa, color=logger_sn), size=1) +
    geom_point(aes(x=datetime, y=atm_pressure_kpa), color='black', size=1) +
    scale_color_manual(name='Serial Number',
                       values = c('blue', 'orange')) +
    labs(title=paste(s, y),
         subtitle=paste(start_time, '-', end_time),
         x='Date', 
         y='Absolute Pressure (kpa)') +
    theme_minimal()
  print(p)
}

correct_jump <- function(df, threshold = 5) {
  df <- df %>% arrange(logger_sn, datetime)  # Ensure proper ordering
  
  corrected_list <- list()
  
  for (sn in unique(df$logger_sn)) {
    df_sn <- df %>% filter(logger_sn == sn)
    
    # Compute pressure difference
    df_sn <- df_sn %>%
      mutate(pressure_diff = abs_pressure_kpa - lag(abs_pressure_kpa),
             jump = abs(pressure_diff) > threshold)
    
    jump_indices <- which(df_sn$jump)
    
    # Track cumulative shift to apply progressively
    cumulative_shift <- 0
    
    for (i in jump_indices) {
      shift_value <- df_sn$abs_pressure_kpa[i] - df_sn$abs_pressure_kpa[i - 1]
      cumulative_shift <- cumulative_shift + shift_value
      
      # Shift all previous values up/down
      df_sn$abs_pressure_kpa[1:(i - 1)] <- df_sn$abs_pressure_kpa[1:(i - 1)] + shift_value
    }
    
    corrected_list[[as.character(sn)]] <- df_sn
  }
  
  corrected_df <- bind_rows(corrected_list)
  corrected_df <- corrected_df %>% select(-pressure_diff, -jump)
  
  return(corrected_df)
}


## Load in Data ----
setwd(here())
pt_fpath <- 'data/all_years/PT_data_raw.csv'
pt_df <- read.csv(pt_fpath) %>%
  select(-1) %>%
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S'),
         logger_sn = as.character(logger_sn),
         year = year(datetime))  # Used to split data
head(pt_df)


## Prepare to process in chunks ----
# Create list of site/year dataframes
df_list <- pt_df %>%
  group_by(site, year) %>%
  group_split()

# Label them
df_keys <- pt_df %>%
  group_by(site, year) %>%
  group_keys()

names(df_list) <- paste(df_keys$site, df_keys$year, sep = "_")


## Process each chunk ----

### MOOS ----

#### 2020 -----
site_year <- 'MOOS_2020'

df_list[[site_year]] <- df_list[[site_year]] %>% 
  filter(!is.na(abs_pressure_kpa),
         logger_sn == '20452210')

# Clean some stray points
filter_times <- list(  # start -> end
  c('2020-08-17 13:15:00', '2020-08-17 14:45:00')
)

for (sublist in filter_times) {
  start_time <- as.POSIXct(sublist[1], format='%Y-%m-%d %H:%M:%S')
  end_time <- as.POSIXct(sublist[2], format='%Y-%m-%d %H:%M:%S')
  df_list[[site_year]] <- df_list[[site_year]] %>% filter(logger_sn != '10710340' |
                                                            !(datetime >= start_time & datetime <= end_time))
}

df_list[[site_year]] <- df_list[[site_year]] %>% filter(atm_pressure_kpa> 95)
df_list[[site_year]] <- correct_jump(df_list[[site_year]], threshold=1)  # correct shift from cleaning

plot_pt(df_list[[site_year]], 'MOOS', 2020)

    
#### 2021 -----
site_year <- 'MOOS_2021'

# Get rid of NA values
df_list[[site_year]] <- df_list[[site_year]] %>% filter(!is.na(stream_pressure_kpa))

# Remove data from before July - it's clearly bad 
df_list[[site_year]] <- df_list[[site_year]] %>% filter(month(datetime) > 7)

# Second PT sometimes dips below atm pressure, and doesn't add any value - drop it
df_list[[site_year]] <- df_list[[site_year]] %>% filter(logger_sn == 1)

plot_pt(df_list[[site_year]], 'MOOS', 2021)


#### 2022 -----
# This one is a mess...
site_year <- 'MOOS_2022'

df_list[[site_year]] <- df_list[[site_year]] %>% 
  filter(datetime > '2022-05-24 14:00:00')
  
# Drop most of second transducer - except for part that lines up with 1st
start_time <- '2022-08-31 14:07:56'
end_time <- '2022-09-28 13:57:56'
df_list[[site_year]] <- df_list[[site_year]] %>%
  filter(logger_sn != 20452210, 
         abs_pressure_kpa > 100) %>% # Remove errant points
  mutate(abs_pressure_kpa = lag(abs_pressure_kpa, n=180))  # Aligning timezones


df_list[[site_year]] <- correct_jump(df_list[[site_year]], threshold=1)  # correct shift from cleaning

plot_pt(df_list[[site_year]], 'MOOS', 2022)


#### 2024 -----
site_year <- 'MOOS_2024'
df_list[[site_year]] <- df_list[[site_year]] %>% filter(datetime > '2024-05-14 10:25:00')  # points before are out of water based on df_list[[site_year]]

df_list[[site_year]] <- df_list[[site_year]] %>% 
  mutate(atm_pressure_kpa = atm_pressure_kpa - 5) %>% # Adjust so that atm is lower than abs
  filter(logger_sn == 20574421,  # Cleaner dataset
         datetime > '2024-05-31 11:35:00')  # out of water points 

plot_pt(df_list[[site_year]], 'MOOS', 2024)

#### 2025 -----
site_year <- 'MOOS_2025'
temp <- df_list[[site_year]] %>% 
  filter(datetime > '2025-05-15 18:40:00',
         datetime != '2025-06-24 10:40:00',
         datetime != '2025-06-24 10:35:00', # out of water
         datetime != '2025-06-24 10:30:00',  # errant point
         logger_sn == 20574421) # No jump in data 

temp <- correct_jump(temp, threshold=0.5)  # correct shift from cleaning


plot_pt(temp, 'MOOS', 2025)


### FRCH ----

#### 2020 ---- 
site_year <- 'FRCH_2020'
df_list[[site_year]] <- df_list[[site_year]] %>% 
  filter(!is.na(atm_pressure_kpa),
         logger_sn != 20005935, # Really messy, isn't needed
         datetime > '2020-06-11 13:15:00') %>% # out of water
  mutate(atm_pressure_kpa = ifelse(datetime == '2020-10-14 17:45:00',
                                   NA_real_, atm_pressure_kpa))  # remove errant point

plot_pt(df_list[[site_year]], 'FRCH', 2020)

#### 2021 ---- 
site_year <- 'FRCH_2021'
df_list[[site_year]] <- df_list[[site_year]] %>%
  filter(logger_sn != 1,  # 2 is a better record, 1 isn't needed 
         abs_pressure_kpa > 100, # out of water points 
         datetime < '2021-09-26 20:55:00')  %>% # data begins to oscillate in last two hours 
  mutate(abs_pressure_kpa = lag(abs_pressure_kpa, n=180))  # aligning timezones


plot_pt(df_list[[site_year]], 'FRCH', 2021)


#### 2022 ---- 
# This one has gaps in both datasets - but the gaps don't align. We'll create
# a single curve by splicing them together and correcting the offset. They follow
# the exact same pattern throughout, so this should be ok.

# Doesn't quite align with the atm data - shifting by 9 hours to align timezones
# fixes the latter half of the data, but misaligns the first half - did they use
# different timezones?

site_year <- 'FRCH_2022'

# get datetime of second logger date
sn_start <- df_list[[site_year]] %>% 
  filter(logger_sn ==  20075857) %>%
  slice(1) %>%
  pull(datetime)

df_list[[site_year]] <- df_list[[site_year]] %>% 
  filter((logger_sn == 20005935 & datetime < sn_start) |
         (logger_sn == 20075857 & datetime >= sn_start),  # Splice the curves together
         datetime != '2022-06-07 11:10:00',
         datetime > '2022-05-11 13:45:00',
         datetime < '2022-10-10 10:40:00') %>% # out of water points
  mutate(logger_sn = as.character(1))  # Dummy number

df_list[[site_year]] <- correct_jump(df_list[[site_year]], threshold=1)  # correct shift from cleaning

plot_pt(df_list[[site_year]], 'FRCH', 2022)


#### 2024 ---- 
# Unsure why 6 hour shift - time on sensor was wrong? May need more 
# fine tuning... Is there a programatic way? 
site_year <- 'FRCH_2024'
df_list[[site_year]] <- df_list[[site_year]] %>%
  filter(abs_pressure_kpa > atm_pressure_kpa,
         month(datetime) > 5,  # data from before june is clearly bad
         datetime != '2024-10-09 06:00:00',# Out of water
         logger_sn == 20005935)
df_list[[site_year]] <- correct_jump(df_list[[site_year]], threshold=1.5)  # correct shift from cleaning

plot_pt(df_list[[site_year]], 'FRCH', 2024)



#### 2025 ---- 
site_year <- 'FRCH_2025'
temp <- df_list[[site_year]] %>%
  filter(logger_sn == 20574422)  # no jump in data
plot_pt(temp, 'FRCH', 2025)

### ELDO ----

#### 2024 ----
# Unsure about hour shift here.. Seems better though

site_year <- 'ELDO_2024'
df_list[[site_year]] <- df_list[[site_year]] %>%
  mutate(atm_pressure_kpa = atm_pressure_kpa - 1) %>%
  filter(abs_pressure_kpa > atm_pressure_kpa,
         logger_sn == 21984141)

plot_pt(df_list[[site_year]], 'ELDO', 2024)

#### 2025 ----
# no changes
site_year <- 'ELDO_2025'
temp <- df_list[[site_year]] %>%
  filter(logger_sn == 21984143)  %>% # No jump in data
  mutate(abs_pressure_kpa = ifelse(datetime == '2025-06-25 12:40:00',
                                   NA_real_, abs_pressure_kpa))  # remove errant point

plot_pt(temp, 'ELDO', 2025)
 

### VAUL ----
#### 2020 ----
site_year <- 'VAUL_2020'
df_list[[site_year]] <- df_list[[site_year]] %>%
  filter(logger_sn != 20574422,   # Clearly bad
         datetime < '2020-10-14 12:30:00')  # Clearly bad
plot_pt(df_list[[site_year]], 'VAUL', 2020)

#### 2021 ----
site_year <- 'VAUL_2021'
df_list[[site_year]] <- df_list[[site_year]] %>% 
  filter(datetime > '2021-06-30', # Flat line before this 
         datetime < '2021-09-27 07:50:00')  %>%
  mutate(abs_pressure_kpa = lag(abs_pressure_kpa, n=180))
plot_pt(df_list[[site_year]], 'VAUL', 2021)

#### 2022 ----
site_year <- 'VAUL_2022'

df_list[[site_year]] <- df_list[[site_year]]
df_list[[site_year]] <- df_list[[site_year]] %>% mutate(abs_pressure_kpa = lag(abs_pressure_kpa, n=220)) %>%  # seems to be shifted by 9 hours - UTC? 
  filter(datetime > '2022-07-01',  # First half of dataset is very suspect
         datetime < '2022-10-11 13:55:00')  # Out of water points based on df_list[[site_year]]erature
plot_pt(df_list[[site_year]], 'VAUL', 2022)

#### 2024 ----
site_year <- 'VAUL_2024'
df_list[[site_year]] <- df_list[[site_year]] %>% 
  mutate(atm_pressure_kpa = atm_pressure_kpa - 5) %>%  # Slight adjust to maintain abs above 0
  filter(logger_sn == 21984144,
         abs_pressure_kpa > atm_pressure_kpa,
         datetime > '2024-06-19 13:30:00',
         datetime < '2024-10-10 19:15:00') # Out of water based on df_list[[site_year]]erature

df_list[[site_year]] <- correct_jump(df_list[[site_year]], threshold=1)  # correct shift from cleaning
plot_pt(df_list[[site_year]], 'VAUL', 2024)

#### 2025 ----
site_year <- 'VAUL_2025'
df_list[[site_year]] <- df_list[[site_year]] %>%
  filter(logger_sn == 21984144,  # Better PT
         datetime < '2025-06-11 10:40:00' | datetime > '2025-06-11 10:50:00')  
plot_pt(df_list[[site_year]], 'VAUL', 2025)


### POKE ----
#### 2020 ----
site_year <- 'POKE_2020'
df_list[[site_year]] <- df_list[[site_year]] %>% 
  filter(datetime < '2020-10-14 07:45:00',
         logger_sn == 10766894)  # out of water

df_list[[site_year]] <- correct_jump(df_list[[site_year]], threshold=.5)  # correct shift from cleaning

plot_pt(df_list[[site_year]], 'POKE', 2020)

#### 2021 ----
# It's not great, but unsure of how to fix further
site_year <- 'POKE_2021'
df_list[[site_year]] <- df_list[[site_year]] %>%
  filter(datetime < '2021-09-29 05:15:00',  # Very high values (frozen in?)
         logger_sn == 2, # Somewhat cleaner than first datalogger
         !is.na(atm_pressure_kpa),
         abs_pressure_kpa > 100)  %>% # Errant point
  mutate(abs_pressure_kpa = ifelse(datetime > '2021-06-30',
                                   lag(abs_pressure_kpa, n=160),  # time zone correction
                                   lag(abs_pressure_kpa, n=60))) %>%   # PT time had drifted? 
  filter(!(datetime > '2021-06-23' & datetime < '2021-07-01'))  # straight line, not real


plot_pt(df_list[[site_year]], 'POKE', 2021)


#### 2022 ----
site_year <- 'POKE_2022'
df_list[[site_year]] <- df_list[[site_year]] %>%
  filter(!(logger_sn == 20574425 & month(datetime) > 7),
         datetime < '2022-10-07 10:00:00',
         datetime > '2022-05-17 16:40:00')  %>%  # out of water
  mutate(abs_pressure_kpa = lag(abs_pressure_kpa, n=180))
df_list[[site_year]] <- correct_jump(df_list[[site_year]], threshold=1)  # correct shift from cleaning
plot_pt(df_list[[site_year]], 'POKE', 2022) 


#### 2024 ----
site_year <- 'POKE_2024'
plot_pt(df_list[[site_year]], 'POKE', 2024) 

# This isn't salvageable - I think the PTs were out of water most of the time
df_list[['POKE_2024']] <- NULL

#### 2025 ----
site_year <- 'POKE_2025'
df_list[[site_year]] <- df_list[[site_year]] %>%
  filter(logger_sn == 20574425)
df_list[[site_year]] <- correct_jump(df_list[[site_year]], threshold=0.5)  # correct shift from cleaning
plot_pt(df_list[[site_year]], 'POKE', 2025) 


## Rejoin all data and save out ----
df_list[grepl("_NA|GLEN|STRT", names(df_list))] <- list(NULL)
df_list <- df_list[!sapply(df_list, is.null)]  # remove entires

df_clean <- do.call(rbind, df_list) %>%
  select(-year) %>%
  mutate(stream_pressure_kpa = abs_pressure_kpa - atm_pressure_kpa)
write.csv(df_clean, here('data/all_years/PT_clean.csv'))


## Generate plots of clean data
## Plot ----
plot_dir <- here('Plots/aggregated_years/PT_clean/')

years <- c(2020, 2021, 2022, 2024, 2025)
sites <- c('FRCH', 'MOOS', 'ELDO', 'VAUL', 'POKE')

for(s in sites) {
  for(y in years) {
    plot_df <- df_clean %>% filter(site == s, year(datetime) == y) %>%
      mutate(logger_sn = ifelse(is.na(logger_sn), 'No SN', logger_sn),
             logger_sn = as.character(logger_sn))
    if(nrow(plot_df) == 0) next  # skip if empty
    
    # Extract first and last points
    start_time <- plot_df %>% slice(1) %>% pull(datetime)
    end_time <- plot_df %>% slice_tail(n=1) %>% pull(datetime)
    
    p <- ggplot(data = plot_df) + 
      geom_point(aes(x=datetime, y=abs_pressure_kpa, color=logger_sn), size=1) +
      geom_point(aes(x=datetime, y=atm_pressure_kpa), color='black', size=1) +
      scale_color_manual(name='Serial Number',
                         values = c('blue', 'orange')) +
      labs(title=paste(s, y),
           subtitle=paste(start_time, '-', end_time),
           x='Date', 
           y='Absolute Pressure (kpa)') +
      theme_minimal()
    
    ggsave(plot=p, 
           filename=paste0(s, '_', y, '.jpg'),
           path=plot_dir,
           width = 14, height = 8)
  }
}




