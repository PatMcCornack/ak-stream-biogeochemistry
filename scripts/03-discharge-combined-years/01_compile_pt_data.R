## Purpose: For each year, compile all pressure transducer data for that year to single file. Each years data was downloaded from the DoD Drive.
## Note that this is just a compiling script, QC edits are in a different script. Also joins atmospheric record to each.

## Inputs: Pressure transducer files.
## Outputs: One compiled PT file per year.
## Author: Pat McCornack
## Date: 08/04/25

library(tidyverse)
library(here)
library(zoo)
library(lubridate)
library(readxl)

## Functions ----

# Read and preprocess data - assumes default data format for hobo pressure transducers
process_data <- function(pt_vector) {
  col_names <- c('datetime', 'abs_pressure_kpa', 'temp_c')
  
  # Read in data
  df <- read.csv(pt_vector[[3]], skip=1) %>% 
    select(2:4) %>% 
    setNames(col_names) %>% 
    filter(!is.na(abs_pressure_kpa)) %>%
      mutate(datetime = as.POSIXct(parse_date_time(datetime, orders = c("mdy IMS p", "mdY HMS")), tz='UTC'),
             logger_sn = pt_vector[[2]],
             site=pt_vector[[1]])
  
  return(df)
}

# Process the precompiled data for each 2021 site
process_2021 <- function(pt_vector) {
  col_names <- c('datetime', 'abs_pressure_kpa', 'temp_c')
  df <- read.csv(pt_vector[[3]]) %>% 
    select(3,5) %>%
    setNames(c('abs_pressure_kpa', 'datetime')) %>%
    filter(!is.na(abs_pressure_kpa)) %>%
    mutate(site = pt_vector[[1]],
           logger_sn = pt_vector[[2]],
           temp_c = NA, 
           datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='UTC')) %>%
    group_by(datetime) %>%
    mutate(logger_sn = if_else(row_number() == 1, 1L, 2L)) %>%
    ungroup()
  
  return(df)
}

# Process the 2024 data
process_2024 <- function(fname){
  col_names <- c('datetime', 'abs_pressure_psi', 'temp_f')
  site_dict <- c('Eldo' = 'ELDO',
                 'French' = 'FRCH',
                 'Glen' = 'GLEN',
                 'Moose' = 'MOOS',
                 'Poker' = 'POKE',
                 'Vault' = 'VAUL')
  
  sn <- str_extract(fname, "\\d{8}")  # Get serial number
  sitename <- site_dict[[basename(dirname(fname))]]  # Get site name
  
  df <- read_excel(fname, skip=1) %>%
    select(2:4) %>%
    setNames(col_names) %>%
    mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='UTC'),
           logger_sn = sn,
           site=sitename)
  
  return(df)
}

## 2020 ----
# Pressure transducers
setwd(here('data/all_years/PT_2020/'))

pt_list <- list(
  c('FRCH', 20005935, 'FRCH/20005935_FRCH_stream_1.csv'),
  c('FRCH', 10710335, 'FRCH/10710335_FRCH_stream_2.csv'),
  c('MOOS', 10710340, 'MOOS/10710340_MOOS_stream_1.csv'),
  c('MOOS', 20452210, 'MOOS/20452210_MOOS_Stream_2.csv'),
  c('POKE', 10766894, 'POKE/10766894_POKE_stream_1.csv'),
  c('POKE', 20574424,'POKE/20574424_POKE_stream_2.csv'),
  c('STRT', 20075857, 'STRT/20075857_STRT_STREAM1.csv'),
  c('STRT', 20075856 ,'STRT/20075856_STRT_STREAM2.csv'),
  c('VAUL', 20574422 ,'VAUL/20574422_VAUL_stream_1.csv'),
  c('VAUL', 20574420 ,'VAUL/20574420_VAUL_stream_2.csv')
)

df_list <- lapply(pt_list, process_data)
pt_df <- bind_rows(df_list)
print(summary(pt_df))

# Atmospheric Data
# First check records: All are contiguous and look very similar. We'll use POKE
#  because it is the longest
file_list <- list.files(pattern='atmo.csv', recursive=TRUE)
col_names <- c('datetime', 'abs_pressure_kpa', 'temp_c')

for(file in file_list){
  atm_df <- read.csv(file, skip=1) %>%
    select(2:4) %>%
    setNames(col_names) %>%
    mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p'), tz='UTC')
  head(df)
  
  site <- strsplit(file, '/')[[1]][1]
  p <- ggplot(data=atm_df) +
        geom_point(aes(x=datetime, y=abs_pressure_kpa)) + 
        labs(title=site,
             x='Date',
             y='Atm Pressure') +
        theme_minimal()
  print(p)
}


# Now join POKE atmospheric to 2020 PT data.
atm_fname <- "POKE/20005936_POKE_atmo.csv"
atm_df <- read.csv(atm_fname, skip=1) %>%
  select(2:3) %>%
  setNames(c('datetime', 'atm_pressure_kpa')) %>%
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p', tz='UTC'))

join_df <- left_join(pt_df, atm_df, by='datetime')
join_df <- join_df %>% mutate(stream_pressure_kpa = abs_pressure_kpa - atm_pressure_kpa)

write.csv(join_df, 'PT_2020_compiled.csv')


## 2021 ----
setwd(here('data/all_years/PT_2021/'))

# These data are a mess, values seem to be mean of two PTs. Just took the SN from one transducer from each site.
pt_list <- list(
  c('FRCH', 20005935, 'FRCH/frch.pt.2021.csv'),  # Took SN from first PT
  c('MOOS', 10710340, 'MOOS/moos.pt.2021.csv'),
  c('POKE', 10766894, 'POKE/poke.pt.2021.csv'),
  c('STRT', 20075856, 'STRT/strt.pt.2021.csv'),
  c('VAUL', 20574421, 'VAUL/vaul.pt.2021.csv')
)

df_list <- lapply(pt_list, process_2021)
pt_df <- bind_rows(df_list)

print(summary(pt_df))


# Atmospheric data
# The VAUL record seems pretty good.
# Plot
file_list <- list.files(pattern='atmo.csv', recursive=TRUE)
atm_df <- read.csv(file_list[[2]], skip=1) %>%
  select(2:3) %>%
  setNames(c('datetime', 'atm_pressure_kpa')) %>%
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p'), tz='UTC')
p <- ggplot(data=atm_df) +
  geom_point(aes(x=datetime, y=atm_pressure_kpa)) + 
  labs(title='VAUL',
       x='Date',
       y='Atm Pressure') +
  theme_minimal()
print(p)

# Read in and join
atm_fname <- file_list[[2]]
atm_df <- read.csv(atm_fname, skip=1) %>%
  select(2:3) %>%
  setNames(c('datetime', 'atm_pressure_kpa')) %>%
  mutate(datetime = as.POSIXct(datetime, format='%m/%d/%y %I:%M:%S %p'), tz='UTC')

# Have to expand to 5 minute interval and interpolate 
five_min_intervals <- seq(
  from = min(atm_df$datetime),
  to = max(atm_df$datetime),
  by = "5 min"
)

df_5min <- data.frame(datetime = five_min_intervals)

atm_df <- df_5min %>%
  left_join(atm_df, by = "datetime") %>%
  arrange(datetime) %>%
  mutate(
    atm_pressure_kpa = na.approx(atm_pressure_kpa, x = datetime, na.rm = FALSE),
  )

# Join PT to ATM data
join_df <- left_join(pt_df, atm_df, by='datetime')
join_df <- join_df %>% mutate(stream_pressure_kpa = abs_pressure_kpa - atm_pressure_kpa)

write.csv(join_df, 'PT_2021_compiled.csv')


## 2022 ----
setwd(here('data/all_years/PT_2022/'))

# List of pressure transducers
pt_list <- list(
  c('FRCH', 20005935, 'FRCH/FRCH_220930merged_stream1.csv'),
  c('FRCH', 20075857, 'FRCH/FRCH_220930_merged_stream2.csv'),  # Labeled STRT in metadata, but seems to be ok
  c('MOOS', 10710340, 'MOOS/MOOS_221013_merged_stream1.csv'),
  c('MOOS', 20452210, 'MOOS/MOOS_221013_merged_stream2.csv'),
  c('POKE', 20574425, 'POKE/POKE_220930_merged_stream1.csv'),
  c('POKE', 10766894, 'POKE/POKE_220930_merged_stream2.csv'),
  c('STRT', 20005934, 'STRT/STRT_WL_220902.csv'),
  c('STRT', 20075856, 'STRT/STRT_WR_220902.csv'),
  c('VAUL', 20574421, 'VAUL/VAUL_221013_merged_stream2.csv')  # The other transducer as incomplete record starting in August
)

# Join all data together
df_list <- lapply(pt_list, process_data)
pt_df <- bind_rows(df_list)

print(summary(pt_df))  # Quick check


# Atmospheric data
# None of the baro PTs are very good... Check the NEON data
neon_baro <- read.csv('ATMO/NEON_bp_2022.csv') %>%
  select(6,8) %>%
  setNames(c('datetime', 'atm_pressure_kpa')) %>%
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S'), tz='UTC')

ggplot(data=neon_baro) + geom_point(aes(x=datetime, y=atm_pressure_kpa)) +
  theme_minimal()

# Some gaps - need to fill these
# Have to expand to 5 minute interval and interpolate 
one_min_intervals <- seq(
  from = min(neon_baro$datetime),
  to = max(neon_baro$datetime),
  by = "1 min"
)

df_1min <- data.frame(datetime = one_min_intervals)

neon_baro <- df_1min %>%
  left_join(neon_baro, by = "datetime") %>%
  arrange(datetime) %>%
  mutate(
    atm_pressure_kpa = na.approx(atm_pressure_kpa, x = datetime, na.rm = FALSE),
  )

ggplot(data=neon_baro) + geom_point(aes(x=datetime, y=atm_pressure_kpa)) +
  theme_minimal()

# Join to PT data
join_df <- left_join(pt_df, neon_baro, by='datetime')
join_df <- join_df %>% mutate(stream_pressure_kpa = abs_pressure_kpa - atm_pressure_kpa)
write.csv(join_df, 'PT_2022_compiled.csv')


## 2024 ----
setwd(here("data/2024/PT"))
baro_data_fpath <- here('data/2024/PT/neon_atm.csv')
pt_fpath <- 'PT_raw_2024.csv'

## Read in transducer data
# Get list of raw files
file_list <- list.files(  
               pattern = "^\\d{8}.*\\.xlsx$", 
               full.names = TRUE,
               recursive=TRUE)

# remove barometer files
file_list <- file_list[!grepl("AIR", file_list)]

# Process PT data
pt_df <- bind_rows(lapply(file_list, process_2024))

## Read in neon atm data
baro_df <- read.csv(baro_data_fpath) %>%
  select(2:3) %>%
  mutate(datetime = as.POSIXct(datetime, '%Y-%m-%d %H:%M:%S', tz='UTC'))

# Expand to 15 minutes
df_15min <- data.frame(
  datetime = seq(min(baro_df$datetime, na.rm=TRUE), max(baro_df$datetime, na.rm=TRUE), by = "15 min")
)

baro_df <- df_15min %>%
  left_join(baro_df, by = "datetime") %>%
  arrange(datetime) %>%
  mutate(atm_pressure_kpa = approx(datetime, atm_pressure_kpa, xout = datetime, rule = 1)$y)

# join PT to atm data
setwd(here("data/2024/PT")) 
join_df <- left_join(pt_df, baro_df, by='datetime')

# Imperial to metric
join_df <- join_df %>% 
  mutate(temp_c = (temp_f - 32) * 5 / 9,
         abs_pressure_kpa = abs_pressure_psi * 6.89476) %>%
  select(-temp_f, -abs_pressure_psi)
  

# Write out joined data
write.csv(join_df, here('data/all_years/PT_2024/PT_2024_compiled.csv'))


## 2025 ----
# Convert imperial to metric
setwd(here('data/all_years'))
fpath <- 'PT_2025/PT_2025.csv'
df <- read.csv(fpath)

df <- df %>% 
  mutate(temp_c = (temp_f - 32) * 5 / 9,
         abs_pressure_kpa = abs_pressure_psi * 6.89476,
         atm_pressure_kpa = atm_pressure_psi * 6.89476,
         stream_pressure_kpa = stream_pressure_psi * 6.89476) %>%
  select(-abs_pressure_psi, -atm_pressure_psi, -stream_pressure_psi, -temp_f)

write.csv(df, 'PT_2025/PT_2025_compiled.csv')


## Aggregate years ----
setwd(here('data/all_years'))
file_list <- list.files(pattern='compiled.csv', recursive=TRUE)

df_list <- lapply(file_list, read.csv)

df <- bind_rows(df_list) %>% 
  select(-1) %>%
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S'), tz='UTC') %>%
  select(-tz)

# Quick check for qc issues
summary(df) # Couple of problems - will need to return to this

# Save out
write.csv(df, 'PT_data_raw.csv')


## Plot ----
plot_dir <- here('Plots/aggregated_years/PT_raw/')

years <- c(2020, 2021, 2022, 2024, 2025)
sites <- c('FRCH', 'MOOS', 'ELDO', 'VAUL', 'POKE')

for(s in sites) {
  for(y in years) {
    plot_df <- df %>% filter(site == s, year(datetime) == y) %>%
      mutate(logger_sn = ifelse(is.na(logger_sn), 'No SN', logger_sn),
             logger_sn = as.factor(logger_sn))
    if(nrow(plot_df) == 0) next  # skip if empty
    
    # Extract first and last points
    start_time <- plot_df %>% slice(1) %>% pull(datetime)
    end_time <- plot_df %>% slice_tail(n=1) %>% pull(datetime)
    
    p <- ggplot(data = plot_df) + 
      geom_point(aes(x=datetime, y=abs_pressure_kpa, color=logger_sn), size=1) +
      geom_point(aes(x=datetime, y=atm_pressure_kpa), color='black', size=1) +
      scale_color_manual(name='Serial Number',
        values = c('blue', 'orange', 'red')) +
      labs(title=paste(s, y),
           subtitle=paste(start_time, '-', end_time),
           x='Date', 
           y='Absolute Pressure (kpa)') +
      theme_minimal()
    
    ggsave(plot=p, 
           filename=paste0(y, '_', s, '.jpg'),
           path=plot_dir,
           width = 8, height = 5)
  }
}





