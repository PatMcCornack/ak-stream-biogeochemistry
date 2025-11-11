## Purpose: Process pressure transducer data
## Inputs: Pressure transducer data
## Outputs: Plots of raw pressure and stream pressure head + aggregated PT data. 

## NOTE: To access CPCRW CRREL Met data (as of 09-09-2025) see: https://drive.google.com/file/d/1cHFpm2_SLKwGniP8ZaK-U5lnOBSjgbu7/view?usp=sharing

library(tidyverse)
library(lubridate)
library(here)
library(googledrive)
library(data.table)
library(readr)
library(zoo)
library(ggplot2)

### EDIT HERE ### ----
# Filepaths are relative to RProj
ptURL <- "https://drive.google.com/drive/u/0/folders/1maL_VFLaZhWIwyfQOYb1l6imAZtCMAos"  # Drive link to pressure transducer data

pt_data_dir <- "data/2025/pressure_transducer/individual_files"  # Where to save PT files


pt_plots_outdir <- "plots/2025/pressure_transducer"  # Where to save raw PT plots
pt_out_fpath <- 'data/2025/pressure_transducer/PT_2025_raw.csv' # Where to save aggregated PT data
pt_processed_out_fpath <- 'data/2025/pressure_transducer/PT_2025_processed.csv'

### Functions ----

# Extract data from sensor .csv files and format the data
process_data <- function(fpath) {
  # Get Serial Number/Site
  lines <- readLines(fpath)
  line <- str_split(lines[1], ':', simplify=TRUE)[1,2]
  sn <- str_trim(str_split(line, '_', simplify=TRUE)[1,1])
  site <- str_trim(str_split(line, '_', simplify=TRUE)[1,2])
  
  # Read in data
  df <- suppressWarnings(read_csv(fpath, skip=1, col_select=-1, show_col_types=FALSE))  # Warning comes from mixed data types - is ok
  df <- df %>%
    slice(1:(n()-2)) %>%  # Last two rows aren't pressure/temp data
    select(names(df[1:3]))
  names(df) <- c('datetime', 'abs_pressure_psi', 'temp_f')

  # Format data
  df$datetime <- as.POSIXct(df$datetime, format = "%m/%d/%y %I:%M:%S %p", tz = "America/Anchorage")  # datetime recorded as GMT-8
  df$datetime <- lubridate::round_date(df$datetime, "5 minutes")
  df$abs_pressure_psi <- as.numeric(df$abs_pressure_psi)
  df$temp_f <- as.numeric(df$temp_f)
  df$logger_sn <- sn
  df$site <- site
  
  return(as.data.frame(df))
}

# Plots absolute pressure
plot_abs_pressure <- function(pt_df, baro_df, s){
  
  site_df <- pt_df %>% filter(site == s)
  
  site_qc_pl <- ggplot() +
    geom_point(data=site_df, aes(x = datetime, y = abs_pressure_psi, color = logger_sn), size=.5) +
    geom_point(data=baro_df, aes(x = datetime, y = atm_pressure_psi), color = 'black', size=.5) +
    theme_minimal() +
    ggtitle(paste("Site:", s)) +
    labs(subtitle='Black = Atmospheric',
         x = 'Date',
         y = 'Absolute Pressure PSI')
  plot(site_qc_pl)
  
  ggsave(filename = paste0(s, "_absolute_corrected.png"),
         plot = site_qc_pl,
         path = pt_plots_outdir,
         width = 8, height = 5,
         bg='white')
}

# Correct errant points caused when taking PT out of water to read. Uses
#   a threshold in difference between last and next point.
correct_points <- function(pt_df, threshold=0.025){
  
  pt_df <- pt_df %>% 
    arrange(datetime) %>%
    group_by(logger_sn) %>%
    mutate(
      diff_val = abs_pressure_psi - lag(abs_pressure_psi),
      diff_next = lead(abs_pressure_psi) - abs_pressure_psi
    ) %>%
    filter(
      abs(diff_val) < threshold,       # adjust threshold as needed
      abs(diff_next) < threshold
    ) %>%
    #select(-diff_val, -diff_next) %>%
    ungroup()
  
  return(pt_df)
}

# Identify where brushing likely happened using a before/after pressure
#   difference threshold
identify_jumps <- function(df, threshold = .2) {
  df <- df %>% 
    arrange(datetime) %>% 
    group_by(logger_sn) %>%
    mutate(difference = abs_pressure_psi - lag(abs_pressure_psi)) %>%
    filter(abs(difference) > threshold) %>%
    ungroup()
  
  
  return(df)
}

# Shift sections of the data to correct for PT well brushing
shift_section <- function(df, start, end, shift, sn){
  start <- ymd_hms(start, tz='America/Anchorage')
  end <- ymd_hms(end, tz='America/Anchorage')
  
  df <- df %>%
    mutate(abs_pressure_psi = if_else(
      datetime >= start & datetime < end & logger_sn == sn,
      abs_pressure_psi + shift,
      abs_pressure_psi
    ))
  
  return(df)
  
}

### Download Data ----

# Get list of Drive files
pt_new <- drive_get(as_id(ptURL))
pt_glist <- drive_ls(pt_new, pattern='.csv', recursive=TRUE)

# Download files
setwd(here(pt_data_dir))
walk(pt_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))


### Process Data ----
setwd(here(pt_data_dir))

file_list_full <- list.files(recursive = F, 
                             pattern = ".csv", 
                             full.names = TRUE) 

# Separate out barometer and conductivity files
crrel_file <- file_list_full[str_detect(file_list_full, "(?i)crrel")]
file_list <- file_list_full[!str_detect(file_list_full, "(?i)baro|(?i)cond|(?i)crrel")]

# Read and join files
pt_df <- bind_rows(lapply(file_list, process_data)) %>% 
  mutate(site = toupper(site))

# Read atmospheric pressure data
baro_df <- read.csv(crrel_file, skip=5) %>%
  select(1, 6) %>%
  setnames(c('datetime', 'atm_pressure_mbar')) %>%
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='America/Anchorage'),
         atm_pressure_psi = atm_pressure_mbar * 0.0145038)

setwd(here())

# Interpolate atm to 5 minute intervals
baro_df <- baro_df %>%
  select(datetime, atm_pressure_psi) %>%
  complete(datetime = seq(min(datetime), max(datetime), by='5 min')) %>%  # Expand to 5 minute intervals
  mutate(atm_pressure_psi = na.approx(atm_pressure_psi, x=datetime, na.rm=FALSE))  # Interpolate between hours

# Join data and calculate relative pressure
pt_df <- pt_df %>% 
  left_join(baro_df %>% select(datetime, atm_pressure_psi), by='datetime') %>%
  mutate(relative_pressure_psi = (abs_pressure_psi - atm_pressure_psi))

# Save out joined dataset
pt_df_out <- pt_df %>% mutate(datetime = as.character(datetime))  # save as string to reduce headaches later
write_csv(pt_df_out, pt_out_fpath)


## Plot raw pressures ----
setwd(here())
sites <- unique(pt_df$site)
walk(sites, function(s) {
  site_df <- pt_df %>% filter(site == s)
  site_qc_pl <- ggplot() +
    geom_point(data=site_df, aes(x = datetime, y = abs_pressure_psi, color = logger_sn), size=.5) +
    geom_point(data=baro_df, aes(x = datetime, y = atm_pressure_psi), color = 'black', size=.5) +
    theme_minimal() +
    ggtitle(paste("Site:", s)) +
    labs(subtitle='Black = Atmospheric',
         x = 'Date',
         y = 'Absolute Pressure PSI')
  plot(site_qc_pl)
  
  print(paste("Saving raw PT plots to:", pt_plots_outdir))
  ggsave(filename = paste0(s, "_absolute_raw.png"),
         plot = site_qc_pl,
         path = pt_plots_outdir,
         width = 8, height = 5,
         bg='white')
})


# Correct data ----

## Point correction - out of water points ----
# This section removes errant points in the data where the pressure transducer
#   was out of water while the data was being offloaded.
pt_df_corrected <- correct_points(pt_df, threshold=0.03) %>%
  filter(!(site=='VAUL' & as.Date(datetime) == '2025-06-11' & abs_pressure_psi < 14.5)) %>%  # Manual correction
  select(-diff_val, -diff_next)  # Drop utility columns

## Jump correction - PT well cleanings ----
# This section shifts sections of the data to account for brushing out 
#   the PT wells. Shifts up in the raw plots indicate cleaning (PT sits deeper).
#   Shifts down indicate that silt settled while the PT was pulled out, causing 
#   the PT to sit more shallow. 

jump_df <- identify_jumps(pt_df_corrected, threshold = .1)

# List of sections to correct: start, end, shift, sn
parameters <- list(  
  # ELDO
  c('2025-05-16 20:00:00', '2025-05-28 12:25:00', 0.4650, 21984141),
  c('2025-08-14 13:25:00', '2025-09-03 13:35:00', 0.4171, 21984141),
  
  # FRCH
  c('2025-06-24 12:55:00', '2025-07-28 10:40:00', 0.4119, 20005934),
  c('2025-08-20 11:20:00', '2025-09-15 15:25:00', 0.2737, 20574422),
  
  # MOOS
  c('2025-06-18 10:40:00', '2025-07-28 11:55:00', 0.2897, 20452210),
  c('2025-09-02 11:40:00', '2025-09-15 12:45:00', 0.6296, 20452210),
  c('2025-09-02 11:45:00', '2025-09-15 12:45:00', 0.4369, 20574421),
  
  # POKE
  c('2025-05-13 17:00:00', '2025-06-10 10:35:00', 0.3876, 10766894),
  
  # VAUL
  c('2025-05-20 21:05:00', '2025-06-25 09:45:00', 0.1541, 21984142),
  c('2025-07-22 14:55:00', '2025-08-31 00:00:00', 0.1198, 21984142),
  c('2025-08-14 10:35:00', '2025-08-31 00:00:00', 0.1033, 21984142)
)

for(p in parameters){ # Shift each section
  pt_df_corrected <- shift_section(pt_df_corrected, p[1], p[2], as.double(p[3]), as.integer(p[4]))
}


# Trim dataset ----
# Trim out sections of bad data + out of water

# VAUL logger pulled out of water by high flows here - other PT was lost
start <- ymd_hms('2025-08-25 00:00:00', tz='America/Anchorage')
end <- ymd_hms('2025-09-02 16:00:00', tz='America/Anchorage')
sn <- 21984142

pt_df_corrected <- pt_df_corrected %>% 
  filter(!(logger_sn == sn & datetime >= start & datetime < end)) %>%
  mutate(relative_pressure_psi = abs_pressure_psi - atm_pressure_psi)  # recalculate relative pressure

# Remove data from after pulling sensors
pull_dates <- list(
  'ELDO' = '2025-10-08 14:10:00',
  'FRCH' = '2025-10-01 10:55:00',
  'MOOS' = '2025-10-06 11:05:00',
  'POKE' = '2025-10-08 11:35:00',
  'VAUL' = '2025-10-07 13:25:00'
)

for(s in names(pull_dates)){
  pt_df_corrected <- pt_df_corrected %>%
    filter(!(site == s & datetime >= pull_dates[[s]]))
}

# Replacement PT started logging before being deployed
pt_df_corrected <- pt_df_corrected %>% 
  filter(!(logger_sn == 20574420 & datetime < '2025-09-19 17:00:00'))

write.csv(pt_df_corrected, pt_processed_out_fpath)


# Plot corrected pressures ----
sites <- unique(pt_df$site)
lapply(sites, function(s) {
  plot_abs_pressure(pt_df_corrected, baro_df, s)
})


# Plot relative pressures ----
for(s in unique(pt_df_corrected$site)){
  site_df <- pt_df_corrected %>% 
    filter(site == s)
  start <- as.Date(min(site_df$datetime, na.rm=TRUE))
  end <- as.Date(max(site_df$datetime, na.rm=TRUE))
  
  p <- ggplot() + 
    geom_point(data = site_df, aes(x=datetime, y=relative_pressure_psi, color=logger_sn)) +
    labs(title = s,
         subtitle = paste(start, '-', end),
         y = 'Relative Pressure (PSI)',
         x = 'Date') + 
    theme_minimal()
  
  ggsave(filename = paste0(s, "_relative_corrected.png"),
         plot = p,
         path = pt_plots_outdir,
         width = 8, height = 5,
         bg='white')
}



