## Purpose: Calculate hysteresis indices and plots HI timeseries + C-Q loops
## Author: Pat McCornack
## Date: 08/26/25

## TO DO: 
#  1. Look into pulling data from another repo without referencing local copy.

library(tidyverse)
library(here)
library(glue)

# Set up the Python source script + python/R interface ----
library(reticulate)
use_virtualenv("r-reticulate", required = TRUE)  # Automatically create/use a virtualenv named "r-reticulate" in the user library
if (!py_module_available("pandas")) {  # Install pandas in that virtualenv if missing
  py_install("pandas", envname = "r-reticulate", pip = TRUE)
}

source_python("python_scripts/hysteresis_metrics.py")  # Import functions from .py file

reticulate::py_run_string("
import warnings
warnings.filterwarnings('ignore', category=RuntimeWarning)
")

# Define functions ----

## Define functions to process  Qdata - years not consistently structured
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

normalize.data <- function(x, na.rm = TRUE){(x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))}

count_na_runs <- function(vec) {
  r <- rle(is.na(vec))
  na_runs <- r$lengths[r$values]
  tibble(run_length = na_runs)
}

run_qc <- function(cq_week_var, logfile, s, v, start_date, end_date){
  ## QC checks for site/week/var combo
  n_days <- length(unique(as.Date(cq_week_var$datetime, tz='America/Anchorage'))) 
  unique_n_var <- length(unique(cq_week_var[[v]]))
  unique_n_q <- length(unique(cq_week_var$q_ls))
  
  # Define conditions
  has_7_days <- n_days == 7 
  no_na_q <- !any(is.na(cq_week_var$q_ls))  
  no_na_var <- !any(is.na(cq_week_var[[v]]))  
  no_gaps <- !any(cq_week_var$time_diff > 15, na.rm=TRUE)
  low_identical_var <- unique_n_var > 100  # >>>> EDIT <<<< Consider a better way to handle identical values
  low_identical_q <- unique_n_q > 100  # >>>> EDIT <<<< Consider a better way to handle identical values
  
  checks <- c(
    "7 days of data" = has_7_days,
    "no NAs in Q" = no_na_q,
    "no NAs in var" = no_na_var,
    "no gaps" = no_gaps,
    "no var interpolation" = low_identical_var,
    "no q interpolation" = low_identical_q
  )
  
  # Check if data set meets all conditions
  if(!all(checks)){
    failed <- names(checks)[!checks]
    msg <- glue("{s} {v} : Start {start_date}, End {end_date}: FAILED checks: {paste(failed, collapse = ', ')}.")
    writeLines(msg, logfile)
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

format_for_hi <- function(cq_week_hourly_norm, v){
  formatted_df <- cq_week_hourly_norm %>% 
    select('hour', !!sym(v)) %>%
    rename_with(~ c("valuedatetime", "datavalue")) %>%
    mutate(valuedatetime = as.POSIXct(  # Convert hour to dummy datetime for HI calc
      paste("1999-12-31", floor(valuedatetime), tz='America/Anchorage'),  # hour part
      format = "%Y-%m-%d %H",
      tz = "America/Anchorage"
    ))
  
  formatted_df <- r_to_py(formatted_df)
  
  return(formatted_df)
}

median_cl_boot <- function(x, conf = 0.95) {
  if (length(x) == 0 || all(is.na(x))) {  # Check if list is empty
    return(data.frame(
      hi_boot_median = NA_real_,
      hi_boot_ci_min = NA_real_,
      hi_boot_ci_max = NA_real_
    ))
  }
  
  lconf <- (1 - conf)/2
  uconf <- 1 - lconf
  require(boot)
  bmedian <- function(x, ind) median(x[ind])
  bt <- boot(x, bmedian, 10000)
  bb <- boot.ci(bt, conf = 0.95, type = "perc")
  data.frame(hi_boot_median = median(x), 
             hi_boot_ci_min = quantile(bt$t, lconf),
             hi_boot_ci_max = quantile(bt$t, 
                                                                                                           uconf))
}

calc_gaps <- function(var){
  gaps <- cq_df %>%
    select(datetime, site, !!sym(var)) %>%
    filter(!is.na(!!sym(var))) %>%
    arrange(site, datetime) %>%
    mutate(year = lubridate::year(datetime)) %>%
    group_by(site, year) %>%
    mutate(
      prev_time = lag(datetime),
      time_diff = as.numeric(difftime(datetime, prev_time, units = "mins"))
    ) %>%
    filter(!is.na(prev_time), time_diff > 15) %>%
    transmute(
      site,
      year,
      variable = var,
      start_time = prev_time,
      end_time   = datetime,
      gap_length = time_diff
    )
  
  return(gaps)
}


# Define Directory paths ----
setwd(here())

## Data directories
sensor_dir <- here('interpolated_obs')
Q_dir <- here('../DoD_Discharge_copy/Predicted_Discharge')  # >>EDIT HERE<< This may need to changed depending on local file structure
                                                            # because it's referencing another repo
hi_outfile <- here('data/weekly_hysteresis_indices.rds')
gaps_outfile <- here('plots/C-Q_plots_PM/data_gaps.csv')

## Plot directories
plot_dir <- here('plots/C-Q_plots_PM')


# Preprocess data ----
# Create a log file to track potential issues
logfile_name <- glue("plots/C-Q_plots_PM/C-Q_log.txt")
logfile <- file(logfile_name, open = "wt")   # "wt" = write, truncate


## Sensor data ----
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


## Discharge Data ----

### Join years
Q_df <- bind_rows(
  process_2019_2022(file.path(Q_dir, '2019/Predicted_Q_2019_gapfill.csv')),
  process_2020_2021(file.path(Q_dir, '2020/Q_2020.csv')),
  process_2020_2021(file.path(Q_dir, '2021/Q_2021.csv')),
  process_2019_2022(file.path(Q_dir, '2022/Predicted_Q_2022_gapfill.csv')))  

## Join C-Q ----
cq_df <- full_join(sensor_df, Q_df, by=c('datetime', 'site')) %>%
  mutate(year = year(datetime),
         month = month(datetime),
         hour = hour(datetime))

rm(Q_df, sensor_df)  # Clear memory used by intermediate objects

# Look for runs of NA in each variable
na_run_counts <- cq_df %>%
  imap_dfr(~ count_na_runs(.x) %>%
             count(run_length, name = "n_runs") %>%
             mutate(variable = .y),
           .id = NULL) %>%
  select(variable, run_length, n_runs)


## Check for gaps ----
vars <- c("q_ls", "no3.uM.lab.int", "SpCond.uScm.int", "fDOM.QSU.int", "Turb.FNU.int")
gaps_df <- bind_rows(lapply(vars, calc_gaps))
write.csv(gaps_df, gaps_outfile)



#  Weekly moving window C-Q HI ----
# Process data using a weekly window that moves by one day

## Preprocessing ----
cq_df <- cq_df %>% 
  mutate(hour = hour(datetime),
         time_diff = difftime(datetime, lag(datetime), units='mins'),
         date = as.Date(datetime, tz='America/Anchorage'))

## HI Calculation Loop ----
# Get list of unique dates
unique_dates <- sort(unique(cq_df$date))
vars <- c("no3.uM.lab.int", "SpCond.uScm.int", "fDOM.QSU.int", "Turb.FNU.int")

# Generate all site/date/variable combinations
var_combos <- crossing(
  site = unique(cq_df$site),
  start_date = unique_dates,
  v = vars
) %>%
  mutate(end_date = start_date + 7)

# Calculate HI for each combination
HI_df <- var_combos %>% 
  mutate(results = pmap(list(site, start_date, end_date, v), function(s, start_date, end_date, v) {
    cq_week_var <- cq_df %>%
      filter(site == s,
             datetime >= as.Date(start_date, tz = "America/Anchorage"),
             datetime < as.Date(end_date, tz = "America/Anchorage")) %>%
      select(datetime, sym(v), q_ls, hour) %>%
      mutate(time_diff = difftime(datetime, lag(datetime), units = "mins"))
    
    # QC
    if (run_qc(cq_week_var, logfile, s, v, start_date, end_date)) return(NULL)
    
    # Aggregate/Normalize
    cq_week_var <- cq_week_var %>%
      group_by(hour) %>%
      summarise(across(c(q_ls, sym(v)), mean, na.rm=TRUE)) #%>%
      #mutate(across(c(q_ls, sym(v)), normalize.data))
    
    # HI calculation
    q_data <- format_for_hi(cq_week_var, "q_ls")
    var_data <- format_for_hi(cq_week_var, sym(v))
    
    hyst_dict <- hysteresisMetrics(q_data, var_data, 60, 60,
                                   debug = FALSE, interpall = TRUE,
                                   discharge_time_spacing_units = "minutes",
                                   response_time_spacing_units = "minutes",
                                   discharge_units = "Lsec")
    
    hyst_dict <- lapply(hyst_dict, function(x) if (is.null(x)) NA else x)  # handle NULLs
    
    # Convert to tibble w hile preserving Hysteresis_Index as list
    hyst_main <- as_tibble(hyst_dict[setdiff(names(hyst_dict), "Hysteresis_Index")])
    hyst_index <- as_tibble(lapply(hyst_dict, list)) %>% select(Hysteresis_Index)
    
    hyst_indices = bind_cols(hyst_main, hyst_index)
    
    
    return(hyst_indices)
  })) %>%
  filter(!map_lgl(results, is.null)) %>%
  unnest(results)


## 95% Confidence Interval ----
# Confidence interval using bootstrapped hysteresis indices
HI_df <- HI_df %>%
  mutate(boot_ci = map(Hysteresis_Index, ~ median_cl_boot(as.numeric(.x)))) %>% 
  unnest_wider(boot_ci) 


## Subset & Save ----
subset_cols <- c("site", "start_date", "v", "end_date",
                 "HI_mean_with_Interp", "HI_standard_deviation_with_Interp",
                 "interpolated Max width of response","Min response","Max response", "Peak Q",
                 "Hysteresis_Index", "hi_boot_median", "hi_boot_ci_min", "hi_boot_ci_max")
new_cols <- c("site", "start_date", "v", "end_date",
              "HI_mn_int", "HI_sd_int",
              "int_max_width_response","min_response","max_response", "peak_q",
              "hysteresis_index", "hi_boot_median", "hi_boot_ci_min", "hi_boot_ci_max")

HI_df <- HI_df %>% select(subset_cols)
names(HI_df) <- new_cols

saveRDS(HI_df, file=here(hi_outfile)) 
close(logfile)


# Plot hysteresis loops + C/Q time series for each window ----  
# >>>> EDIT <<<< This should be revised to be less redundant - but it does work

## Log File ----
logfile_name <- glue("plots/C-Q_plots_PM/weekly_window_plots_log.txt")
logfile <- file(logfile_name, open = "wt")


unique_dates <- sort(unique(as.Date(cq_df$datetime, tz='America/Anchorage')))
# Dictionaries for plot labels
title_dict <- c(
  "FRCH" = "A) French 33%",
  "MOOS" = "B) Moose 38%",
  "POKE" = "C) Poker 25%",
  "STRT"= "D) Stuart 31%",
  "VAUL" = "E) Vault 58%"
)

# Variables to process + clean labels
var_dict <- c(
  "no3.uM.lab.int" = "NO3 (uM)"
)

# "SpCond.uScm.int" = "SPC (uS/cm)",
# "no3.uM.lab.int" = "NO3 (uM)",
# "fDOM.QSU.int" = "fDOM (QSU)",
# "Turb.FNU.int" = "Turbidity (FNU)",
# "abs254.int" = "abs254"


for(start_date in unique_dates){  # Loop through each date
  end_date <- start_date + 7  # Define week based on start_date
  
  start_label <- format(as.Date(start_date, tz='America/Anchorage'), "%y%m%d")
  end_label <- format(as.Date(end_date, tz='America/Anchorage'), "%y%m%d")
  
  cq_week <- cq_df %>% filter(datetime >= as.Date(start_date, tz='America/Anchorage') & datetime < as.Date(end_date, tz='America/Anchorage'))
  
  for(v in names(var_dict)){
    cq_week_var <- cq_week %>% 
      select(site, datetime, !!sym(v), q_ls, hour) %>%
      mutate(time_diff = difftime(datetime, lag(datetime)), units='mins')
    
    # QC checks
    s = 'plotting'
    if(run_qc(cq_week_var, logfile, s, v, start_date, end_date) == TRUE){
      next
    }
    
    cq_week_hourly <- cq_week_var %>% 
      group_by(site, hour) %>% 
      summarise(!!v := mean(!!sym(v), na.rm = TRUE),
                q_ls = mean(q_ls, na.rm=TRUE),
                n = n(),  # Get counts of observations used to calculate hourly means
                .groups='drop')
    
    ## Normalize hourly data ----
    cq_week_hourly_norm <- cq_week_hourly %>%
      group_by(site) %>%
      mutate(!!v := normalize.data(!!sym(v)),
             q_ls = normalize.data(q_ls)) %>%
      ungroup() %>%
      mutate(site = factor(site, levels = names(title_dict))) %>%
      complete(site, fill = c(list(q_ls = NA, hour = NA), setNames(list(NA), v)))  # Forces empty panels for site even if data missing
    
      
    # Intersite comparisons
    p <- ggplot(cq_week_hourly_norm, aes(x=q_ls, y=!!sym(v), color=hour)) +
      geom_point(size=3) +
      labs(title = paste(var_dict[v], ": ", start_label, "—", end_label),
           subtitle = 'Normalized Values',
           x = 'Q (L/s)',
           y = var_dict[v]) +
      facet_wrap(~ site, scales = 'free', labeller = labeller(site=title_dict)) +
      theme_light() +
      theme(text = element_text(size=24))
    #print(p)
    
      fname <- paste0('weekly_moving_window/1_loops/', strsplit(v, "\\.")[[1]][1], '_', start_label, '_', end_label, '_norm.png')  # var_m_norm.png
      ggsave(fname, p, path=plot_dir, width=15, height=12)
    
      p <- ggplot(data=cq_week_hourly_norm) +
        geom_point(aes(x=hour, y=q_ls, color='Q L/s'), size=3) +
        geom_point(aes(x=hour, y=!!sym(v), color = var_dict[v]), size=3) +
        labs(title = paste(var_dict[v], ": ", start_label, "—", end_label),
             subtitle = 'Normalized Values',
             x = 'Hour',
             y = var_dict[v]) +
        facet_wrap(~ site, scales = 'free', labeller = labeller(site=title_dict)) +
        theme_light() + 
        theme(text=element_text(size=24))
      #print(p)
      
      fname <- paste0('weekly_moving_window/0_time_series/', strsplit(v, "\\.")[[1]][1], '_', start_label, '_', end_label, '_norm.png')  # var_m_norm.png
      ggsave(fname, p, path=plot_dir, width=18, height=12)
  }
}
close(logfile)
