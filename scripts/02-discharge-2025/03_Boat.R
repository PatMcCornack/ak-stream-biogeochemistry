## Purpose: Compile data from Boat .dis files - these are exported from .rqsmb files using RSQ
## Inputs: Boat discharge data from google drive
## Outputs: csv file containing discharge measurements
## Edited by: Pat McCornack (08/25/2025)

rm(list=ls())
library(tidyverse)
library(here)
library(googledrive)
library(googlesheets4)
library(dplyr)
library(readxl)
library(zoo)
library(readr)

#### EDIT HERE ### ----
# These variables will change year to year and need to be updated - the rest of the script shoudn't need to be modified to run
boatURL <- "https://drive.google.com/drive/u/0/folders/1TgioRbqUmr5BG4tJWe_oInFZ9YzTnKo3"  # Set this to current years boat discharge directory
Q_out_fpath <- "data/2025/boat/Q_boat_2025.csv"  # File path to save out to - relative to RProj file location
boat_data_dir <- 'data/2025/boat/individual_files'  # Where to save boat .dis files

### Functions ### ----

# Extract relevant info from .dis files
extract_data <- function(fname) {
  # Get metadata
  lines <- readLines(fname)
  site <- str_trim(str_split(lines[5], ';', simplify=TRUE)[1,2])
  date <- str_trim(str_split(lines[2], ':', simplify=TRUE)[1,2])
  
  # Get measurements
  df <- suppressWarnings(read_tsv(fname, skip = 51, show_col_types=FALSE))  # Skip metadata before table
  df <- df %>% filter(!is.na(`Total Q (m³/s)`))  # Drop rows without discharge
  discharge_mean <- mean(df$`Total Q (m³/s)`)
  discharge_sd <- sd(df$`Total Q (m³/s)`)
  datetime <- as.POSIXct(df[[1,4]], format='%Y-%m-%d %H:%M:%S', tz='UTC')  # Not actually UTC
  
  # Combine into a dataframe
  data.frame(
    site = site,
    datetime = datetime,
    discharge_mn_m3.s = as.numeric(discharge_mean),
    discharge_sd_m3.s = as.numeric(discharge_sd)
  )
}


### Extract discharge to single file ### ----

# Download .dis boat files from Drive to local
boat_new <- drive_get(as_id(boatURL)) # Authenticate session
boat_glist <- drive_ls(boat_new, pattern='.dis', recursive=TRUE)  # Recursion lets us check each subdirectory

setwd(here(boat_data_dir))  # Download data here
walk(boat_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))

# Extract data from each file
file_list <- list.files(pattern='.dis')
boat_df <- bind_rows(lapply(file_list, extract_data))
boat_df <- boat_df %>% mutate(discharge_mn_L.s = `discharge_mn_m3.s` * 1000)  # Calculate L/s discharge

boat_df <- boat_df %>% 
  mutate(site = toupper(site)) %>%  # Convert site to all uppercase
  mutate(discharge_cv = discharge_sd_m3.s / discharge_mn_m3.s)  # Calculate coefficient of variation

# Save combined file
setwd(here())
write.csv(boat_df, here(Q_out_fpath), row.names = FALSE)