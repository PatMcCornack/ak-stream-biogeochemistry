## Purpose: Compile data from FlowTracker .dis files
## Inputs: Wading Rod discharge data from google drive
## Outputs: csv file containing discharge measurements and uncertainty per measurement
## Edited by: Pat McCornack (08/25/2025)

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
# Filepaths are relative to RProj location
ftURL <- "https://drive.google.com/drive/u/0/folders/1cM2_2pDxCcCCIg5vAMIcGSCJaAT_YTBH"  # Set this to current years flowtracker discharge directory
Q_out_fpath <- "data/2025/flowtracker/Q_flowtracker_2025.csv"  # File path to save out to - relative to RProj file location
ft_data_dir <- "data/2025/flowtracker/individual_files"  # Where to save flowtracker files


### Functions ### ----

# Extract relevant info from .dis files
extract_data <- function(fname) {
  df <- suppressWarnings(read_csv(fname, skip=1, n_max=68, col_names=c('var', 'unit', 'value'), na=c("", "NaN"), show_col_types = FALSE))
  site <- df[[1,3]]  # Site_Name
  timestamp <- df[[3,3]]  # Local_Start_Time
  discharge <- df[[24,3]]  # Total_Discharge
  uncertainty <- df[[56,3]]  # Overall (under Discharge_Uncertainty_ISO)
  
  # Combine into a dataframe
  data.frame(
    site = site,
    datetime = as.POSIXct(timestamp, format='%Y-%m-%d %H:%M:%S'),
    discharge_m3.s = as.numeric(discharge),
    uncertainty_perc = as.numeric(uncertainty)
  )
}

### Extract discharge to single file ### ----

# Download .dis flowtracker files from Drive to local
ft_new <- drive_get(as_id(ftURL)) # Authenticate session
ft_glist <- drive_ls(ft_new, pattern='.dis.csv', recursive=TRUE)  # Recursion lets us check each subdirectory

setwd(here(ft_data_dir))  # Download data here
walk(ft_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))

# Extract data from each file
file_list <- list.files(pattern='.dis.csv')
ft_df <- bind_rows(lapply(file_list, extract_data))
ft_df <- ft_df %>% mutate(discharge_L.s = `discharge_m3.s` * 1000)  # Calculate L/s discharge

ft_df <- ft_df %>% 
  mutate(., flag = ifelse(uncertainty_perc > 10, "y", "n")) %>%  # Flag data with high uncertainty
  mutate(site = toupper(site))  # Convert site to all uppercase

# Save combined file
setwd(here())
write.csv(ft_df, here(Q_out_fpath), row.names = FALSE)