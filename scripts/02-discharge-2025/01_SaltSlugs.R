### Compile and calculate discharge data from salt slugs ###

## Inputs: Salt slug data from google drive. Will need to change the URLs if folders change location

## Outputs: compiled datasheet for each site per year, relevant plot

rm(list=ls())
library(tidyverse)
library(data.table)
library(here)
library(googledrive)
library(readxl)
library(readr)
library(googlesheets4)
library(ggplot2)
library(lubridate)

### EDIT HERE ### ----
# These need to be updated yearly, other parts of the script will also need updates. Filepaths are relative to 
slugURL <- "https://drive.google.com/drive/u/0/folders/1v7QM4YLJek4e4lyBdXsTUUtLM8K4BEML"  # Drive directory with slug data
Q_data_dir <- "data/2025/slug/individual_files/"  # Where to download slug files 
Q_out_fpath <- paste0(Q_data_dir, '../Q_slug_2025.csv')  # Where to save out the discharge measurements
metadata_fpath <- 'data/2025/slug/slug_2025_metadata.csv'  # Path to metadata file
plot_out_dir <- "plots/2025/slug/"  # Directory where plots are saved
slugs_out_fpath <- 'data/2025/Slug/slugs_2025.csv'  # Combined slugs file


### Utility Functions ### ----

# Reconstruct timestamp for files that have lost their seconds information. Writes the 
#   changes to the local copy of the file, but not the Drive copy.
reconstruct_timestamp <- function(fpath) {
  df <- read_csv(fpath, show_col_types = FALSE)
  
  # Convert to datetime format
  df$Timestamp <- as.POSIXct(df$Timestamp, format = "%m/%d/%y %H:%M")
  
  # Add seconds based on length of group (minute) in record
  df <- df %>%
    group_by(Timestamp) %>%
    mutate(
      n_in_group = n(),
      start_second = 60 - n_in_group,
      Timestamp = Timestamp + seconds(row_number() - 1 + start_second)
    ) %>%
    ungroup() %>%
    select(-n_in_group, -start_second)  # remove additional columns
  
  df$Timestamp <- as.character(df$Timestamp)  # We want to write out as string
  
  write_csv(df, fpath)
  print(fpath)
}


### Load Data from Drive and Preprocess ### ----

## google drive authentication. First time, select 1, open browser, go to google and allow API access
slug_new <- drive_get(as_id(slugURL))
slug_glist <- drive_ls(slug_new, pattern = "\\.csv$")

## set working directory to subdirectory we want to put the data in
setwd(here(Q_data_dir))

# downloads files from google drive onto local computer in Slug folder
walk(slug_glist$id, ~ drive_download(as_id(.x), overwrite = TRUE))

# Get list of data files
pattern <- ".*\\.csv$"
file_list <- list.files(recursive = F, 
                        pattern = pattern, 
                        full.names = TRUE) 

# Reconstruct seconds part of timestamp for problematic files
file_edit_list <- list('./250711_export.csv', './250627_export.csv')
walk(file_edit_list, reconstruct_timestamp)

# Join all data together
slug_df <- do.call("rbind", lapply(file_list, function(f) {read.csv(f, fileEncoding='UTF-8')}))

# Format timestamp
slug_df$Timestamp <- parse_date_time(slug_df$Timestamp, 
                                orders = c("mdY HMS", "ymd HMS"))
slug_df$Timestamp <- format(slug_df$Timestamp, format = "%m/%d/%Y %H:%M:%S")

# Convert timestamps to POSIXct 
slug_df$Timestamp <- as.POSIXct(slug_df$Timestamp, format = "%m/%d/%Y %H:%M:%S", tz = "UTC")

#  Create 'Date' Object
slug_df$Date <- format(slug_df$Timestamp, "%y%m%d")

# Format column names 
col_names <- c("Timestamp", "Cond_ms.cm", "SPcond_uS.cm", "Temp_c", "Comment", "Site", "Folder", "ID", "Date")
names(slug_df) <- col_names

write.csv(slug_df, here(slugs_out_fpath), row.names = FALSE) # export


### Data Cleanup ### ----
# QC Adjustments
slug_df <- slug_df %>% filter(!(Site == 'FRENCH' & Date == '250515' & SPcond_uS.cm < 20))  # Outlier after end of curve
slug_df <- slug_df %>% filter(!(Site == 'GLEN' & Date == '250710' & SPcond_uS.cm < 200))  # Outliers 


### Plot Conductivity Curves ### ----
setwd(here())  # Return to project home

# Function to plot the conductivity curve
plot_cond_curve <- function(site_df, site_name) {
  site_plot <- site_df %>% 
    ggplot(aes(x = Timestamp, y = `SPcond_uS.cm`)) +
    geom_point() + 
    ggtitle(paste(site_name, "2025")) +
    facet_wrap(~Date, scales = "free")
  
  # Save plot using site_name in filename to avoid overwriting
  ggsave(plot = site_plot, 
         filename = paste0("2025_", site_name, ".pdf"), 
         path = plot_out_dir, 
         width = 10, height = 7.5, units = "in")
}

# List of sites to process
site_list <- c("POKER", "MOOS", "FRENCH", "ELDO", "VAULT", "GLEN")

# Iterate through dataframes to plot them
lapply(site_list, function(site_name) {
  print(site_name)
  site_df <- slug_df %>% filter(Site == site_name)
  plot_cond_curve(site_df, site_name)
})


### Calculate Discharge ### ----
# Load in event metadata
metadata_fpath <- file.path(getwd(), metadata_fpath)
metadata <- read.csv(metadata_fpath)
metadata <- metadata %>% mutate(Date=as.character(Date))

# Join metadata ot each event
events <- slug_df %>% mutate(drop_id=NA_real_) %>% group_by(Date, Site) %>% group_split()  # break into separate 'events' 
events_merged <- list()
for (i in 1:length(events)){
  event_meta <- metadata %>% filter(Date == events[[i]]$Date[1] & Site == events[[i]]$Site[1])  # Get metadata for those observations
  # Fill start/end times based on data where not assigned
  for (j in 1:nrow(event_meta)) {
    if (is.na(event_meta[j, 'start'])){
      event_meta[j, 'start'] <- format(min(events[[i]]$Timestamp, na.rm=TRUE), '%H:%M:%S')
    }
    if (is.na(event_meta[j, 'end'])){
      event_meta[j, 'end'] <- format(max(events[[i]]$Timestamp, na.rm=TRUE), '%H:%M:%S')
    }
  }
  
  # Create timestamps from start/end times
  event_meta <- event_meta %>% mutate(date_formatted = as.Date(as.character(Date), format='%y%m%d', tz='UTC'))
  event_meta <- event_meta %>% mutate(start_datetime = as.POSIXct(paste(date_formatted, start),  format='%Y-%m-%d %H:%M', tz='UTC'))
  event_meta <- event_meta %>% mutate(end_datetime = as.POSIXct(paste(date_formatted, end),  format='%Y-%m-%d %H:%M', tz='UTC'))
  
  # Assign drop IDs and clip out observations
  for (j in 1:nrow(event_meta)) {
    events[[i]] <- events[[i]] %>% mutate(drop_id = if_else(Timestamp >= event_meta[j, 'start_datetime'] &  # 
        Timestamp <= event_meta[j, 'end_datetime'],
        event_meta[j, 'drop_id'],  # the value you want to assign
        drop_id      # keep the original value (or NA if drop_id doesn't exist yet)
    ))
  }
}

# Merge events back to single dataframe
slug_df <- bind_rows(events)
slug_df <- inner_join(slug_df, (metadata %>% select(Site, Date, drop_id, volume_liters, batch_number, grade, batch_cond, notes)), by=c('Site', 'Date', 'drop_id'))

# Calculate baseline for each drop based on first 10 values
baseline <- slug_df %>% dplyr::group_by(Date, Site, drop_id) %>% dplyr::summarise(baseline = mean(head(SPcond_uS.cm, 10),  na.rm = T))

# Bind baseline to events and calculate corrected spCond
slug_df <- merge(slug_df, baseline) %>% 
  mutate (correct.cond = SPcond_uS.cm - baseline) %>% 
  mutate(volume_ml = volume_liters * 1000)

# Calculate discharge for each drop
Q <- slug_df %>% dplyr::group_by(Site, Date, drop_id) %>% dplyr::summarise(numerator = first((volume_ml/1000)*(batch_cond)),
                                                                        denominator = sum(correct.cond, na.rm = TRUE),
                                                                        MeasuredQ_Ls = numerator/denominator, # top of function
                                                                        Time = first(Timestamp), 
                                                                        VolSlugml = first(volume_ml),
                                                                        grade=first(grade),
                                                                        notes=first(notes))
Q <- Q %>% filter(is.finite(MeasuredQ_Ls)) %>% 
  filter(MeasuredQ_Ls < 1e+06) %>% filter(MeasuredQ_Ls > 0)

write.csv(Q, here(Q_out_fpath), row.names = FALSE)

# # add to discharge summary file
# Q_combo <- Q %>% mutate(Method = "YSI") %>% select(-c(numerator, denominator, Rep))
# Q_combo$Date <- as.Date(Q_combo$Date)
# 
# Q_summary <- read.csv(here("data", "Q_summary_2024.csv"))
# 
# Q_summary <- full_join(Q_summary, Q_combo)
# 
# write.csv(Q_summary, here("Data", "Q_summary_2024.csv"), row.names = FALSE)
