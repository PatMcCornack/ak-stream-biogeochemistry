## Purpose: Rename files to format SITE_DATE_SN, where date is date of first observation. 
## Author: Pat McCornack
## Date: 08/15/25

library(tidyverse)
library(here)


setwd(here('data'))
new_data_dir <- 'hobo_files'

file_list <- list.files(pattern='^[0-9]{8}.*\\.csv$', recursive=TRUE)


if (!dir.exists(new_data_dir)) {
  dir.create(new_data_dir, recursive = TRUE)
}

# Tracks duplicate names
name_counts <- integer()

for (f in file_list) {
  df <- read.csv(f, skip=1)
  fbase <- basename(f)
  
  fdate <- as.POSIXct(df[1,2], format='%m/%d/%y %I:%M:%S %p')
  fdate <- format(as.Date(fdate), '%y%m%d')
  
  site <- sub(".*?_(?:[^A-Za-z]*)([A-Za-z]{5}).*", "\\1", fbase)  # Extracts site from fname
  serial <- strsplit(fbase, '_')[[1]][1]
  
  
  base_name <- paste0(site, "_", fdate, "_", serial)
  
  # Handle duplicates
  if (base_name %in% names(name_counts)) {
    name_counts[base_name] <- name_counts[base_name] + 1
    new_fname <- paste0("individual_files/", base_name, "_", name_counts[base_name], ".csv")
  } else {
    name_counts[base_name] <- 0
    new_fname <- paste0("individual_files/", base_name, ".csv")
  }
  
  print(new_fname)
  
  # Copy file to new directory and rename
  #file.copy(f, new_fname, overwrite = FALSE)
}

