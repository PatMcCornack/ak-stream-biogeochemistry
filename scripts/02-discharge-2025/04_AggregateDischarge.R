## Purpose: Join together discharge files from Slugs/Flowtracker/Boat into single file
## Author: Pat McCornack
## Date: 07/15/25

library(tidyverse)
library(googledrive)

### EDIT HERE ### ---
out_fname <- 'Q_summary_2025.csv'
out_fpath <- paste0('data/2025/', out_fname)  # Where to save aggregated discharge data
drive_Q_dir <- 'https://drive.google.com/drive/u/0/folders/188h2Uur_05-rsBQm64ecaI1tMYhNltgi'  # Drive discharge directory

### Load Discharge Files ### ----
# Find file paths
boat_fpath <- list.files(pattern='Q_boat_2025.csv', recursive=TRUE)
ft_fpath <- list.files(pattern='Q_flowtracker_2025.csv', recursive=TRUE)
slug_fpath <- list.files(pattern='Q_slug_2025.csv', recursive=TRUE)

# Load in and process data
col_names <- c('site', 'datetime', 'discharge_L.s', 'grade', 'method')
boat_df <- read_csv(boat_fpath, show_col_types=FALSE) %>% 
  select(site, datetime, discharge_mn_L.s) %>%
  mutate(grade='none') %>% 
  mutate(method='boat')
names(boat_df) <- col_names

ft_df <- read_csv(ft_fpath, show_col_types=FALSE) %>%
  select(site, datetime, discharge_L.s) %>%
  mutate(grade='none') %>%
  mutate(method='flowtracker')
names(ft_df) <- col_names

slug_df <- read_csv(slug_fpath, show_col_types=FALSE) %>% 
  select(Site, Time, MeasuredQ_Ls, grade) %>%
  mutate(method='slug') %>%
  filter(grade != 'unusable')  # Remove very bad slugs
names(slug_df) <- col_names
  

### Join data and save ### ----
join_df <- bind_rows(boat_df, ft_df, slug_df) %>% 
  mutate(site = ifelse(site == "FRENCH", "FRCH", site)) %>%  # Clean up site names
  mutate(site = ifelse(site == "VAULT", "VAUL", site)) %>%
  mutate(site = ifelse(site == "POKER", "POKE", site))
write.csv(join_df, out_fpath, row.names=FALSE)
 
# Save out to drive
#drive_auth()
drive_upload(out_fpath, path=as_id(drive_Q_dir), name=out_fname, overwrite=TRUE)
