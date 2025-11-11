## Purpose: Thermistor files are split into a bunch of directories. This walks 
##          through the provided google drives and extracts them all to a
##          single location.
## Author: Pat McCornack
## Date: 08/15/25

library(googledrive)


# Extract list of .hobo files to download ----
drive_dirs <- c(
  "https://drive.google.com/drive/u/1/folders/1Fgqi_xWPDQvFdgNY1b_P5m3ydCWwg9Ta",
  "https://drive.google.com/drive/u/1/folders/1_v1OHrWlFPBQpDD6vUHjVamW8YpFPARz",
  "https://drive.google.com/drive/u/1/folders/1h_3zpigfC_Y3kL7R5fXKwlj7D68Z8kTF",
  "https://drive.google.com/drive/u/1/folders/1U2O0UUGKbCQHKo5HKWQE6DOw0VGgJBJA",
  "https://drive.google.com/drive/u/1/folders/15DufHuhU51dNiJJG912N6J2O3afrU2pe",
  "https://drive.google.com/drive/u/1/folders/162TCuXKunUoUKaVUFKWW9ECBjP6BwztX",
  "https://drive.google.com/drive/u/1/folders/1jMTjOimV9IuW-UGVxlYJiH9pfuH3GmIP",
  "https://drive.google.com/drive/u/1/folders/1jMb40R0hJls8tUOVnv4_6ZyjZqXERVh2",
  "https://drive.google.com/drive/u/1/folders/1cfNyzLyT0cwf5QROJrFDLGWqgf4PnuKm",
  "https://drive.google.com/drive/u/1/folders/1vZuHruYmVkxQVsIaGlB6J7X2GbFJ8aLf"
)

# Initialize empty dribble to store results
all_hobo_files <- data.frame()

for (dir in drive_dirs) {
  # List all files recursively in the folder
  files <- drive_ls(as_id(dir), recursive = TRUE)
  
  # Filter for .hobo files
  hobo_files <- files[grepl("\\.hobo$", files$name, ignore.case = TRUE), ]
  
  # Combine with results
  all_hobo_files <- rbind(all_hobo_files, hobo_files)
}

# Download those files ----

# Make sure destination directory exists
dest_dir <- "/Users/patmccornack/Downloads/thermistor_downloads"

# Initialize counter for duplicates
name_counts <- integer()

for (i in seq_len(nrow(all_hobo_files))) {
  f <- all_hobo_files[i, ]
  fname <- f$name
  
  # Remove extension for now
  fname_base <- sub("\\.hobo$", "", fname, ignore.case = TRUE)
  
  # Handle duplicates
  if (fname_base %in% names(name_counts)) {
    name_counts[fname_base] <- name_counts[fname_base] + 1
    new_fname <- paste0(fname_base, "_", name_counts[fname_base], ".hobo")
  } else {
    name_counts[fname_base] <- 0
    new_fname <- paste0(fname_base, ".hobo")
  }
  
  # Build full path for download
  dest_path <- file.path(dest_dir, new_fname)
  
  # Download the file
  drive_download(as_id(f$id), path = dest_path, overwrite = FALSE)
  
  cat("Downloaded:", dest_path, "\n")
}

# TEMPORARY: Extract all hobo files previously downloaded ----
setwd(here('data'))
new_data_dir <- 'hobo_files'


file_list <- list.files(pattern='.hobo', recursive=TRUE)
# Exclude any files in the "individual files" subdirectory
file_list <- file_list[!grepl("individual_files", file_list)]

if (!dir.exists(new_data_dir)) {
  dir.create(new_data_dir, recursive = TRUE)
}

# Tracks duplicate names
name_counts <- integer()

for (f in file_list) {
  base_name <- tools::file_path_sans_ext(basename(f))
  
  # Handle duplicates
  if (base_name %in% names(name_counts)) {
    name_counts[base_name] <- name_counts[base_name] + 1
    new_fname <- paste0(new_data_dir, '/', base_name, "_", name_counts[base_name], ".hobo")
  } else {
    name_counts[base_name] <- 0
    new_fname <- paste0(new_data_dir, '/', base_name, ".hobo")
  }
  
  print(new_fname)
  
  # Copy file to new directory and rename
  file.copy(f, new_fname)
}


