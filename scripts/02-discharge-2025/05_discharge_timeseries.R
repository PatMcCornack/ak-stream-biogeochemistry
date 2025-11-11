## Purpose: Plot timeseries of discharge and PT data.
## Inputs: Slug and wading rod compiled discharge data
## Outputs: timeseries plot per sampling season
## Date: Revised 08/01/2025

library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)


#### EDIT HERE ### ----
# These variables will change year to year and need to be updated - the rest of the script shoudn't need to be modified to run
plots_outdir <- 'Plots/2025/discharge_timeseries'


## Load and preprocess data ----
q_df <- read.csv(here('data/2025/Q_summary_2025.csv'))
q_df$datetime <- as.POSIXct(q_df$datetime, format='%Y-%m-%d %H:%M:%S')

pt_df <- read.csv(here('data/2025/pressure_transducer/PT_2025.csv'))
pt_df$datetime <- as.POSIXct(pt_df$datetime, format='%Y-%m-%d %H:%M:%S')


## Plot discharge as timeseries ----
sites <- c('POKE', 'MOOS', 'FRCH', 'ELDO', 'GLEN', 'VAUL')

for (site_name in sites) {
  site_q <- q_df %>% filter(site == site_name)
  
  p <- ggplot(data = site_q) +
          geom_point(aes(x = datetime, y = discharge_L.s, color = method), size = 2, alpha = 0.8) +
          labs(
            title = paste(site_name, "Discharge"),
            x = "Date",
            y = "Discharge (L/s)",
            color = "Method"
          ) +
          scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 week") +
        
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
            plot.subtitle = element_text(hjust = 0.5, size = 14),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right",
            panel.grid.major = element_line(color = "grey90"),
            panel.grid.minor = element_blank()
          )
  
    ggsave(filename = paste0(site_name, "_discharge", ".png"),
         plot = p,
         path = plots_outdir,
         width = 8, height = 5)
}
