## Purpose: Create timeseries plots of hysteresis indices (HI). HIs were generated
##          using a weekly window averaging over each hour (diel timescale). The window was
##          moved forward a single day for each index. 
## Author: Pat McCornack
## Date: 09/26/25


library(tidyverse)
library(here)
library(ggplot2)
library(patchwork)

options(tz='America/Anchorage')


# Path Definitions ----
hi_fpath <- here('data/weekly_hysteresis_indices.rds')
plot_dir <- here('plots/C-Q_plots_PM')

precip_fpath <- 'data/cpcrw_crrel_ppt_190501-221031.csv'

# Read in Data ----
hi_df <- readRDS(hi_fpath) %>%
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(end_date),
         year = year(start_date),
         HI_mn_int = as.numeric(HI_mn_int),
         )

# Read/process precip data
precip_df <- read.csv(precip_fpath, skip=4) %>%
  set_names(c('datetimeAK', 'ppt_mm')) %>%
  mutate(date = as.Date(datetimeAK)) %>%
  group_by(date) %>% 
  summarize(ppt_mm = sum(ppt_mm, na.rm=FALSE)) %>%
  mutate(year = year(date))



# Calculate axis limits ----
# Global y-limits for HI
hi_ymin <- min(hi_df$hi_boot_ci_min, na.rm = TRUE)
hi_ymax <- max(hi_df$hi_boot_ci_max, na.rm = TRUE)

# Global y-limits for Precip
ppt_ymin <- 0
ppt_ymax <- max(precip_df$ppt_mm, na.rm = TRUE)

# Plot ----

hi_df %>%
  group_split(site, year, v) %>%
  walk(~ {
    if (nrow(.x) < 2) return(NULL)
    
    s <- .x$site[1]
    var <- .x$v[1]
    y <- .x$year[1]
  
    subset_precip <- precip_df %>% filter(year == y)
    
    x_min <- as.Date(paste0(y, "-05-01"))
    x_max <- as.Date(paste0(y, "-10-30"))
    

    p1 <- ggplot(data=.x, aes(x=start_date, y=hi_boot_median)) + 
      geom_point(size=3) +
      geom_line() +
      geom_errorbar(
        aes(ymin = hi_boot_ci_min, ymax = hi_boot_ci_max),
        width = 0.2,
        color = "grey40",
        alpha = 0.7
      ) +
      scale_y_continuous(limits = c(hi_ymin, hi_ymax)) +
      scale_x_date(limits = c(x_min, x_max),
                   date_breaks = "1 month",
                   date_labels = "%b") +
      labs(x = "Start Date",
           y = "Hysteresis Index") +
      theme_light() +
      theme(text = element_text(size=24))
    
    p2 <- ggplot(data=subset_precip, aes(x=date, y=ppt_mm)) +
      geom_col(fill='darkblue') +
      scale_y_reverse(
        limits = c(ppt_ymax, ppt_ymin),   # fixed across groups
        breaks = function(x) pretty(x, n = 3)
      ) + 
      scale_x_date(limits = c(x_min, x_max)) +
      labs(y="PPT (mm)",
           title = paste(s, y, var),
           subtitle = 'Weekly Moving Windows with 95% CIs') +
      theme_light() +
      theme(axis.text.x = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_blank(),
            plot.margin = margin(0, 5, 0, 5),
            text = element_text(size=24))
    
    p3 <- p2 / p1 + plot_layout(heights = c(1,4))
    
    #print(p3)
    
    fname <- paste0('weekly_moving_window/2_HI_timeseries/', var, '_', s, '_', y, '_hysteresis_indices.png')  # var_m_norm.png
    ggsave(fname, p3, path=plot_dir, width=18, height=12)
    
  })




