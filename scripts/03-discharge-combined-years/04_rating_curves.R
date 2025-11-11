## Purpose: Develop rating curves of the aggregate discharge and PT data. 
## Inputs: Discharge and pressure transducer data
## Outputs: Plot of relationship between discharge/pressure for each site
## Author: Pat McCornack
## Date: 08/04/2025

library(tidyverse)
library(here)
library(data.table)
library(minpack.lm)

## Read in data ----
setwd(here('data/all_years'))
pt_fpath <- 'PT_clean.csv'
q_fpath <- 'Q_summary.csv'

# Load in pressure transducer record
pt_df <- read.csv(pt_fpath) %>% 
  select(-1) %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S")) %>%
  filter(#!(site == 'VAUL' & year(datetime) == 2024),  # All of VAUL 2024 was bad
          year(datetime) == 2024 | year(datetime) == 2025)  # Tamara only wants 24/25

# Load in discharge record
q_df <- read.csv(q_fpath) %>%
  select(-1) %>%
  mutate(datetime = as.POSIXct(datetime, format="%Y-%m-%d %H:%M:%S")) %>% 
  filter(method != 'ADCP') %>%  # Only one ADCP point, seems bad
  filter(!(site == 'ELDO' & (date(datetime) %in% c('2024-06-27', '2024-09-18'))),  # Bad discharge measurements
         !(site == 'FRCH' & (date(datetime) %in% c('2025-06-18', '2024-10-09'))),
         !(site == 'POKE' & (date(datetime) == '2025-07-22')),  
         !(site == 'MOOS' & (date(datetime) == '2025-06-18') & method == 'flowtracker'),
         !(site == 'VAUL' & (date(datetime) %in% c('2024-09-04', '2024-09-17'))),
         !(site %in% c('VAUL', 'ELDO') & year(datetime) == 2024))  # Doesn't add value to these


## Preprocess data ----
# Combine PTs for each site/year
pt_df_gp <- pt_df %>% 
  group_by(site, datetime) %>%
  summarise(stream_pressure_kpa = mean(stream_pressure_kpa, na.rm=TRUE), .groups="drop")

# Center the PT baseline and shift to make it positive
pt_df_gp <- pt_df_gp %>%
 mutate(year = year(datetime)) %>%
 group_by(site, year) %>%
 mutate(stream_pressure_kpa = (stream_pressure_kpa - mean(stream_pressure_kpa, na.rm = TRUE) + 3)) %>%
 ungroup()

# Convert to data.table
pt_dt <- as.data.table(pt_df_gp)
q_dt <- as.data.table(q_df)

# Add date columns for same-day filtering
pt_dt[, date := as.Date(datetime)]
q_dt[, date := as.Date(datetime)]

# Set keys for rolling join
setkey(pt_dt, site, date, datetime)

# Join Q measurement to nearest PT measurement of the same day
df_join <- pt_dt[q_dt, on = .(site, date, datetime), roll = "nearest"]

# Rename and clean
df_join <- df_join %>%
  filter(!site %in% c('STRT', 'GLEN')) %>%
  select(-month_day)

# TEMPORARY REMOVALS
df_join <- df_join %>%
  filter(!(site == 'POKE' & datetime == '2025-07-22 12:08:10'))  # Out of water, current end of record (08/19/25)

# Save out 
write.csv(df_join, here('data/all_years/Q_PT_combined.csv'))



## Plot Discharge - Pressure Relationships ----
qp_outdir <- here('Plots/aggregated_years/Q_P_relationships')
sites <- c('POKE', 'VAUL', 'MOOS', 'FRCH', 'ELDO')
df_join <- df_join %>% 
  drop_na() %>%
  mutate(year = as.factor(year(datetime)))

for (s in sites) {
  df <- df_join %>% filter(site == s)
  
  qp <- ggplot(aes(y = discharge_L.s, x = stream_pressure_kpa), data = df) +
    geom_point(aes(color = year, shape=method), size = 3) +
    geom_text(aes(label = as.Date(datetime)), hjust = -0.1, vjust = 0, size = 3) +  # Label points with date
    theme_light() +
    ggtitle(paste(s, "Rating Curve")) +
    coord_cartesian(clip = "off")  # Allows labels at edge of plot to be seen
  
  ggsave(filename = paste0(s, ".png"),
         plot = qp,
         path = qp_outdir,
         width = 10, height = 8)
}


## Plot Rating Curves ----
rating_curve_outdir <- here('Plots/aggregated_years/rating_curves')

for(s in c('MOOS', 'FRCH', 'ELDO', 'VAUL', 'POKE')){
  print(s)
  df <- df_join %>% 
    filter(site == s,
           complete.cases(.))  %>%
    mutate(year = as.factor(year(datetime)))
  
  # Estimate initial n and c using linear model on log-transformed data
  fit_log <- lm(log(discharge_L.s) ~ log(stream_pressure_kpa), data = df)
  n_start <- coef(fit_log)[[2]]
  c_start <- exp(coef(fit_log)[[1]])
  a_start=.05
  
  # Fit power function
  power.nls <- nlsLM(discharge_L.s ~ c * (stream_pressure_kpa + a)^n,
                   data=df,
                   start=c(c=c_start, n=n_start, a=a_start))
  summary(power.nls)
  
  # Calculate R2
  y_pred <- predict(power.nls)
  y_obs  <- df$discharge_L.s
  
  ss_res <- sum((y_obs - y_pred)^2) # Residual sum of squares
  ss_tot <- sum((y_obs - mean(y_obs))^2) # Total sum of squares
  R2 <- 1 - ss_res/ss_tot
  
  # Define equation label for plot
  params <- coef(power.nls)
  C_hat <- params["c"]
  a_hat <- params["a"]
  n_hat <- params["n"]
  
  eq_label <- bquote(
    atop(
      Q == .(as.numeric(round(C_hat, 2))) * (p + .(as.numeric(round(a_hat, 2))))^.(as.numeric(round(n_hat, 2))),
      R^2 == .(as.numeric(round(R2, 3)))
    )
  )
  
  # Create predicted dataset to plot fit
  newdata <- data.frame(stream_pressure_kpa = seq(min(df$stream_pressure_kpa),
                                                  max(df$stream_pressure_kpa),
                                                  length.out = 200))
  newdata$pred <- predict(power.nls, newdata)
  
  # Plot data + fitted curve
  rating_curve <- ggplot(df, aes(x = stream_pressure_kpa, y = discharge_L.s)) +
                      geom_line(data = newdata, aes(x = stream_pressure_kpa, y = pred),
                                color = "black", size = 0.8) +
                      geom_point(aes(color = year, shape=method), size=2.5) +
                      annotate("text", 
                               x = min(df$stream_pressure_kpa), 
                               y = max(df$discharge_L.s), 
                               label = as.expression(eq_label),
                               hjust = 0, vjust = 1, size = 5, parse = TRUE) +
                      labs(title = s,
                           x = "Stream Pressure (kPa)",
                           y = "Discharge (L/s)") +
                      theme_light() +
    scale_color_brewer(palette = "Set1")
  print(rating_curve)
  
  ggsave(filename = paste0(s, ".png"),
         plot = rating_curve,
         path = rating_curve_outdir,
         width = 10, height = 8)
}
  

## Plot stream pressure head -----
rel_pressure_outdir <- here('Plots/aggregated_years/PT_atm_corrected')

for(s in c('POKE', 'FRCH', 'MOOS', 'ELDO', 'VAUL')) {
  for(y in c(2020, 2021, 2022, 2024, 2025)){
    plot_df <- pt_df_gp %>%
      filter(site == s,
             year(datetime) == y)
    if(nrow(plot_df) == 0){next}
    
    p <- ggplot(data = plot_df) + 
      geom_point(aes(x=datetime, y=stream_pressure_kpa), size=1) +
      labs(title=paste(s, y),
           x='Date', 
           y='Absolute Pressure (kpa)') +
      theme_minimal()
    
    ggsave(plot=p, 
           filename=paste0(s, '_', y, '.jpg'),
           path=rel_pressure_outdir,
           width = 14, height = 8)
  }
}



## Plot timeseries of discharge vs. pressure with fixed scales per site ----
plot_outdir <- here('Plots/aggregated_years/PT_discharge_ts')
if (!dir.exists(plot_outdir)) dir.create(plot_outdir, recursive = TRUE)

# Precompute pressure and discharge ranges for each site
site_ranges <- sites %>%
  map_df(~{
    site <- .x
    pt_range <- pt_df_gp %>%
      filter(site == !!site) %>%
      summarise(min_pressure = min(stream_pressure_kpa, na.rm = TRUE),
                max_pressure = max(stream_pressure_kpa, na.rm = TRUE))
    
    q_range <- q_df %>%
      filter(site == !!site) %>%
      summarise(min_discharge = min(discharge_L.s, na.rm = TRUE),
                max_discharge = max(discharge_L.s, na.rm = TRUE))
    
    tibble(site = site,
           min_pressure = pt_range$min_pressure,
           max_pressure = pt_range$max_pressure,
           min_discharge = q_range$min_discharge,
           max_discharge = q_range$max_discharge)
  })


for (s in sites) {
  site_pt_df <- pt_df_gp %>% filter(site == s)
  site_q_df <- q_df %>% filter(site == s)
  
  # Get fixed scale values for this site
  p_min <- site_ranges %>% filter(site == s) %>% pull(min_pressure)
  p_max <- site_ranges %>% filter(site == s) %>% pull(max_pressure)
  q_min <- site_ranges %>% filter(site == s) %>% pull(min_discharge)
  q_max <- site_ranges %>% filter(site == s) %>% pull(max_discharge)
  
  for (y in c(2020, 2021, 2022, 2024, 2025)) {
    plot_pt_df <- site_pt_df %>% filter(year(datetime) == y)
    plot_q_df <- site_q_df %>% filter(year(datetime) == y) %>%
      arrange(datetime)
    
    if (nrow(plot_pt_df) == 0 & nrow(plot_q_df) == 0) next
    
    # Set fixed x-axis: May 1 to Oct 31 of the year
    x_start <- as.POSIXct(paste0(y, "-05-02 00:00:00"), tz = "UTC")
    x_end <- as.POSIXct(paste0(y, "-10-15 23:59:59"), tz = "UTC")
    
    month_ticks <- seq(from = x_start, to = x_end, by = "1 month")
    
    png_filename <- file.path(plot_outdir, paste0(s, '_', y, '.png'))
    png(filename = png_filename, width = 1200, height = 800, res = 150)
    
    par(mar = c(5, 4, 4, 5) + 0.1)
    
    # Plot stream pressure (left y-axis)
    plot(plot_pt_df$datetime, plot_pt_df$stream_pressure_kpa,
         type = "p", pch = 16, cex = 0.5, col = "blue",
         ylab = "Stream Pressure (kPa)", xlab = "Date",
         main = paste(s, y, "Pressure & Discharge"),
         xlim = c(x_start, x_end),
         ylim = c(p_min, p_max),
         xaxt = "n")  # suppress default x-axis
    
    axis.POSIXct(1, at = month_ticks, format = "%b")
    
    
    # Add discharge (right y-axis) as line
    par(new = TRUE)
    plot(plot_q_df$datetime, plot_q_df$discharge_L.s,
         type = "l", col = "red", lwd = 1.5,
         axes = FALSE, xlab = "", ylab = "",
         xlim = c(x_start, x_end),
         ylim = c(q_min, q_max))
    
    # Add discharge points
    points(plot_q_df$datetime, plot_q_df$discharge_L.s,
           pch = 16, col = "red", cex = 0.5)
    
    axis(side = 4)
    mtext("Discharge (L/s)", side = 4, line = 4)
    
    legend("topright", legend = c("Pressure", "Discharge"),
           col = c("blue", "red"), pch = 16, pt.cex = 0.75)
    
    dev.off()
  }
}
