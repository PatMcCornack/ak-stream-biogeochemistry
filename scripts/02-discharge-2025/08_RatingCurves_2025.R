## Purpose: Plot rating curves
## Inputs: PT data and dischare data
## Outputs: Rating curve plots
## Author: Pat McCornack (revised from previous scripts)
## Date: 08/01/2025

library(tidyverse)
library(here)
library(data.table)
library(minpack.lm)

### EDIT HERE ### ----
# Filepaths are relative to RProj
q_fpath <- "data/2025/Q_summary_2025.csv"  # Location of aggregated discharge data
pt_fpath <- "data/2025/pressure_transducer/PT_2025_processed.csv"

pt_q_outfpath <- "data/2025/2025_Q_PT_joined.csv"

rating_curve_outdir <- "plots/2025/rating_curves"  # Where to save rating curve plots


# Load data ----
# Read in discharge data and preprocess
q_df <- read.csv(here(q_fpath)) %>%
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='America/Anchorage'))

# Read in PT data and preprocess
pt_df <- read.csv(pt_fpath) %>% 
  mutate(datetime = as.POSIXct(datetime, format='%Y-%m-%d %H:%M:%S', tz='America/Anchorage'),
         logger_sn = as.factor(logger_sn)) %>%
  select(-1)  # Drop extra index column


# Process data ----

## Select PT records ----
# Select which pressure transducers to use for each site. Notes on why each was 
# chosen included below. 

logger_sns <- c(
  21984143,  # ELDO: Both look good
  20574422,  # FRCH: Marginally better with less jumps
  20452210,  # MOOS: Marginally better with less jumps
  20574425,  # POKE: More complete record
  21984144,  # VAUL: Downstream beginning of record
  21984142  # VAUL: Rest of record
)


# Center the PT baseline and shift to not produce negative values
pt_df_gp <- pt_df %>% 
  filter(logger_sn %in% logger_sns) %>%
  group_by(logger_sn) %>%
  mutate(relative_pressure_psi = (relative_pressure_psi - mean(relative_pressure_psi, na.rm = TRUE) + 3)) %>%
  ungroup()

# Stitch VAUL Together
pt_df_gp <- pt_df_gp %>% filter(!(logger_sn == 21984144 & datetime > '2025-06-11 11:05:00'))

# Plot PT record
for(s in unique(pt_df_gp$site)){
  site_df <- pt_df_gp %>% filter(site == s)
  
  p <- ggplot(data = site_df) + 
    geom_point(aes(x=datetime, y=relative_pressure_psi, color=logger_sn)) + 
    ggtitle(s)
  plot(p)
}


## Join Q and PT ----
# Convert to data.table
pt_dt <- as.data.table(pt_df_gp)
q_dt <- as.data.table(q_df)

# Add date columns for same-day filtering
pt_dt[, date := as.Date(datetime, tz='America/Anchorage')]
q_dt[, date := as.Date(datetime, tz='America/Anchorage')]

# Set keys for rolling join
setkey(pt_dt, site, date, datetime)

# Join Q measurement to nearest PT measurement of the same day
df_join <- pt_dt[q_dt, on = .(site, date, datetime), roll = "nearest"]

# Rename and clean
df_join <- df_join %>%
  filter(!site %in% c('STRT', 'GLEN')) 

# Save out 
write.csv(df_join, here(pt_q_outfpath))


## QC data before plotting ----
#df_join <- df_join %>% filter(method != 'slug')


## Plot Rating Curves ----
rating_curve_outdir <- here('Plots/2025/rating_curves')

for(s in c('MOOS', 'FRCH', 'ELDO', 'VAUL', 'POKE')){
  print(s)
  df <- df_join %>% 
    filter(site == s,
           complete.cases(.))  %>%
    mutate(year = as.factor(year(datetime)))
  
  # Estimate initial n and c using linear model on log-transformed data
  fit_log <- lm(log(discharge_L.s) ~ log(relative_pressure_psi), data = df)
  n_start <- coef(fit_log)[[2]]
  c_start <- exp(coef(fit_log)[[1]])
  a_start = 0.01
  
  # Fit power function
  power.nls <- nlsLM(discharge_L.s ~ c * (relative_pressure_psi + a)^n,
                     data=df,
                     start=c(c=c_start, n=n_start, a=a_start),
                     control = nls.lm.control(maxiter = 500))
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
      Q == .(as.numeric(round(C_hat, 2))) * (p + .(as.numeric(round(a_hat, 2))))^.(as.numeric(round(n_hat, 2)))
    )
  )
  
  # Create predicted dataset to plot fit
  newdata <- data.frame(relative_pressure_psi = seq(min(df$relative_pressure_psi),
                                                  max(df$relative_pressure_psi),
                                                  length.out = 200))
  newdata$pred <- predict(power.nls, newdata)
  
  # Plot data + fitted curve
  rating_curve <- ggplot(df, aes(x = relative_pressure_psi, y = discharge_L.s)) +
    geom_line(data = newdata, aes(x = relative_pressure_psi, y = pred),
              color = "black", size = 0.8) +
    geom_point(aes(color = year, shape=method), size=2.5) +
    annotate("text", 
             x = min(df$relative_pressure_psi), 
             y = max(df$discharge_L.s), 
             label = list(eq_label),
             hjust = 0, vjust = 1, size = 5, parse = TRUE) +
    labs(title = s,
         x = "Relative Pressure (psi)",
         y = "Discharge (L/s)") +
    theme_light() +
    scale_color_brewer(palette = "Set1") +
    
    geom_text(aes(label = date),  # Label points with dates              
              hjust = -0.1, vjust = 0.5,        
              size = 3, show.legend = FALSE) +
    coord_cartesian(clip = "off")
    
  print(rating_curve)
  
  ggsave(filename = paste0("25_",s, ".png"),
         plot = rating_curve,
         path = rating_curve_outdir,
         width = 10, height = 8)
}
