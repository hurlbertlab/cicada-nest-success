## Getting climate data for eastern united states from 2007-2024
library(daymetr)
library(dplyr)
library(tidyr)


# Define a set of latitude and longitude points within the eastern United States
points <- data.frame(
  site = c("point1", "point2", "point3", "point4"), # Add more points as needed
  lat = c(35.0, 40.0, 30.0, 45.0),                 # Latitude values
  lon = c(-80.0, -75.0, -85.0, -70.0)              # Longitude values
)

start_year <- 2007
end_year <- 2024

data_list <- list()

# Loop through each point and download data
for (i in 1:nrow(points)) {
  site <- points$site[i]
  lat <- points$lat[i]
  lon <- points$lon[i]
  
  daymet_data <- download_daymet(
    site = site,
    lat = lat,
    lon = lon,
    start = start_year,
    end = end_year,
    internal = TRUE
  )
  
  data_list[[i]] <- as.data.frame(daymet_data$data)
}

# Combine all data into a single data frame
climate_data <- bind_rows(data_list) %>%
  mutate(date = as.Date(paste(year, yday, sep = "-"), format = "%Y-%j"),
         month = as.numeric(format(date, "%m"))) %>%
  mutate(tmean = (tmin..deg.c. + tmax..deg.c.) / 2) %>%
  filter(month %in% c(5, 6, 7))

# Calculate monthly 
monthly_data <- climate_data %>%
  group_by(year, month) %>%
  summarise(
    min_temp = min(tmin..deg.c.),
    mean_temp = mean(tmean),
    max_temp = max(tmax..deg.c.),
    total_precip = sum(prcp..mm.day.)
  ) %>%
  ungroup()

# Calculate long-term averages (2007-2024)
long_term_avg <- monthly_data %>%
  filter(year < 2024) %>%
  group_by(month) %>%
  summarise(
    long_term_min_temp = mean(min_temp),
    long_term_mean_temp = mean(mean_temp),
    long_term_max_temp = mean(max_temp),
    long_term_total_precip = mean(total_precip)
  )


  

