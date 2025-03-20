## Getting climate data for eastern united states from 2007-2024
library(daymetr)
library(dplyr)
library(tidyr)


# Define a set of latitude and longitude points within the eastern United States
#points <- data.frame(
 # site = c("point1", "point2", "point3", "point4"), 
 #lat = c(35.0, 40.0, 30.0, 45.0),                 
  #lon = c(-80.0, -75.0, -85.0, -70.0)              
#)

#start_year <- 2007
#end_year <- 2024

#data_list <- list()

# Loop through each point and download data
#for (i in 1:nrow(points)) {
  
  
  #daymet_data <- download_daymet(
    #site = points$site[i],
    #lat = poinst$lat[i],
    #lon = points$lon[i],
    #start = start_year,
    #end = end_year,
    #internal = TRUE
 # )
  
 # data_list[[i]] <- as.data.frame(daymet_data$data)
#}

# Create CSV in The format of: site name, latitude, longitude, where each site is a nestbox
nest_summaries <- read.csv("data/filtered_summaries.csv") %>%
  filter(startsWith(Subnational.Code, "US-")) %>%
  drop_na(Young.Total) %>%
  select(Location.ID, Latitude, Longitude) %>%
  mutate(coordinate_pairs = paste(Latitude, Longitude, sep = ",")) %>%
  distinct(coordinate_pairs, .keep_all = TRUE) %>%
  select(-coordinate_pairs)




data_list <- list()

# Loop through each point and download data
for (i in 1:nrow(nest_summaries)) {
  daymet_data <- download_daymet(
    site = as.character(nest_summaries$Location.ID[i]),  # Ensure it's a string
    lat = as.numeric(nest_summaries$Latitude[i]),       # Ensure it's numeric
    lon = as.numeric(nest_summaries$Longitude[i]),      # Ensure it's numeric
    start = 1980,
    end = 2024,
    internal = TRUE
  )
  
  # Convert to data frame and store in list
  #may 1st = 121
  #july 31st = 212
  df <- as.data.frame(daymet_data$data) %>%
    filter(yday >= 121 & yday <= 212) %>% #filter to relevant julian days 
    group_by(year) %>%
    summarize(mean_temp = mean(tmin..deg.c. + tmax..deg.c./2),mean_precip = mean(prcp..mm.day.)
              )
  df$site <- nest_summaries$Location.ID[i]  # Add site ID column for reference
  data_list[[i]] <- df
  
}

# Combine all downloaded data into a single data frame
daymet_df <- bind_rows(data_list)

write.csv(daymet_df, "data/daymet_data.csv",row.names=FALSE)

 
#
historical_climate <- daymet_data %>% 
  group_by(site) %>%
  summarize(meanTemp,
            meanPrecip)


# two joins:
# 1) join by site so that every nest record has cols for historical temp and precip
# 2) join by site, year so that every nest record has cols for climate in that year







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


  

