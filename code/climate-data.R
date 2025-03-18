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
  select(Location.ID,Latitude,Longitude)

write.csv(nest_summaries,"data/nestcoordinates.csv",row.names = FALSE)


# Get raw daymet
daymet_data <- download_daymet_batch(file_location = "data/nestcoordinates.csv",
                      start = 1980,
                      end = 2010,
                      internal = TRUE) %>%
  mutate(tmean = ((tmin..deg.c. + tmax..deg.c.) / 2)) %>%
  group_by(site, year) %>%
  summarize(summerTemp = mean(tmean[yday %in% 153:213], na.rm = TRUE),
            summerPrecip = sum(prcp.mm.day.[yday %in% 153:213], na.rm = TRUE)) 

write.csv(daymet_data)
 
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


  

