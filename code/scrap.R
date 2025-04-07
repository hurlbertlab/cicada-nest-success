# I only have county code, I want county name

county_codes <- read.csv("https://www2.census.gov/geo/docs/reference/codes2020/national_county2020.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE, strip.white = TRUE) #county df from US Census)

#State FIPS and County FIPS are separate columns, need to merge to match cicada_county 

county_codes$STATEFP <- sprintf("%02d", as.integer(county_codes$STATEFP)) #make sure its 2 digits
county_codes$COUNTYFP <- sprintf("%03d", as.integer(county_codes$COUNTYFP)) #make sure its 3 digits
county_codes$ST_CNTY_CODE <- paste0(county_codes$STATEFP, county_codes$COUNTYFP) #combine

# Now merge to create cicada county with both code and name
cicada_county <- left_join(cicada_county, county_codes, by = "ST_CNTY_CODE")
cicada_county <- subset(cicada_county, select = -c(STATE, STATEFP, COUNTYFP, COUNTYNS, CLASSFP, FUNCSTAT))

# cicada year trial and error
#so you have nestboxes used multiple Years, and a longer time frame, so some counties will have multiple emergence Years. That's not a problem. 
#so....
#need a df of every county associated with ALL the broods that might emerge
#like every row is a county + brood + that brood's emergence Years..
#but there might be more than one row for every county bc there could be more than one brood.
#   that has it's own multiple emergence Years
#once you have that..... you can sort each county for the closest date emergence Year to each row?
# and use that?

#what if we don't add it to bluebirds directly.
#and the first df we make, takes each county, and has a row for each brood with associated Years.
#THEN all we need
new_nests_year_county <- data.frame()

temp_nbroods <- nestboxes_county_cicada %>%
  group_by(ST_CNTY_CODE) %>%
  filter(!is.na(BROOD_NAME)) %>%
  summarize(n_broods = length(unique(BROOD_NAME)))

for(a in 1:nrow(temp_nbroods)) {
  
  temp_syc <- nestboxes_county_cicada %>% 
    filter(ST_CNTY_CODE == temp_nbroods$ST_CNTY_CODE[a])
  
  if(temp_nbroods$n_broods[a] == 1) {
    
    yr_emergence <- max(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    #the max doesn't mean anything, bc there's only one emergence Year. Just removing the NAs
    
    temp_syc <- temp_syc %>%
      mutate(cicada_year = Year - yr_emergence)
    
  } else if(temp_nbroods$n_broods[a] == 2) {
    
    #get the latest emergence Year
    yr_emergence_one <- max(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    #get the earliest emergence Year
    yr_emergence_two <- min(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE) 
    #create two cicada_years, take the min, don't replace if emergence Year is a 0
    temp_syc <- temp_syc %>%
      mutate(emone = Year - yr_emergence_one,
             emtwo = Year - yr_emergence_two,
             cicada_year = case_when(
               cicada_year == 0 ~ cicada_year, #keep the same
               cicada_year != 0 ~ pmin(emone, emtwo, na.rm = TRUE)
             )) %>%
      dplyr::select(-emone, -emtwo)
    
  } else if(temp_nbroods$n_broods[a] == 3) {
    
    #get the three emergence Years
    yr_emergence_one <- max(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    yr_emergence_two <- median(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    yr_emergence_three <- min(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    
    #create three cicada_years, take the min, don't replace if emergence Year is a 0
    temp_syc <- temp_syc %>%
      mutate(emone = Year - yr_emergence_one,
             emtwo = Year - yr_emergence_two,
             emthree = Year - yr_emergence_three,
             cicada_year = case_when(
               cicada_year == 0 ~ cicada_year, #keep the same
               cicada_year != 0 ~ pmin(emone, emtwo, emthree, na.rm = TRUE)
             )) %>%
      dplyr::select(-emone, -emtwo, -emthree)
    
  } else if(temp_nbroods$n_broods[a] == 4) {
    
    #get the four emergence Years
    yr_emergence_one <- max(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    yr_emergence_two <- sort(temp_syc$Year[temp_syc$cicada_year == 0], decreasing = TRUE)[2]
    yr_emergence_three <- sort(temp_syc$Year[temp_syc$cicada_year == 0], decreasing = TRUE)[3]
    yr_emergence_four <- min(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    
    #create four cicada_years, take the min, don't replace if emergence Year is a 0
    temp_syc <- temp_syc %>%
      mutate(emone = Year - yr_emergence_one,
             emtwo = Year - yr_emergence_two,
             emthree = Year - yr_emergence_three,
             emfour = Year - yr_emergence_four,
             cicada_year = case_when(
               cicada_year == 0 ~ cicada_year, #keep the same
               cicada_year != 0 ~ pmin(emone, emtwo, emthree, emfour, na.rm = TRUE)
             )) %>%
      dplyr::select(-emone, -emtwo, -emthree, -emfour)
    
  } else {
    #error out
    assertthat::assert_that(1 == 2, msg = "There's a brood with more than 4 BROOD_NAMES or an unexpected number of broods")
  }
  new_nests_year_county <- bind_rows(new_nests_year_county, temp_syc)
}


#================================================================#
#================================================================#
library(daymetr)
library(dplyr)
library(tidyr)
library(tigris)   
library(sf)       

# US county shapefile
county_shapefile <- counties(cb = TRUE) %>%
  st_transform(county_shapefile, crs = 4326)

# Define a set of latitude and longitude points within the eastern United States
points <- data.frame(
  site = c("point1", "point2", "point3", "point4"),
  lat = c(35.0, 40.0, 30.0, 45.0),
  lon = c(-80.0, -75.0, -85.0, -70.0)
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
  
  # Add the point's lat and lon to the data
  daymet_df <- as.data.frame(daymet_data$data)
  daymet_df$lat <- lat
  daymet_df$lon <- lon
  
  data_list[[i]] <- daymet_df
}

# Combine all data into a single data frame
climate_data <- bind_rows(data_list) %>%
  mutate(date = as.Date(paste(year, yday, sep = "-"), format = "%Y-%j"),
         month = as.numeric(format(date, "%m"))) %>%
  mutate(tmean = (tmin..deg.c. + tmax..deg.c.) / 2) %>%
  filter(month %in% c(5, 6, 7))

# Convert the climate data to an 'sf' object (spatial data)
climate_data_sf <- st_as_sf(climate_data, coords = c("lon", "lat"), crs = 4326)%>%
  st_transform(climate_data_sf, crs = 4326)

# Spatial join to add the county information to the climate data
climate_data_with_county <- st_join(climate_data_sf, county_shapefile, join = st_intersects)

# Convert to a data frame
climate_data_with_county_df <- as.data.frame(climate_data_with_county)

# Now calculate monthly data by county
monthly_data_by_county <- climate_data_with_county_df %>%
  group_by(county_name = NAME) %>%
  group_by(year, month, county_name) %>%
  summarise(
    min_temp = min(tmin..deg.c.),
    mean_temp = mean(tmean),
    max_temp = max(tmax..deg.c.),
    total_precip = sum(prcp..mm.day.)
  ) %>%
  ungroup()

# Calculate long-term averages (2007-2024) by county
long_term_avg_by_county <- monthly_data_by_county %>%
  filter(year < 2024) %>%
  group_by(county_name, month) %>%
  summarise(
    long_term_min_temp = mean(min_temp),
    long_term_mean_temp = mean(mean_temp),
    long_term_max_temp = mean(max_temp),
    long_term_total_precip = mean(total_precip)
  )
#================================================================#
#================================================================#
# Get raw daymet
daymet_data <-download_daymet_batch(file_location = "data/nestcoordinates.csv",
                                    start = 1980,
                                    end = 2024,
                                    internal = FALSE,
                                    path = "data/")

wardaymet_df <- daymet_data$data
<<<<<<< HEAD
<<<<<<< HEAD

#================================================================#
#================================================================#
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


#
historical_climate <- daymet_data %>% 
  group_by(site) %>%
  summarize(meanTemp,
            meanPrecip)


=======
>>>>>>> 994044e83b95c2afb0f780442e8bace2c57d5181
=======
>>>>>>> 994044e83b95c2afb0f780442e8bace2c57d5181
