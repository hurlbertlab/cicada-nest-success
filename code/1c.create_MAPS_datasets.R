##############
#
# Exploring MAPS data
# and preparing datasets
# for use
#
##############

library(dplyr)
library(tidyverse)
library(sf)
library(stringr)
library(ggplot2)
library(daymetr)

#read in cicada info
cicada_emergence_years <- read.csv("data/cicada/cicada_emergence_years_wide.csv")  %>% ## this has broods with 4 emergence years in 4 separate columns
  dplyr::select(-emergence_2019_through_2024)
cicada <- st_read(dsn = "data/cicada/periodical_cicada_with_county.gdb")
#st_crs(cicada) #awesome, already the same crs as MAPs

#load in maps data
locations <- read.csv("data/MAPS_1992-2018_cicada_region/MAPS_STATION_location_and_operations.csv") |>
  st_as_sf(coords = c('DECLNG', "DECLAT"), crs = 4269, remove = FALSE) |>
  #remove when we don't have lat/lon (only affects 4 sites)
  filter(LATITUDE != "-") |>
  rename(STATION_NAME = NAME) |>
  #filter to only locations within a cicada county
  st_join(cicada, join = st_within) |> #dataset gets longer b/c some sites are within the bounds of multiple broods.
  #filter out when it's not within a brood location
  filter(!is.na(BROOD_NAME))

ggplot(locations) + geom_sf()

  n_distinct(locations$STATION_NAME)
  #283 locations.
  #alright now what!
  #we need to add in brood information.....
  #and using the D columns filter to locations that were surveyed in ... at least one cicada year ... hm ...in a way it didn't make sense to for the nestbox data I guess I could have a random effect of station?
  #ah, I think actually, no. I don't need to filter based on being sampled in at least one cicada year.
  #that's going to come later ok? At the least: here are the locations for which we need to get climate data.
  #LATER we can think about if filtering for stations with that kind of stuff is necessary. B/c it's going to all be either cicada year or year before or year after filtering later. And no problem b/c with nestwatch data, I allow for all that variation. I don't like, need a location to be sampled ALL THREE YEARS, I just say - if there's a boost it's gonna affect everywhere. And I just need a general idea of nest success across all years in the years before, year of, and year after a cicada emergence. 
  
  #write.csv(locations, "data/MAPS_locations.csv")

  #and hm. let's go ahead and do climate stuff in here b/c it's not an issue and doesn't need a big loop with only 283 locations.
if(file.exists("data/maps_climate_data.csv")) {
  #if the climate data already exists, just read it in.
  maps_climate <- read.csv("data/maps_climate_data.csv")
  print("climate data exists, reading in. variable name is maps_climate")
} else {
  distinct_locs <- locations |>
    distinct(STATION_NAME, .keep_all = TRUE) |>
    select(STATION_NAME, DECLNG, DECLAT) |>
    st_drop_geometry()
  #and this MAPS data spans from 1991 to 2018.
  #AH. but to calculate anomaly we need the data from 1980 onwards anyway. cheers!
  
  data_list <- list()
  error_list <- list()
  startYear <- 1980
  endYear <- 2025 #even though we won't need the 2025 data, it's used in the anomaly calculations for the nestwatch data so we should use it here too.

  start_idx <- 1
  end_idx <- nrow(distinct_locs)
  
  data_index <- 1

  # Loop through each point and download data
  for (i in start_idx:end_idx) {
    daymet_data <- tryCatch({
      download_daymet(
        site = as.character(distinct_locs$STATION_NAME[i]),  # Ensure it's a string
        lat = as.numeric(distinct_locs$DECLAT[i]),       # Ensure it's numeric
        lon = as.numeric(distinct_locs$DECLNG[i]),      # Ensure it's numeric
        start = startYear,
        end = endYear, 
        internal = TRUE
      )
    }, error = function(e) {
      df <- data.frame(site = rep(distinct_locs$STATION_NAME[i], endYear - startYear + 1),
                       year = startYear:endYear,
                       mean_temp = rep(NA, endYear - startYear + 1),
                       mean_precip = rep(NA, endYear - startYear + 1)
      )
      
      error_list[[length(error_list) + 1]] <<- distinct_locs[i, ]  
      return(NULL)  # Return NULL to show failure
    })
    
    # Skip if data retrieval failed
    if (is.null(daymet_data)) next
    
    # Convert to data frame and store in list
    df <- as.data.frame(daymet_data$data) %>%
      filter(yday >= 121 & yday <= 212) %>%  # Filter to relevant Julian days
      group_by(year) %>%
      summarize(
        mean_temp = mean((tmin..deg.c. + tmax..deg.c.) / 2, na.rm = TRUE),  
        mean_precip = mean(prcp..mm.day., na.rm = TRUE)
      )
    
    df$site <- distinct_locs$STATION_NAME[i]  # Add site ID column for reference
    data_list[[data_index]] <- df  # Store sequentially
    data_index <- data_index + 1  # Increment index
    
    timestamp()
    print(i)
  } #end loop downloading climate data for each MAPS location.
  
  climate_df <- bind_rows(data_list)
  #couldn't download data for one site: North 1/3 of Comp. 16 at 32.33083/-91.33611 latitude/longitude
  #tried to run it again to see if that fixed it but nah
  #that's okay, a recovery rate of 282/283 is not an issue.
  
  maps_climate <- climate_df |>
    #rename variables to be more informative
    rename(STATION_NAME = site) |>
    rename(Year = year) |>
    rename(y_temp = mean_temp) |>
    rename(y_precip = mean_precip) |>
    #for each location calculate long term temperature means and year anomalies from those means
    group_by(STATION_NAME) |>
    mutate(mean_temp = mean(y_temp, na.rm = TRUE))|>
    mutate(mean_precip = mean(y_precip, na.rm = TRUE)) |>
    mutate(y_anomaly_temp = y_temp-mean_temp) |>
    mutate(y_anomaly_precip = y_precip-mean_precip) |>
    mutate(n_years = n()) |>
    ungroup() |>
    group_by(STATION_NAME, Year) |>
    distinct(.keep_all = TRUE) |>
    ungroup() |>
    #and we also only are going to need data from 1992 to 2018
    filter(Year >= 1992) |>
    filter(Year <= 2018)
  
  #save the climate data csv
  write.csv(maps_climate, "data/maps_climate_data.csv", row.names = FALSE)

} #end if/else to get climate data
  

  