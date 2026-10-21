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

ggplot(locations) + 
  geom_sf()

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

#OKAY. Now I have to actually um read in the dataset. 
  
# filter to the locations + years with suitable data:
location_years <- locations |>
  #remove Y columns because we only care about operation years if the data is also available to us
  select(-starts_with("Y0")) |>
  select(-starts_with("Y1")) |>
  #pivot
  tidyr::pivot_longer(cols = D89:D18, names_to = "year", names_prefix = "D", values_to = "run_in_year") |>
  #fix years so they're more interpretable when picking up the dataset
  mutate(year = case_when(as.numeric(year) > 70 ~ as.numeric(paste0("19",year)),
                          as.numeric(year) < 70 ~ as.numeric(paste0("20", year))
                          )
         ) |>
  #keep only year-location combinations when we have the data
  filter(run_in_year == "X") |>
  #add in all the information about all potential brood years...
  left_join(
    cicada_emergence_years,
    by = c("BROOD_NAME")
  ) |>
  #okay, and now we keep location-year combinations where they have data during one of the emergence years, or a year before, or a year after. hehe okay long filter...go!
  filter(
    year == emergence_one - 1 |
      year == emergence_one |
      year == emergence_one + 1 |
      year == emergence_two - 1 |
      year == emergence_two |
      year == emergence_two + 1 |
      year == emergence_three - 1 |
      year == emergence_three |
      year == emergence_three + 1 |
      year == emergence_four - 1 | 
      year == emergence_four |
      year == emergence_four + 1 
  ) |>
  #okay now similarly, we need a case_when to tell us this kinda year before/ year of / year after information.
  mutate(cicada_year = 
           case_when(
             year == emergence_one | year == emergence_two | year == emergence_three | year == emergence_four ~ 0,
             year == emergence_one -1 | year == emergence_two -1 | year == emergence_three -1 | year == emergence_four -1 ~ -1,
             year == emergence_one +1 | year == emergence_two +1 | year == emergence_three +1 | year == emergence_four +1 ~ 1
           )) |>
  #awright and now make a binary one while we're here..
  mutate(cicada_year_binary = 
           case_when(cicada_year == 0 ~ 1,
                     cicada_year == -1 | cicada_year == 1 ~ 0)) |>
  #ope, we also need to filter to years >= 1992 b/c there was another protocol used before then and capture efforts are not comprable.
  filter(year >= 1992) |>
  ungroup() |>
  mutate(loc_year = paste0(LOC, year))

#some quick visualization
hist(location_years$year)
hist(location_years$cicada_year)
table(location_years$cicada_year) #epic, all about the same.
table(location_years$cicada_year_binary)

n_distinct(location_years$STATION_NAME)
#cool, and we have 156 unique stations remaining where they banded in +-1 of a cicada year.
  
#get the captures information 
#you may have to unzip the file if you haven't yet, b/c it is stored on github only in zipped form.
captures <- read.csv("data/MAPS_1992-2018_cicada_region/MAPS_BANDING_capture_data.csv") |>
  #start with 728096 total records
  #filter to just the year/location combos of interest
  mutate(loc_year = paste0(LOC, year)) |>
  filter(loc_year %in% location_years$loc_year) |> #cool, we retain about 10% of the capture data. YAY
  #73607/728096
  #create month and day columns, we'll see if we want to filter on those later.
  left_join(
    (location_years |>
       select(LOC, year, STA, STATION_NAME, BROOD_NAME, MULT_BROOD, cicada_year, cicada_year_binary)), by = c("LOC", "year", "STA")) |>
  mutate(month = as.numeric(str_remove_all(str_extract(DATE, "-[0-9][0-9]-"), "-")),
         day = as.numeric(str_remove_all(str_extract(DATE, "-[0-9][0-9]$"), "-"))
         ) |>
  #awesome, well table(captures$month) shows us we've only got information from May to August. Seems good to me!
  #filter to newly banded or recaptured birds. birds without bands we can't know anything about and will not keep records of?
  filter(C %in% c("N", "R"))  |>
  #and also filter to only records meant to be used in productivity or survivorship analyses
  filter(N %in% c("-"))
  #70027/728096, #9.6% of data retained.

#let's start consensing all this information down
summary_captures <- captures |>
  #ah! before we summarize, we need to remove duplicate captures. keep just one unique capture of each bird per year per location to filter out recaptures of the same bird within the season + year
  distinct(year, BAND, LOC, .keep_all = TRUE) |>
  summarize(n_young = sum(AGE == 4 | AGE == 2), #local or hatching year birds
            n_adults = sum(AGE == 1 | AGE >= 5), #bird is at least an after-hatching-year bird through after-third-year bird. We don't worry about score 0s, indeterminable age birds (which accounts for only 2,770 records anyway. Like 1%)
            .by = c(LOC, year, SPEC, cicada_year, cicada_year_binary) # u know.. let's group by location rather than banding station? way fewer 0s
            ) |>
  mutate(productivity = n_young/n_adults) |>
  #and, we can't use any years where no adults were captured, as that brings productivity to infinity
  filter(!is.infinite(productivity)) |>
  #let's also remove weirdly high values, like a productivity of 32 where 98 baltimore oriole young were captured and only 3 adults. That's an outlier adn should not be counted. that still leaves us some cases of high productivity but .. those are .. okay?
  filter(productivity < 20)

table(summary_captures$productivity > 2) #6,875 cases where productivity is less than 2, and 284 cases where productivity is higher than that. Eh, we may filter those out at the end, but I think perhaps okay for now. Probably a lot of those are going to be affect by filters to remove sp without enough data.

ggplot(summary_captures[summary_captures$productivity < 2,], aes(x = productivity, )) +
  geom_density(alpha = 0.5)

  #alright, and now we need to set some minimum filter band on species. We only want to look at species with X number of year/location observations where at least one young was captured. 
n_obser <- summary_captures |>
  group_by(SPEC) |>
  summarize(n = n(),
            n_y_gtr_1 = sum(n_young > 1),
            perc_w_y = n_y_gtr_1/n) |>
  #well, first step, species where young are never observed should be cut
  filter(n_y_gtr_1 > 0) |>
  #and I want at least 30 total observations to work with
  filter(n >= 30) |>
  #oh! and we have eastern bluebird on this list! okay well we definitely want EABL so we can compare between the two data-sets. lets use that as a minimum bound and say at least 20% of the time, we have to have observed young. That means at least 6 observations with young with our minimum bound of 30. hm. think about if that boundary is actually well placed...
  filter(perc_w_y >= .20)
  

  