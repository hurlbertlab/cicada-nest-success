##################
#
# Getting climate data for the nestbox locations in the eastern united states
# from 2007-2025
# !!!!!!!!!!!!!!2025 daymet data is not yet available for download. So we can only use the subset of nests through 2024 for now.
# writes filtered_climate_data.csv as the ultimate output (available in the git repository)
#
# For those interested in recreating the full all_climate_data.csv, see the last section separated by
#################################
# For those interested in the process of collecting the climate data and then summarizing it from daymet,
# the whole file is of interest.
#
###################

library(daymetr)
library(dplyr)
library(tidyr)
library(beepr)

# Create CSV in The format of: site name, latitude, longitude, where each site is a nestbox
nest_summaries <- read.csv("data/filtered_nestbox_summaries.csv") |>
  select(Location.ID, Latitude, Longitude) %>%
  mutate(coordinate_pairs = paste(Latitude, Longitude, sep = ",")) %>%
  distinct(coordinate_pairs, .keep_all = TRUE) %>%
  select(-coordinate_pairs)

# However, we've already run this code before. We only need to get the climate data now for the locations we don't yet have data on
# May need to run script at bottom of code if you don't have the climate data csv (which is too large to be stored on github), that takes all the daymet chunks and puts them together.
#We have more already_have_climate rows than we do nest_summaries because of an added filter on nest timing, so for nests where we had NO timing information (hatch, lay, or fledging date) we filtered those out.
already_have_climate <- read.csv("data/NWV3_all_climate_data.csv") |> #before was: read.csv("data/prev_bella_work/climate_data.csv")
  distinct(Location.ID) |>
  mutate(check = "okay")
  
  #for getting only 2025 data
  #take latest nest_summaries, and filter to the location.IDs 
  #that already have climate data.
  get_missing_2025_climate <- nest_summaries |>
    filter(Location.ID %in% already_have_climate$Location.ID)

nest_summaries <- nest_summaries |>
  filter(!Location.ID %in% already_have_climate$Location.ID)
#now nest_summaries only has the locations for which we don't yet have climate data.
  #confirm with...
  a <- left_join(nest_summaries, already_have_climate, by = "Location.ID")
  nrow(table(a$check)) == 0 #TRUE so we're good.
  already_have_climate <- already_have_climate |> dplyr::select(-check)
  rm(a)

#finding location.IDs with missing coordinates

data_list <- list()
error_list <- list()
chunk_size <- 8000  # Number of locations per chunk
num_chunks <- 6 # number of chunks

startYear <- 1980
#startYear <- 2025 #when getting 2025 climate data only
endYear <- 2025 # 2024 was the most recent data available when this was last run

#nest_summaries <- get_missing_2025_climate #when getting 2025 climate data only for sites we already have from previous work.

for (chunk in 1:num_chunks) {
  start_idx <- (chunk - 1) * chunk_size + 1
  end_idx <- min(chunk * chunk_size, nrow(nest_summaries))
  
  data_index <- 1  # Reset data index for each chunk
  
  # Loop through each point and download data
  for (i in start_idx:end_idx) {
    daymet_data <- tryCatch({
      download_daymet(
        site = as.character(nest_summaries$Location.ID[i]),  # Ensure it's a string
        lat = as.numeric(nest_summaries$Latitude[i]),       # Ensure it's numeric
        lon = as.numeric(nest_summaries$Longitude[i]),      # Ensure it's numeric
        start = startYear,
        end = endYear, 
        internal = TRUE
      )
    }, error = function(e) {
      df <- data.frame(site = rep(nest_summaries$Location.ID[i], endYear - startYear + 1),
                       year = startYear:endYear,
                       mean_temp = rep(NA, endYear - startYear + 1),
                       mean_precip = rep(NA, endYear - startYear + 1)
        )
      
      error_list[[length(error_list) + 1]] <<- nest_summaries[i, ]  
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
    
    df$site <- nest_summaries$Location.ID[i]  # Add site ID column for reference
    data_list[[data_index]] <- df  # Store sequentially
    data_index <- data_index + 1  # Increment index
    
    timestamp()
    print(i)
  }
  # took about 2.5 days / one day second time through in 2026
  # Combine all downloaded data into a single data frame
  chunk_df <- bind_rows(data_list)
  write.csv(chunk_df, paste0("daymet_chunk_", (chunk)+10, ".csv"), row.names = FALSE)
  
  # Clear memory for the next chunk
  data_list <- list()
}

save_errors <- bind_rows(error_list)
#read error list and add the new ones.
prev_errors <- read.csv("data/dayment_unfixed_error_list.csv", row.names = FALSE)
save_errors <- bind_rows(prev_errors, save_errors)
write.csv(save_errors, "data/daymet_unfixed_error_list.csv", row.names = FALSE)


#####################################################################################################################
# # We had quite a bit of missing data when running the first time, but when testing again, got some data for the locations that had missing data previously. Goal here is to run through the so called "bad locations" to see if we can get data for them this time. 
# 
# # more trouble shooting
# woclim <- nestboxes_county_cicada %>% filter(is.na(y_temp))
# write.csv(woclim, file = "woclim.csv", row.names = FALSE)
# 
# 
# 
# badlocations2 <- read.csv("woclim.csv")%>%
#   left_join(nest_summaries) %>%
#   drop_na(Latitude) %>%
#   select(Location.ID, Latitude, Longitude)
# 
# #error at L571386, row 418 of bad locations, same at 429 L571385, 446 L571392, 2037  L571396, 2053  L571383, L571387, L571391, L571393
# #"Error in `filter()`:
# #ℹ In argument: `yday >= 121 & yday <= 212`.
# #Caused by error:
# #! object 'yday' not found
# #Run `rlang::last_trace()` to see where the error occurred."
# 
# data_list <- list()
# error_list <- list()
# 
# for (i in 4000:nrow(badlocations2)) {
#   daymet_data <- tryCatch({
#     download_daymet(
#       site = as.character(badlocations2$Location.ID[i]),  # Ensure it's a string
#       lat = as.numeric(badlocation2$Latitude[i]),       # Ensure it's numeric
#       lon = as.numeric(badlocations2$Longitude[i]),      # Ensure it's numeric
#       start = startYear,
#       end = endYear, 
#       internal = TRUE
#     )
#   }, error = function(e) {
#     df <- data.frame(site = rep(badlocations2$Location.ID[i], endYear - startYear + 1),
#                      year = 1980:2023,
#                      mean_temp = rep(NA, endYear - startYear + 1),
#                      mean_precip = rep(NA, endYear - startYear + 1)
#     )
#     
#     error_list[[length(error_list) + 1]] <<- badlocations2[i, ]  
#     #return(NULL)  # Return NULL to show failure
#   })
#   
#   # Skip if data retrieval failed
#   if (is.null(daymet_data)) next
#   
#   # Convert to data frame and store in list
#   df <- as.data.frame(daymet_data$data) %>%
#     filter(yday >= 121 & yday <= 212) %>%  # Filter to relevant Julian days
#     group_by(year) %>%
#     summarize(
#       mean_temp = mean((tmin..deg.c. + tmax..deg.c.) / 2, na.rm = TRUE),  
#       mean_precip = mean(prcp..mm.day., na.rm = TRUE)
#     )
#   
#   df$site <- badlocations2$Location.ID[i]  # Add site ID column for reference
#   data_list[[data_index]] <- df  # Store sequentially
#   data_index <- data_index + 1  # Increment index
#   
#   
#   timestamp()
#   print(i)
# }
# # Combine all downloaded data into a single data frame
# badlocs_df2 <- bind_rows(data_list)
# write.csv(badlocs_df2,"more_daymetErrors", row.names = FALSE)


######################################################################################

daymet_data_files <- list.files("data/daymet", full.names = TRUE)
climatedata_list <- list()

for(i in 1:length(daymet_data_files)) {
  climatedata_list[[i]] <- data.frame(read.csv(file = daymet_data_files[i]))
}
# ^ code above replaces this below:
# chunk_1 <- read.csv("data/daymet/daymet_chunk_1.csv")
# chunk_2 <- read.csv("data/daymet/daymet_chunk_2.csv")
# chunk_3 <- read.csv("data/daymet/daymet_chunk_3.csv")
# chunk_4 <- read.csv("data/daymet/daymet_chunk_4.csv")
# chunk_5 <- read.csv("data/daymet/daymet_chunk_5.csv")
# chunk_6 <- read.csv("data/daymet/daymet_chunk_6.csv")
# chunk_7 <- read.csv("data/daymet/daymet_chunk_7.csv")
# chunk_8 <- read.csv("data/daymet/daymet_chunk_8.csv")
# chunk_9 <- read.csv("data/daymet/daymet_chunk_9.csv")
# chunk_10 <- read.csv("data/daymet/daymet_chunk_10.csv")
# error_locs <- read.csv("data/daymet/badlocs_fixed_through2053.csv")
# more_error_locs <- read.csv("data/daymet/badlocs1-417.csv")
# final_error_locs <- read.csv("data/daymet/badlocs_fixed_64-91.csv")
# chunk_1_5_2024 <- read.csv("data/daymet/daymet_chunk_1-5_2024.csv")
# bind_rows(chunk_1, 
#          chunk_2, 
#          chunk_3, 
#          chunk_4, 
#          chunk_5,
#          error_locs,
#          more_error_locs,
#          final_error_locs,
#          chunk_6,
#          chunk_7,
#          chunk_8,
#          chunk_9,
#          chunk_10,
#          chunk_1_5_2024)

climate_data <- bind_rows(climatedata_list) 

statuser::table2(climate_data$year)
# 2024 is missing data for 1,110 observations.
#nooo worries let's try to get those...
check <- climate_data %>% group_by(site) %>% summarize(n = n_distinct(year)) %>% filter(n < 45)
nrow(check)
#ah! these are the same 692 observations that were missing above (which I investigated and removed the code for checking). Basically, I think this is a mismatch between the version of nestwatch that had been downloaded previously (and which we therefore already got the daymet data for) and that I downloaded, and slight differences in filtering criteria. As far as version 3 of Nestwatch goes, I have all the data that meets my filtering criteria. 

climate_data <- bind_rows(climatedata_list) %>%
  #rename variables to be more informative
  rename(Location.ID = site) %>%
  rename(Year = year) %>%
  rename(y_temp = mean_temp) %>%
  rename(y_precip = mean_precip) %>%
  #remove the 692 observations that aren't included in V3 of nestwatch we're working with
  filter(!Location.ID %in% check$site) %>%
  #for each location calculate long term temperature means and year anomalies from those means
  group_by(Location.ID) %>%
  mutate(mean_temp = mean(y_temp, na.rm = TRUE))%>%
  mutate(mean_precip = mean(y_precip, na.rm = TRUE)) %>%
  mutate(y_anomaly_temp = y_temp-mean_temp) %>%
  mutate(y_anomaly_precip = y_precip-mean_precip) %>%
  ungroup() %>%
  group_by(Location.ID, Year) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup()


#save climate data csv
#note: this is too big for github file storage. People reading this code interested in re-creating the full climate_data.csv will need to run the lines above within this ############ chunk
write.csv(climate_data,"data/all_climate_data.csv", row.names = FALSE)

#since we've done all the calculations, we can also now filter our climate data to just the years and variables of interest.
climate_07_thru_24 <- climate_data |>
  filter(Year >= 2007) |>
  dplyr::select(Location.ID, Year, y_anomaly_temp, y_anomaly_precip)
write.csv(climate_07_thru_24, "data/filtered_climate_data.csv", row.names = FALSE)
