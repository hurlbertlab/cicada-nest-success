## Getting climate data for eastern united states from 2007-2024
library(daymetr)
library(dplyr)
library(tidyr)



# Create CSV in The format of: site name, latitude, longitude, where each site is a nestbox
nest_summaries <- read.csv("data/filtered_summaries.csv") %>%
  filter(startsWith(Subnational.Code, "US-")) %>%
  select(Location.ID, Latitude, Longitude) %>%
  mutate(coordinate_pairs = paste(Latitude, Longitude, sep = ",")) %>%
  distinct(coordinate_pairs, .keep_all = TRUE) %>%
  select(-coordinate_pairs)

#finding location.IDs with missing coordinates

data_list <- list()
error_list <- list()
chunk_size <- 8000  # Number of locations per chunk
num_chunks <- 5 # number of chunks

startYear <- 1980
endYear <- 2023 # 2023 is the most recent data available at this time

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
                       year = 1980:2023,
                       mean_temp = rep(NA, endYear - startYear + 1),
                       mean_precip = rep(NA, endYear - startYear + 1)
        )
      
      error_list[[length(error_list) + 1]] <<- nest_summaries[i, ]  
      #return(NULL)  # Return NULL to show failure
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
  # took about 2.5 days 
  # Combine all downloaded data into a single data frame
  chunk_df <- bind_rows(data_list)
  write.csv(chunk_df, paste0("daymet_chunk_", chunk, ".csv"), row.names = FALSE)
  
  # Clear memory for the next chunk
  data_list <- list()
}

#####################################################################################################################
# We had quite a bit of missing data when running the first time, but when testing again, got some data for the locations that had missing data previously. Goal here is to run through the so called "bad locations" to see if we can get data for them this time. 

# more trouble shooting
woclim <- nestboxes_county_cicada %>% filter(is.na(y_temp))
write.csv(woclim, file = "woclim.csv", row.names = FALSE)



badlocations2 <- read.csv("woclim.csv")%>%
  left_join(nest_summaries) %>%
  drop_na(Latitude) %>%
  select(Location.ID, Latitude, Longitude)

#error at L571386, row 418 of bad locations, same at 429 L571385, 446 L571392, 2037  L571396, 2053  L571383, L571387, L571391, L571393
#"Error in `filter()`:
#ℹ In argument: `yday >= 121 & yday <= 212`.
#Caused by error:
#! object 'yday' not found
#Run `rlang::last_trace()` to see where the error occurred."

data_list <- list()
error_list <- list()

for (i in 4000:nrow(badlocations2)) {
  daymet_data <- tryCatch({
    download_daymet(
      site = as.character(badlocations2$Location.ID[i]),  # Ensure it's a string
      lat = as.numeric(badlocation2$Latitude[i]),       # Ensure it's numeric
      lon = as.numeric(badlocations2$Longitude[i]),      # Ensure it's numeric
      start = startYear,
      end = endYear, 
      internal = TRUE
    )
  }, error = function(e) {
    df <- data.frame(site = rep(badlocations2$Location.ID[i], endYear - startYear + 1),
                     year = 1980:2023,
                     mean_temp = rep(NA, endYear - startYear + 1),
                     mean_precip = rep(NA, endYear - startYear + 1)
    )
    
    error_list[[length(error_list) + 1]] <<- badlocations2[i, ]  
    #return(NULL)  # Return NULL to show failure
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
  
  df$site <- badlocations2$Location.ID[i]  # Add site ID column for reference
  data_list[[data_index]] <- df  # Store sequentially
  data_index <- data_index + 1  # Increment index
  
  
  timestamp()
  print(i)
}
# Combine all downloaded data into a single data frame
badlocs_df2 <- bind_rows(data_list)
write.csv(badlocs_df2,"more_daymetErrors", row.names = FALSE)





######################################################################################

chunk_1 <- read.csv("daymet_chunk_1.csv")
chunk_2 <- read.csv("daymet_chunk_2.csv")
chunk_3 <- read.csv("daymet_chunk_3.csv")
chunk_4 <- read.csv("daymet_chunk_4.csv")
chunk_5 <- read.csv("daymet_chunk_5.csv")
error_locs <- read.csv("badlocs_fixed_through2053.csv")
more_error_locs <- read.csv("badlocs1-417.csv")

climate_data <- bind_rows(chunk_1, chunk_2, chunk_3, chunk_4, chunk_5,error_locs,more_error_locs)%>%
  rename(Location.ID = site) %>%
  rename(Year = year) %>%
  rename(y_temp = mean_temp) %>%
  rename(y_precip = mean_precip) %>%
  group_by(Location.ID) %>%
  mutate(mean_temp = mean(y_temp, na.rm = TRUE))%>%
  mutate(mean_precip = mean(y_precip, na.rm = TRUE)) %>%
  mutate(y_anomaly_temp = y_temp-mean_temp) %>%
  mutate(y_anomaly_precip = y_precip-mean_precip) %>%
  ungroup() %>%
  group_by(Location.ID, Year) %>%
  distinct(.keep_all = TRUE) %>%
  ungroup()



write.csv(climate_data,"data/climate_data.csv", row.names = FALSE)



  

