#######################
#
# DEPRECIATED - NOT IN USE
#
# (2)
# If the emergence data is sketchy by brood, either b/c of lack of data or b/c of temperature
# effects we think it's not worth it. 
# Calc emergence date based soley on air temperature -> convert to soil temperature -> calculate
# what date cicadas should emerge. And can validate w/the iNat data for recent broods in a few 
# locations as needed.
#
######################

##### Calculating emergence from soil temperature

#get soil temp data for relevant months/year
devtools::install_github("https://github.com/ErikKusch/KrigR")
library(KrigR) 
# Erik Kusch and Richard Davy (2022) KrigR - a tool for downloading and statistically downsacling climate reanalysis data. Environ. Res. Lett. 17 024005

#transform the cicada counties to the same geographic coordinates at ERA5-Land
brood_map <- st_transform(
  brood_map,
  crs = 4326
)
st_crs(brood_map)

#make a bounding box
brood_bbox <- st_bbox(brood_map)
download_extent <- ext(
  brood_bbox["xmin"],
  brood_bbox["xmax"],
  brood_bbox["ymin"],
  brood_bbox["ymax"]
)

#get the nestbox points and put them in the correct crs as well
nestbox_locations <- read.csv("data/nestboxes_w_county+cicada.csv") |>
  left_join(
    (
      read.csv("data/nestwatchV6/attempts_locs_20260120.csv") |>
        dplyr::select(Location.ID, Latitude, Longitude) |>
        group_by(Location.ID, Latitude, Longitude) |>
        distinct() |>
        ungroup()
    ),
    by = "Location.ID"
  ) |>
  dplyr::select(Location.ID, Latitude, Longitude, BROOD_NAME, ST_CNTY_CODE) |>
  group_by(Location.ID, Latitude, Longitude) |> 
  #keep just one record for each location. 
  distinct(.keep_all = TRUE) |>
  ungroup() |>
  #create row ID
  mutate(ID = cur_group_rows()) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(brood_bbox), remove = FALSE) 


#test if KrigR is working...
i = 1
year = as.character(c(2007:2025))

for (i in 1:length(year)) {
  soil_test <- KrigR::CDownloadS(
    Variable = "soil_temperature_level_2", #soil temperature, 7-28 cm
    DataSet = "reanalysis-era5-land",
    
    DateStart = paste0(year[i], "-04-01 00:00"),
    DateStop  = paste0(year[i], "-07-01 00:00"),
    
    TZone = "EST",
    
    TResolution = "day",
    TStep = 1,
    
    Extent = brood_bbox, #ah-ha! Extent can actually be points. But, because of potential crs issues, better to use just the bbox for now. 
    #Extent = nestbox_locations,
    
    Dir = "data/soil_temperature",
    FileName = paste0("soil_temperature_test_", year[i]),
    
    API_User = "ijbg@unc.edu",
    API_Key = "e9144474-6c3a-4488-b847-51ecd9880bf3"
  )
  
  plot(soil_test)
  #take the raster stack and extract the nestbox points. 
  #project points into the same crs
  points_era5 <- terra::project(
    terra::vect(nestbox_locations),
    terra::crs(soil_test)
  )
  
  #check crs match
  crs(points_era5) == crs(soil_test)
  
  test_values <- terra::extract(
    soil_test,
    points_era5
  ) |>
    #pivot
    tidyr::pivot_longer(
      cols = -ID, 
      names_to = "date",
      values_to = "soil_temp"
    ) |>
    mutate(
      #convert Kelvin to C
      soil_temp = soil_temp - 273.15,
      #start some of the julian day stuff
      days_since_03_31 = 
        as.numeric(str_remove(
          str_extract(date, "_[0-9]([0-9])?([0-9])?$"),
          "_"
        )),
      selected_year = year[i]
    ) |>
    dplyr::select(-date) |>
    #add back in the nestbox information
    left_join(
      (nestbox_locations |>
         st_drop_geometry()), by = "ID") 
  
  write.csv(test_values,
            paste0("data/soil_temperature/daily_nestbox_location_soil_temp_", year[i], ".csv")
  )
  #identify the date that 18 degrees C soil temperature is reached?
  
  first_date_18_degrees <- 
    test_values |>
    filter(soil_temp > 18) |>
    group_by(Location.ID) |>
    summarize(first_date_above_18_C = min(days_since_03_31, na.rm = TRUE),
              selected_year = year[i])
  
  write.csv(first_date_18_degrees,
            paste0("data/soil_temperature/first_day_over_18_", year[i], ".csv"))
}

#this is currently at the just, raw ERA5-Land values from the 9-km grid. 

#18 degrees C for cicada emergence comes from
#Heath, J. E. (1968). Thermal synchronization of emergence in periodical “17-year”
#cicadas (Homoptera, Cicadidae, Magicicada). Am. Midl. Nat. 80, 440–448. doi:
#  10.2307/2423537
# & 
#Sato, Y., and Sato, S. (2015). Spring temperature predicts the long-term molting
#phenology of two cicadas, Cryptotympana facialis and Graptopsaltria nigrofuscata
#(Hemiptera: Cicadidae). Ann. Entomol. Soc Am. 108, 494–500. doi: 10.1093/aesa/
#  sav036