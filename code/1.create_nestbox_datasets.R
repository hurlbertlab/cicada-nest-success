##############
#
# Exploring Nestwatch Data
# and preparing datasets
# for use
#
##############

library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)

## Load in nestwatch data 
nestwatch <- read.csv("data/nestwatchV3/attempts_locs_20250131.csv")

# create new file with relevant data
filter_nest_sum <- nestwatch |>
  dplyr::filter(Longitude > -100,
                Latitude < 47,
                Year > 2006,
                Latitude > 25,
                Longitude < -60,
                #there's a couple 2025 datapoints but this nestwatch data is from Jan. 2025 so filter those out
                Year <= 2024,
                #Lat/Lon should catch this filter but just in case, only want the US observations b/c that's where these periodical cicada broods of interest are
                startsWith(Subnational.Code, "US-") 
                )|>
  #fix northern house wren naming
  mutate(Species.Name = case_when(
    Species.Name == "Northern House Wren (Northern)" ~ "Northern House Wren",
    TRUE ~ Species.Name
  )) 
  #previously we also filtered the Species.Name for %in% c("Eastern Bluebird", "House Wren", "Carolina Chickadee", "Black-capped Chickadee", "Tree Swallow")
  #for now, going to make this file with all species with more than 5k observations. Later, if we want to exclude species b/c of lacking enough data with overlap with cicadas, we definitely can.
  #Well, we chose the prev. species for widespread distribution, high representation in nestwatch data, and insectivorous diets. Combining the chickadees (10k + 5k) that sets the minimum bound of observations at 15k for inclusion. 
  #okay okay, again, for now let's keep the filter broad.

## total count for each species 
species.totals <- filter_nest_sum %>% 
  count(Species.Name)

#top 20 species with highest count
most_obs <- species.totals %>%
  arrange(desc(n)) %>% 
  slice_head(n = 20)

#filter to species with at least 5,000 observations in the dataset
nest_summaries <- filter_nest_sum |>
  left_join(species.totals, by = "Species.Name") |>
  filter(n > 5000) |>
  #and now drop n because we don't need it
  dplyr::select(-n)

## total count for each species by year
yearly.species.totals <- nest_summaries %>% 
  group_by(Year, Species.Name) %>%
  summarize(total_count = n(), .groups = "drop")

## Top 20 years with most observations
most_obs_year <- nest_summaries %>%
  count(Year) %>%
  arrange(desc(n)) %>%
  slice_head(n=20)

# Save dataset of filtered nestboxes of interest
write.csv(nest_summaries, 
          "data/filtered_nestbox_summaries.csv",
          row.names = FALSE)

# Map of locations of any given year of nest Nest Boxes
library(maps)

east_map <- map_data("state") %>%
  filter(long > -100 & long < -50, lat > 20 & lat < 60)

plot_year_nestbox_map <- function(year = 2023) {
  nests_year <- nest_summaries |>
    filter(Year == year)
  
  ggplot() +
    geom_polygon(data = east_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "navy") +
    geom_point(data = nests_year, aes(x = Longitude, y = Latitude), size = 1) +
    labs(title = paste("Nestbox Locations", year), x = "Longitude", y = "Latitude") 
  
}

plot_year_nestbox_map() #default year is 2023
plot_year_nestbox_map(year = 2007)
plot_year_nestbox_map(year = 2024)

################
# Okay, now add the cicada data
################

# Load in cicada data 
cicada_emergence_years <- read.csv("data/cicada/cicada_emergence_years_wide.csv")  %>% ## this has broods with 4 emergence years in 4 separate columns
  dplyr::select(-emergence_2019_through_2024)
cicada <- st_read(dsn = "data/cicada/periodical_cicada_with_county.gdb")

# Merge and Filter to create cicada table 
cicada_county <- left_join(cicada_emergence_years, cicada, by ="BROOD_NAME")
cicada_county <- subset(cicada_county, select = -c(YEAR_NEXT_EMERGENCE,CYCLE)) 
#check that there's no missing information
  assertthat::assert_that(any(is.na(cicada_county) == FALSE))

# Okay, now the nestwatch data needs county based on coordinates
nests_county_cicada <- nest_summaries |>
    #turn nest data to points
    st_as_sf(coords = c('Longitude', 'Latitude'), crs = st_crs(cicada)) |>
    #Note: this doesn't take too long to run on a lab computer, approx. 10 seconds
    #join county + cicada information
    st_join(cicada, join = st_within) |>
    #now join cicada emergence year information
    left_join(cicada_emergence_years, by = "BROOD_NAME") |>
    #filter out some columns we don't need, like really old emergence years etc.
    dplyr::select(-emergence_one, -emergence_two, -LSAD, -CLASSFP, - MTFCC, -ALAND, -AWATER, -INTPTLAT, -INTPTLON, -Height.m, -Cavity.Entrance.Diameter.cm, -Entrance.Orientation, -Location.Entry.Technique, -Substrate.Other.Description, -Predator.Guard.Other) |>
          #if you need to write to shapefile:
          #st_write(nests_w_county_cicada, "data/nests_county_cicada.shp", delete_layer = TRUE)
    #drop the geometry we no longer need
    st_drop_geometry() |>
  #aaand just in case
  ungroup() 

#write.csv
write.csv(nests_county_cicada, 
          "data/nestboxes_w_county+cicada.csv",
          row.names = FALSE)
#Note: this is not saved with geometry. If you want geometry, use st_write. Otherwise, later use will have to add the geometry back in

#aaaaand it's also probably going to be too big to upload to github. so we'll zip it for storage and add it to the .gitignore
#code is here if needed, but actually by removing a few un-needed columns this squeaks by github's storage limit :)
#zip(zipfile = "data/nestboxes_w_county+cicada",
#    files = "data/nestboxes_w_county+cicada.csv")

#Read back in to confirm things are working OK
#Okay, I think this is probably good!
nestboxes_county_cicada <- read.csv("data/nestboxes_w_county+cicada.csv", sep = ",",header = TRUE, stringsAsFactors = FALSE, row.names = NULL)
