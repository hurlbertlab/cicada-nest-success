# Exploring NestWatch Data

library(dplyr)
library(tidyverse)
library(tmap) #Note: what is tmap being used for? Parts of the page may break from raster being depreciated it looks like. Got a warning when loading the package
library(sf)
library(ggplot2)

## Load in nestwatch data 

nestwatch <- read.csv("data/attempts_locs_20240125.csv/nestwatch-summaries-2023.csv")

## create new file with relevant data 
filter_nest_sum <- filter(nestwatch, Longitude > -100, Latitude < 47, Year > 2006, Latitude > 25, Longitude < -60, Species.Name 
                          %in% 
                           c("Eastern Bluebird", "House Wren", "Carolina Chickadee", "Black-capped Chickadee", "Tree Swallow"))

write.csv(filter_nest_sum, "data/filtered_summaries.csv", row.names = FALSE)

nest_summaries <- read.csv("data/filtered_summaries.csv") %>%
  filter(startsWith(Subnational.Code, "US-"))

## total count for each species 
species.totals <- nest_summaries %>% 
  count(Species.Name)

## Top 20 species with most observations
most_obs <- species.totals %>%
  arrange(desc(n)) %>% 
  slice_head(n = 20)

## total count for each species by year
yearly.species.totals <- nest_summaries %>% 
  group_by(Year, Species.Name) %>%
  summarize(total_count = n(), .groups = "drop")

## Top 20 years with most observations
most_obs_year <- nest_summaries %>%
  count(Year) %>%
  arrange(desc(n)) %>%
  slice_head(n=20)

# Map of locations of 2023 Nest Boxes
library(maps)

east_map <- map_data("state") %>%
  filter(long > -100 & long < -50, lat > 20 & lat < 60)

nests_2023 <- nest_summaries %>%
  filter(Year == 2023, startsWith(Subnational.Code, "US-")) 

ggplot() +
  geom_polygon(data = east_map, aes(x = long, y = lat, group = group), fill = "lightblue", color = "navy") +
  geom_point(data = nests_2023, aes(x = Longitude, y = Latitude), size = 1) +
  labs(title = "Nestbox Locations 2023", x = "Longitude", y = "Latitude") 

  
# Load in cicada data 
cicada_emergence_years <- read.csv("data/cicada_emergence_years_wide.csv")  %>% ## this has broods with 4 emergence years in 4 separate columns
  dplyr::select(-emergence_2019_through_2024, - cycle)
cicada <- st_read(dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb")

# Merge and Filter to create cicada table 
cicada_county <- left_join(cicada_emergence_years, cicada, by ="BROOD_NAME")
cicada_county <- subset(cicada_county, select = -c(YEAR_NEXT_EMERGENCE,CYCLE)) 
#check that there's no missing information
  assertthat::assert_that(any(is.na(cicada_county) == FALSE))

# Okay, now the nestwatch data needs county based on coordinates
#turn nest data to points
nest_summaries_points <- st_as_sf(nest_summaries, coords = c('Longitude', 'Latitude'), crs = st_crs(cicada))

#join county + cicada information
nests_county <- 
  st_join(nest_summaries_points, cicada, join = st_within) 
#Note: this doesn't take too long to run on a lab computer, approx. 10 seconds

#now join cicada emergence year information
nests_county_cicada <- nests_county %>%
  left_join(cicada_emergence_years, by = "BROOD_NAME") %>%
  #filter out some columns we don't need, like really old emergence years etc.
  dplyr::select(-emergence_one, -LSAD, -CLASSFP, - MTFCC, -ALAND, -AWATER, -INTPTLAT, -INTPTLON)

#save df 
write.csv(nests_county_cicada, "data/nestboxes_county_cicada.csv", row.names = FALSE)
#Note: this is not saved with geometry. If you want geometry, use st_write. Otherwise, later use will have to add the geometry back in

# Rearrange Ivara's data to have one row for every county, and 2 most recent outbreak years in 2 columns. Join to nestwatch by county.


