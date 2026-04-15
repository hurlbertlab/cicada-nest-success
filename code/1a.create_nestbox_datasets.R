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
library(stringr)
library(ggplot2)

## Load in nestwatch data 
nestwatch <- read.csv("data/nestwatchV6/attempts_locs_20260120.csv")

# create new file with relevant data
filter_nest_sum <- nestwatch |>
  #let's filter out nests that didn't have anything happen 
  filter(!Outcome %in% c("u1", #unknown outcome
                         "u2", #nest monitoring stopped prior to expected fledge date while nest was still active
                         "u3", #no breeding behavior observed
                         "i", #inactive
                         "n" #not monitored
  )) |>
  #and I only want nests during a cicada-relevant time period, not e.g. second broods or anything. Not all nests have all information but I need at least SOME of the information
  mutate(Hatch.Month = str_extract(Hatch.Date, "-[0-9][0-9]-"),
         Lay.Month = str_extract(First.Lay.Date, "-[0-9][0-9]-"),
         Fledge.Month = str_extract(Fledge.Date, "-[0-9][0-9]-")
         ) |> 
  #convert to number
  mutate(Hatch.Month = as.numeric(str_extract(Hatch.Month, "[0-9][0-9]")),
         Lay.Month = as.numeric(str_extract(Lay.Month, "[0-9][0-9]")),
         Fledge.Month = as.numeric(str_extract(Fledge.Month, "[0-9][0-9]"))
         ) |>
  #filter to nesting between April (month 4) and June (month 6). Periodicial cicadas as a group are done by July, see the time graph on inaturalist: https://www.inaturalist.org/taxa/83854-Magicicada 17k observations in June to 600 observations in July
  filter(Hatch.Month > 3 & Hatch.Month < 8 |
         Lay.Month > 3 & Lay.Month < 8 |
         Fledge.Month > 3 & Fledge.Month < 8) |>
  #select for broad location area-of-interest overlap with cicada ranges. We'll narrow this to only counties that have cicadas in a bit.
  filter(Longitude > -100,
         Latitude < 47,
         Year > 2006, #adoption of nestwatch before then too small to be relevant to our study.
         Latitude > 25,
         Longitude < -60,
         #we want 2025 data even b/c there's no cicada emergence for important one-year-after data for the 2024 emergence.
         Year <= 2025,
         #Lat/Lon should catch this filter but just in case, only want the US observations b/c that's where these periodical cicada broods of interest are
         startsWith(Subnational.Code, "US-")
         ) |>
  #fix northern house wren naming
  mutate(Species.Name = case_when(
    Species.Name == "Northern House Wren (Northern)" ~ "Northern House Wren",
    TRUE ~ Species.Name
  )) 
  #previously we also filtered the Species.Name for %in% c("Eastern Bluebird", "House Wren", "Carolina Chickadee", "Black-capped Chickadee", "Tree Swallow")
  #for now, going to make this file with all species with more than 2k observations. Later, if we want to exclude species b/c of lacking enough data with overlap with cicadas, we definitely can.
  #Well, we chose the prev. species for widespread distribution, high representation in nestwatch data, and insectivorous diets. Combining the chickadees (7k + 3k) that sets the minimum bound of observations at 15k for inclusion. 
  #okay okay, again, for now let's keep the filter broad.

## total count for each species 
species.totals <- filter_nest_sum %>% 
  count(Species.Name) |>
  arrange(desc(n))

#top 20 species with highest count
most_obs <- species.totals %>%
  arrange(desc(n)) %>% 
  slice_head(n = 20)

#filter to species with at least 2,000 observations in the dataset
nest_summaries <- filter_nest_sum |>
  left_join(species.totals, by = "Species.Name") |>
  filter(n > 2000) |>
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
plot_year_nestbox_map(year = 2025)

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
  #the dataset DOES grow because of overlap with multiple broods. We will cut these later so we've only got the relevant points in the actual cicada emergence years.

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
