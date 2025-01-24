# Exploring NestWatch Data

library(dplyr)
library(tidyverse)
library(tmap)
library(sf)
library(ggplot2)

## Load in nestwatch data 

nest_summaries <- read.csv("../data/attempts_locs_20240125.csv/nestwatch-summaries-2023.csv")

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

## Map of locations of 2023 Nest Boxes
library(maps)

world_map <- map_data("world") 

nests_2023 <- nest_summaries %>%
  filter(Year == 2023) 

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "#6B8E23", color = "gray70") +
  geom_point(data = nests_2023, aes(x = Longitude, y = Latitude), size = 3) +
  labs(title = "Nestbox Locations 2023", x = "Longitude", y = "Latitude") 

  
# Load in cicada data 
cicada_emergence_years <- read.csv("../data/cicada_emergence_years.csv") %>%
  rename(Year = emergence_year)

# Associate brood with nest observations 
cicada_nest_summaries <- left_join(nest_summaries, cicada_emergence_years, by = "Year")








