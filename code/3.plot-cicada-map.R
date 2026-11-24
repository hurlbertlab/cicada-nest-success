# Load libraries
library(sf)
library(ggplot2)
library(dplyr)
library(maps)
library(ggspatial)
library(viridis)

# --- 1. Read and prepare data ---

# Read cicada data from geodatabase
cicada <- st_read(dsn = "data/cicada/periodical_cicada_with_county.gdb") %>%
  st_transform(cicada, crs = 4326)

# Get state boundaries for context
east_map <- map_data("state") %>%
  filter(long > -100 & long < -50, lat > 20 & lat < 60)

# Filter nestwatch to the locations used in the analysis and add back in lat+lon
nests <- read.csv("data/nestwatch_analysis_df.csv") %>%
  select(Location.ID) |>
  distinct(Location.ID) |>
  left_join(
    (read.csv("data/nestwatchV6/attempts_locs_20260120.csv") |>
       select(Location.ID, Latitude, Longitude) |>
       distinct(Location.ID, Latitude, Longitude)),
    by = c("Location.ID")
  ) 

# Filter MAPS to locations within broods, and 
maps_locations <- read.csv("data/MAPS_1992-2018_cicada_region/MAPS_STATION_location_and_operations.csv") |>
  st_as_sf(coords = c('DECLNG', "DECLAT"), crs = 4326, remove = FALSE) |>
  #remove when we don't have lat/lon (only affects 4 sites)
  filter(LATITUDE != "-") |>
  rename(STATION_NAME = NAME) |>
  #filter to only locations within a cicada county
  sf::st_join(cicada, join = st_within) |> #dataset gets longer b/c some sites are within the bounds of multiple broods.
  #filter out when it's not within a brood location
  filter(!is.na(BROOD_NAME))

ggplot(maps_locations) +
  geom_sf(color = "green3", pch = 18)

# Convert nest data to sf object
nests_sf <- st_as_sf(nests, coords = c("Longitude", "Latitude"), crs = 4326)

# Keep only nests within cicada polygons
nests_in_cicada <- st_join(nests_sf, cicada, join = st_intersects, left = FALSE)

# --- 2. Plot the map ---

#w = 795, h = 580

ggplot() +
  # Cicada BROOD_NAME_NAME areas
  geom_sf(data = cicada, aes(fill = factor(BROOD_NAME)), color = NA, alpha = 0.6) +
  
  # State outlines
  geom_polygon(data = east_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.3) +
  
  # Nestbox points inside cicada areas
  geom_sf(data = nests_in_cicada, size = 1, aes(color = "NestWatch")) +
  
  # MAPS points inside cicada areas
  geom_sf(data = maps_locations, pch = 17, aes(color = "MAPS")) + 
  
  # Map labels
  labs(
       #title = "Periodical Cicada Emergence by Brood",
       #subtitle = "Nestbox locations within cicada brood regions",
       fill = "Cicada Brood",
       color = "Data Source") +
  
  # Scalable color palette for many BROOD_NAMEs
  scale_fill_viridis_d(option = "turbo") +
  
  # Combined color scale (drives the new legend)
  scale_color_manual(
    values = c("NestWatch"    = "black",
               "MAPS" = "#238443"), #8BC774 #DD13FF
    limits = c("NestWatch", "MAPS")
  ) +
  
  # Place the points legend ABOVE the viridis legend
  guides(
    color = guide_legend(
      order = 1,                                   # lower = closer to top
      override.aes = list(shape = c(16, 17),       # circle, triangle
                          size  = c(2, 3))
    ),
    fill  = guide_legend(order = 2)
  ) +
  
  # North arrow and scale bar
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  
  # Clean theme
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right",
        legend.box      = "vertical",     # stack the two legends
        legend.box.just = "left"
        )
