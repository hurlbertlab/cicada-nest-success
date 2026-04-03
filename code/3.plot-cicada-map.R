# Load libraries
library(sf)
library(ggplot2)
library(dplyr)
library(maps)
library(ggspatial)
library(viridis)

# --- 1. Read and prepare data ---

# Read cicada data from geodatabase
cicada <- st_read(dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb") %>%
  st_transform(cicada, crs = 4326)

# Get state boundaries for context
east_map <- map_data("state") %>%
  filter(long > -100 & long < -50, lat > 20 & lat < 60)

# Filter nestbox data to 2023 and US locations
nests_2023 <- read.csv("data/filtered_summaries.csv") %>%
  filter(Year == 2023, startsWith(Subnational.Code, "US-"))

# Convert nest data to sf object
nests_sf <- st_as_sf(nests_2023, coords = c("Longitude", "Latitude"), crs = 4326)

# Keep only nests within cicada polygons
nests_in_cicada <- st_join(nests_sf, cicada, join = st_intersects, left = FALSE)

# --- 2. Plot the map ---

ggplot() +
  # Cicada BROOD_NAME_NAME areas
  geom_sf(data = cicada, aes(fill = factor(BROOD_NAME)), color = NA, alpha = 0.6) +
  
  # State outlines
  geom_polygon(data = east_map, aes(x = long, y = lat, group = group),
               fill = NA, color = "black", size = 0.3) +
  
  # Nestbox points inside cicada areas
  geom_sf(data = nests_in_cicada, size = 1, color = "black") +
  
  # Map labels
  labs(title = "Periodical Cicada Emergence by Brood",
       subtitle = "Nestbox locations within cicada brood regions",
       fill = "Cicada Brood") +
  
  # Scalable color palette for many BROOD_NAMEs
  scale_fill_viridis_d(option = "turbo") +
  
  # North arrow and scale bar
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  
  # Clean theme
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())
