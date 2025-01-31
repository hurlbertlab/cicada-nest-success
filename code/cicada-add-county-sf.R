#####################
# read in the cicada map, add county shapefiles, and save.
#####################

library(sf)
library(dplyr)

#read in cicada data
gdb_path = "copperheads/data/cicada/S_USA.Periodical_Cicada_Brood.gdb"
layer_name = "Periodical_Cicada_Brood"

cicada <- st_read(dsn = gdb_path) %>%
  dplyr::mutate(ST_CNTY_CODE = case_when(
    ST_CNTY_CODE ==  "09005" ~ "09150", 
    ST_CNTY_CODE == "09003" ~ "09130", 
    ST_CNTY_CODE == "09001" ~ "09110",
    ST_CNTY_CODE == "09007" ~ "09170",
    ST_CNTY_CODE == "09009" ~ "09190", 
    TRUE ~ ST_CNTY_CODE
  ))
"The Brood tabular information was designed to be joined to County boundaries and to create map services for the public."

#add county boundaries
  #install.packages("tigris")
  library(tigris)
  
  counties_sf <- tigris::counties() %>%
    select(-CSAFP, -CBSAFP, -METDIVFP, -FUNCSTAT)
  
  c <- left_join(cicada, counties_sf, by = c("ST_CNTY_CODE" = "GEOID"))
  
  assertthat::assert_that(any(is.na(c) == FALSE))
  #great, worked perfectly. 
  
  plot(c$geometry)
  #perfection!
  
  #save the updated cicada that now has county information added.
  #if this doesn't work with your computer, try another one. This worked fine on one of mine and then on my lab computer it said it couldn't build a .gdb (even after updating sf)
  st_write(c, dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb", layer = "Periodical_Cicada_Brood_W_County", driver = "OpenFileGDB")

#to load in and plot the new .gdb this created
cicada <- st_read(dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb")
plot(cicada$SHAPE) 

