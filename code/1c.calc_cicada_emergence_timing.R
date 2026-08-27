#####################
#
# File to calculate cicada emergence timing. 
#
# Two possible methods:
# (1)
# Take the inaturalist magicicada observations
# Based on the map of cicada emergences + year, group observations into broods. 
# (important to include year AND brood boundaries b/c a few individuals)
# (will emerge a year early / year late relative to the bulk of the brood)
# (and we don't want those included in our timing estimates)
# By brood, quantile regression of latitude vs observation date
# examine those graphs and the fit of those lines
# If fit is good, pick the regression quantile (5%? 10%?) that best captures the start of the emergence
# and use that line to say for lat x. cicadas emerged day y. Generate predicted values for each
# nestwatch nest. 
#
# (2)
# If the emergence data is sketchy by brood, either b/c of lack of data or b/c of temperature
# effects we think it's not worth it. 
# Calc emergence date based soley on air temperature -> convert to soil temperature -> calculate
# what date cicadas should emerge. And can validate w/the iNat data for recent broods in a few 
# locations as needed.
#
#
######################

library(dplyr)
library(stringr)
library(sf) #package for spatial data
library(terra) #package for spatial data
library(statuser) #for table2
library(lubridate) #handles dates
library(quantreg) #for quantile regression

#read in the brood map
brood_map <- st_read(dsn = "data/cicada/periodical_cicada_with_county.gdb")
#plot(brood_map$SHAPE)
#cicada emergence data
cicada_emergence_years <- read.csv("data/cicada/cicada_emergence_years_wide.csv")  %>% ## this has broods with 4 emergence years in 4 separate columns
  dplyr::select(-emergence_2019_through_2024)

# read in inaturalist cicada observations
cicada_obs <- read.csv("data/cicada/inaturalist/inat_magicicada_observations.csv") |>
  mutate(year = as.numeric(str_remove(str_extract(observed_on, "[0-9][0-9][0-9][0-9]-"), "-")),
         month = as.numeric(str_remove_all(str_extract(observed_on, "-[0-9][0-9]-"), "-")),
         day = as.numeric(str_remove(str_extract(observed_on, "-[0-9][0-9]$"), "-"))
         ) |>
  #get rid of columns we don't need
  dplyr::select(-user_name, -url) |>
  #filter captive observations
  filter(!captive_cultivated == "true") |>
  #filter unseasonable observations (which are probably molts)
  filter(month > 03 & month < 09) |>
  #filter out if we don't have a location
  filter(!is.na(latitude) & !is.na(longitude)) |>
  #and make dummy columns to keep around even after we add geometry later
  mutate(Lat = latitude,
         Lon = longitude,
         #and we also need julien date
         j_date = lubridate::yday(observed_on)) 
  
#make points on a map
cicada_geom <- cicada_obs |>
  #turn into points
  st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(brood_map)) |>
  # and I ran plot(cicada$SHAPE) then points(cicada_points) to plot on top of each other, works fine and confirmed in the same CRS and overlap as expected.
  #okay, now join county/brood information to each observation
  st_join(brood_map, join = st_within) |>
  #now join cicada emergence year information
  left_join(cicada_emergence_years, by = "BROOD_NAME") |>
  #aaand select out some columns we don't need
  dplyr::select(-emergence_one, -emergence_two, -LSAD, -CLASSFP, -MTFCC, -ALAND, -AWATER, -INTPTLAT, -INTPTLON) |>
  #and we have some duplicated observations joined b/c of overlap with multiple broods. We went from 55168 observations to 70678 after the join. No bueno! And we don't want any straggler cicadas. So we want to only keep observations where the year of the observation matches the emergence_three or emergence_four of the cicada brood in that county. 
  filter(year == emergence_three | year == emergence_four) 
  #perf. after == 42770 observations
  
  #and unless we end up needing it later, let's drop the geometry for now
cicada_points <- cicada_geom |>
  st_drop_geometry()

#data exploration
table2(cicada_points$year) 
table2(cicada_points$BROOD_NAME) #alllright, yup there's a couple broods that are not well represented at all. Do those broods also show up in the nestwatch data?
#ya, went to check in 2.analysis_cicada_year.R with table2(analysis_df$BROOD_NAME) and indeed all the broods are represented (cool for me. )
#guess its a question of if that latitude:observation date relationship is really actually different for different broods or if they're all along the same line.
#aaaah.. hm... b/c it doesn't matter what all the dates of observation are of the cicadas. What matters is my estimated emergence date for each brood. Hence, the group by then quantile regression
plot(y = cicada_points$j_date,
     x = cicada_points$Lat)
q_glm <- lm(j_date ~ Lat, data = cicada_points)
summary(q_glm) #okay well yup, latitude of course is related to the day of observation. Anyway, once more what matters is getting that quantile emergence date.
# https://www.r-bloggers.com/2019/01/quantile-regression-in-r-2/
#test_quant <- cicada_points |>
#  filter(BROOD_NAME == "Brood XIII")

rqfit <- rq(j_date ~ Lat, 
            tau = .05, #tau = what quantile I want, this is the earliest 5%, which should capture the start of emergences
            data = cicada_points)
summary(rqfit)
#this is fit across broods. 

plot(j_date ~ Lat, data = cicada_points, pch = 16, main = "j_date ~ latitude")
abline(coef(rqfit), pch = "solid", lwd = 4, col = "lightgreen")
#fitting the 5% vs 10% isn't a big change, a difference of intercept more than a huge difference in slope.

# Okay, now export the rqfit so we can load it in in the analysis df and use predict()
save(rqfit, file = "data/cicada/model_j_date_latitude_05_quartile.R")

#one test that matters is how different this lat:j_date relationship is for each brood vs all the data together. 
  # brood_rqfit <- list()
  # tau_10_summary <- data.frame(x = 1:13)
  # brood_name <- unique(cicada_points$BROOD_NAME) #breaking on brood XXII which only has 2 observations. So, let's cut that b/c we can't fit a quantile, it's list element 11
  # brood_name <- brood_name[-11]
  # for(i in 1:length(brood_name)) {
  #   
  #   brood_rqfit[[i]] <- rq(j_date ~ Lat,
  #                        tau = 0.10,
  #                        data = cicada_points[cicada_points$BROOD_NAME == brood_name[i],]
  #                        )
  #   
  #   print(brood_name[i])
  #   
  #   tau_10_summary$intercept[i] = summary(brood_rqfit[[i]])$coefficients[1,1]
  #   tau_10_summary$Lat[i] = summary(brood_rqfit[[i]])$coefficients[2,1]
  #   tau_10_summary$brood_name[i] = brood_name[i]
  #   tau_10_summary$n[i] = nrow(cicada_points[cicada_points$BROOD_NAME == brood_name[i],])
  #   
  #   
  # } #ah, these are so all over the place. I think I'd rather either use the model fit accross all broods or use soil temperature to determine emergence and then double check those make sense by comparing with the like, earliest 5% of emergence data.
