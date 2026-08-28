#####################
#
# File to calculate cicada emergence timing. 
#
# (1)
# Take the inaturalist magicicada observations
# Based on the map of cicada emergences + year, 
# decided not to group observations into broods b/c
# they all follow the same general trend line.
# (but it is important to include year AND brood boundaries b/c a few individuals)
# (will emerge a year early / year late relative to the bulk of the brood)
# (and we don't want those included in our timing estimates)
# Quantile regression of latitude vs observation date
# examine those graphs and the fit of those lines
# If fit is good, pick the regression quantile (5%? 10%?) that best captures the emergence
# and use those lines to say for lat x. cicadas were present day y - z. 
# In analysis can then use that to generate predicted values for each
# nestwatch nest of cicada asynchrony.
#
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
library(ggplot2) #for plotting

#read in the brood map
brood_map <- st_read(dsn = "data/cicada/periodical_cicada_with_county.gdb")
#plot(brood_map$SHAPE)
#cicada emergence data
cicada_emergence_years <- read.csv("data/cicada/cicada_emergence_years_wide.csv")  %>% ## this has broods with 4 emergence years in 4 separate columns
  dplyr::select(-emergence_2019_through_2024)
#nymph cicada
nymphs <- read.csv("data/cicada/inaturalist/2.inat_NYMPH_magicicada.csv") |>
  dplyr::select(id, uuid)
#dead cicada
dead <- read.csv("data/cicada/inaturalist/2.inat_DEAD_magicicada.csv") |>
  dplyr::select(id, uuid)
#molts
molts <- read.csv("data/cicada/inaturalist/2.inat_MOLT_magicicada.csv") |>
  dplyr::select(id) |>
  #remove obs from molts that also had live cicadas in the observation
  anti_join(
    (
      read.csv("data/cicada/inaturalist/2.inat_ORGANISM_magicicada.csv") |>
        dplyr::select(id)
     )
  , by = "id") #mwahaha. inat doesn't support the without_term_value_id filter in exporting but I am unstoppable.

# read in inaturalist cicada observations
cicada_obs <- read.csv("data/cicada/inaturalist/1.inat_magicicada_observations.csv") |>
  #filter out nymph cicada (not yet available to birds b/c mostly in the ground), dead cicada, and molts where the living cicada is not also in the photo
  dplyr::filter(!id %in% nymphs$id) |>
  dplyr::filter(!id %in% dead$id) |> #91 observations that were tagged as both dead and nymph
  dplyr::filter(!id %in% molts$id) |>
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
  #and we have some duplicated observations joined b/c of overlap with multiple broods. We went from 45805 observations to 58662 after the join. No bueno! And we don't want any straggler cicadas. So we want to only keep observations where the year of the observation matches the emergence_three or emergence_four of the cicada brood in that county. 
  filter(year == emergence_three | year == emergence_four) 
  #perf. after == 36166 observations
  
  #and unless we end up needing it later, let's drop the geometry for now
cicada_points <- cicada_geom |>
  st_drop_geometry()

#data exploration
table2(cicada_points$year) 
table2(cicada_points$BROOD_NAME) #alllright, yup there's a couple broods that are not well represented at all. Do those broods also show up in the nestwatch data?
#ya, went to check in 2.analysis_cicada_year.R with table2(analysis_df$BROOD_NAME) and indeed all the broods are represented (cool for me. )
#guess its a question of if that latitude:observation date relationship is really actually different for different broods or if they're all along the same line.
#aaaah.. hm... b/c it doesn't matter what all the dates of observation are of the cicadas. What matters is my estimated emergence date for each brood. Hence, the group by then quantile regression
ggplot(cicada_points, 
       aes(x = Lat,
           y = j_date, 
           color = BROOD_NAME)) + 
  geom_point() +
  theme_minimal()
plot(y = cicada_points$j_date,
     x = cicada_points$Lat)
q_glm <- lm(j_date ~ Lat, data = cicada_points)
summary(q_glm) #okay well yup, latitude of course is related to the day of observation. Anyway, once more what matters is getting that quantile emergence date.
# https://www.r-bloggers.com/2019/01/quantile-regression-in-r-2/
#test_quant <- cicada_points |>
#  filter(BROOD_NAME == "Brood XIII")

#set quartiles
min_quartile = 0.025
max_quartile = 0.975

min_bound <- rq(j_date ~ Lat, 
            tau = min_quartile, 
            data = cicada_points)
summary(min_bound)
#this is fit across broods. 

max_bound <- rq(j_date ~ Lat, 
                tau = max_quartile, #tau = what quantile I want, this is the earliest 5%, which should capture the start of emergences
                data = cicada_points)
summary(max_bound) #awesome, very similar line as the minimum bound but with a different intercept. 

#make polygon
{
  min_coef <- coef(min_bound)
  max_coef <- coef(max_bound)
  sorted_data <- cicada_points[order(cicada_points$Lat),]
  polygon_x <- c(28, sorted_data$Lat, 45)
  polygon_ymin <- min_coef[1] + min_coef[2]*polygon_x
  polygon_ymax <- max_coef[1] + max_coef[2]*polygon_x
}

text_size = 1.3
png(filename = "figures/2026.08.27_obs_by_lat.png", 
    width = 650,
    height = 400,
    units = "px", 
    type = "windows")
{
plot(j_date ~ Lat, data = cicada_points, pch = 1,
     main = "Cicada Observation Date by Latitude",
     xlab = "Latitude",
     ylab = "Julien Day",
     cex.main = text_size,
     cex.sub = text_size, 
     cex.lab = text_size, 
     cex.axis = text_size)
abline(coef(min_bound), lty = "dashed", lwd = 4, col = "#619CFF")
abline(coef(max_bound), lty = "dashed", lwd = 4, col = "#619CFF")
polygon(c(polygon_x, rev(polygon_x)),
        c(polygon_ymax, rev(polygon_ymin)),
        col = adjustcolor("#619CFF", alpha.f = 0.3),
        border = NA
        )
} 
dev.off()
# Okay, now export the rqfit so we can load it in in the analysis df and use predict()
cicada_bounds_latitude_by_j_date <- data.frame(
                                               bound = c("min", "max"),
                                               intercept = c(min_coef[1], max_coef[1]),
                                               lat = c(min_coef[2], max_coef[2]),
                                               quartile = c(min_quartile, max_quartile)
                                               )
write.csv(cicada_bounds_latitude_by_j_date, "data/cicada_bounds_latitude_by_j_date.csv", row.names = FALSE)

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
