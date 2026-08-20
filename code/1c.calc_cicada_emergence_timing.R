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

cicada_obs <- read.csv("data/cicada/inaturalist/inat_magicicada_observations.csv")

#make points on a map

#read in the brood map

#connect with brood map + filter to overlaps in the correct years for each brood.
