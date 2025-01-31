#--------------------------
# Filter snakes to 2019+ and find out if a spatial pattern
# exists 
#--------------------------

#libraries
library(dplyr)
library(sf)
library(assertthat)

#load in snake data, this has already been filtered to only cicada counties
snakes_year_county <- read.csv("copperheads/data/snakes/snakes_county_year.csv") %>% 
  filter(year > 2018) %>% #just 2019 onwards, once we've got good iNat adoption rates.
  mutate(emergence_year = NA,
         BROOD_NAME = NA,
         cycle = NA)
#m. yeah need some way to add if it's been an emergence year or not.@
#hm, okay not really jamming away at this today though, I think I'm going to go grade student papers for the afternoon

#load in cicada and county data
cicada <- st_read(dsn = "copperheads/data/cicada/periodical_cicada_with_county.gdb")

emergence_years <- read.csv("copperheads/data/cicada/cicada_emergence_years.csv")

#get the unique broods
unique_broods <- unique(cicada$BROOD_NAME)

c <- data.frame()

#not sure why it works down below and NOT here within this for loop. hm :/
for(i in 1:length(unique_broods)) {
  #filter cicada to get the counties it happens in
  ibroodcounties <- cicada %>% 
    filter(BROOD_NAME == unique_broods[i]) 
  ibroodcounties <- unique(ibroodcounties$ST_CNTY_CODE)
  
  #filter emergence based on the broods to get the years it happens it
  iemergyear <- emergence_years %>%
    filter(years_emerged_brood == unique_broods[i])
  icycle <- unique(iemergyear$cycle)
  assertthat::assert_that(length(icycle) == 1)
  
  #based on county+year, add if it was an emergence year or not and what brood it was.
  snakes_year_county <- snakes_year_county %>%
    mutate(
      
      BROOD_NAME = 
        case_when(
          ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear ~ unique_broods[i]),
      
      emergence_year = case_when(
        ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear ~ 1
      ),
      
      cycle = case_when(
        ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear ~  icycle
      )
    )
  
  
  
}

#filter cicada to get the counties it happens in
ibroodcounties <- cicada %>% 
  filter(BROOD_NAME == unique_broods[i]) 
ibroodcounties <- unique(ibroodcounties$ST_CNTY_CODE)

#filter emergence based on the broods to get the years it happens it
iemergyear <- emergence_years %>%
  filter(years_emerged_brood == unique_broods[i])
icycle <- unique(iemergyear$cycle)
assertthat::assert_that(length(icycle) == 1)
iemergyear <- unique(iemergyear$emergence_year)

#based on county+year, add if it was an emergence year or not and what brood it was.

snakes_year_county <- snakes_year_county %>%
  mutate(
    
    BROOD_NAME = 
      case_when(
    ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear ~ unique_broods[i]),
    
    emergence_year = case_when(
      ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear ~ 1
    ),
    
    cycle = case_when(
      ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear ~  icycle
    )
    
    )

#ah, you know, I suppose it doesn't even matter WHAT brood it is, in the 
#cases where multiple broods overlap. The important thing is that there is a brood
#not which brood specifically it is? Unless like, I want to look across a brood's range
#and look at broods independently?
#better mutate and then filter. And then bind-rows. I can make a dataset where I remove
#duplicate ST_CNTY_CODE years and otherwise then I can filter to each brood's counties
#and...ask..that question...but even THEN it matters when there's a brood. This analysis
#should look at either each COUNTY and ask if copperhead %s are higher in emergence years,
#OR, ACROSS A BROOD AREA, are those counties higher in emergence years. In which case again, why seperate out by which brood it's happening in? shouldn't matter.



snakes_year_county %>%
  filter(
    ST_CNTY_CODE %in% ibroodcounties & year %in% iemergyear
  )


#if not NA, then duplicate the row and fill in with new information for the latest brood

c <- snakes_year_county %>% 
  left_join(emergence_years, by = c("year" = "emergence_year"))

#no, filter to brood and then add true when years is %in% list of years for that brood.
