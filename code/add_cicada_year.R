# Create CicadaYear Column 

library(dplyr)
library(tidyverse)
library(assertthat)


# Dataframe with nest success info and cicada emergence info for nestboxes that are in counties with cicadas

nestboxes_county_cicada <- read.csv("data/nestboxes_county_cicada.csv",
                                    sep = ",",
                                    header = TRUE, 
                                    stringsAsFactors = FALSE,
                                    row.names = NULL) %>%
  mutate(cicada_year = NA) %>% #add new column that we will fill in
  dplyr::select (-emergence_two) # get rid of emergence Years that we dont have nest data for 

nestboxes_county_cicada <- nestboxes_county_cicada %>%
  mutate(
    cicada_year = case_when(
      Year %in% emergence_three | Year %in% emergence_four ~ 1,
      .default = cicada_year
    )
  )

# I think that worked ? So now emergence Year is either 1 or NA... 
# and what we want at the end is a -3:+3, where emergence Year is actually a 0
nestboxes_county_cicada <- nestboxes_county_cicada %>%
  mutate(cicada_year = case_when(cicada_year == 1 ~ 0,
                                  .default = cicada_year))
# now i have 0 and NA

# From Ivara's Code


#so you have nestboxes used multiple Years, and a longer time frame, so some counties will have multiple emergence Years. That's not a problem. 
#so....
#need a df of every county associated with ALL the broods that might emerge
#like every row is a county + brood + that brood's emergence Years..
#but there might be more than one row for every county bc there could be more than one brood.
#   that has it's own multiple emergence Years
#once you have that..... you can sort each county for the closest date emergence Year to each row?
# and use that?

#what if we don't add it to bluebirds directly.
#and the first df we make, takes each county, and has a row for each brood with associated Years.
#THEN all we need
new_nests_year_county <- data.frame()

temp_nbroods <- nestboxes_county_cicada %>%
  group_by(ST_CNTY_CODE) %>%
  filter(!is.na(BROOD_NAME)) %>%
  summarize(n_broods = length(unique(BROOD_NAME)))

for(a in 1:nrow(temp_nbroods)) {
  
  temp_syc <- nestboxes_county_cicada %>% 
    filter(ST_CNTY_CODE == temp_nbroods$ST_CNTY_CODE[a])
  
  if(temp_nbroods$n_broods[a] == 1) {
    
    yr_emergence <- max(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    #the max doesn't mean anything, bc there's only one emergence Year. Just removing the NAs
    
    temp_syc <- temp_syc %>%
      mutate(cicada_year = Year - yr_emergence)
    
  } else if(temp_nbroods$n_broods[a] == 2) {
    
    #get the latest emergence Year
    yr_emergence_one <- max(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    #get the earliest emergence Year
    yr_emergence_two <- min(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE) 
    #create two cicada_years, take the min, don't replace if emergence Year is a 0
    temp_syc <- temp_syc %>%
      mutate(emone = Year - yr_emergence_one,
             emtwo = Year - yr_emergence_two,
             cicada_year = case_when(
               cicada_year == 0 ~ cicada_year, #keep the same
               cicada_year != 0 ~ pmin(emone, emtwo, na.rm = TRUE)
             )) %>%
      dplyr::select(-emone, -emtwo)
    
  } else if(temp_nbroods$n_broods[a] == 3) {
    
    #get the three emergence Years
    yr_emergence_one <- max(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    yr_emergence_two <- median(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    yr_emergence_three <- min(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    
    #create three cicada_years, take the min, don't replace if emergence Year is a 0
    temp_syc <- temp_syc %>%
      mutate(emone = Year - yr_emergence_one,
             emtwo = Year - yr_emergence_two,
             emthree = Year - yr_emergence_three,
             cicada_year = case_when(
               cicada_year == 0 ~ cicada_year, #keep the same
               cicada_year != 0 ~ pmin(emone, emtwo, emthree, na.rm = TRUE)
             )) %>%
      dplyr::select(-emone, -emtwo, -emthree)
    
  } else if(temp_nbroods$n_broods[a] == 4) {
    
    #get the four emergence Years
    yr_emergence_one <- max(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    yr_emergence_two <- sort(temp_syc$Year[temp_syc$cicada_year == 0], decreasing = TRUE)[2]
    yr_emergence_three <- sort(temp_syc$Year[temp_syc$cicada_year == 0], decreasing = TRUE)[3]
    yr_emergence_four <- min(temp_syc$Year[temp_syc$cicada_year == 0], na.rm = TRUE)
    
    #create four cicada_years, take the min, don't replace if emergence Year is a 0
    temp_syc <- temp_syc %>%
      mutate(emone = Year - yr_emergence_one,
             emtwo = Year - yr_emergence_two,
             emthree = Year - yr_emergence_three,
             emfour = Year - yr_emergence_four,
             cicada_year = case_when(
               cicada_year == 0 ~ cicada_year, #keep the same
               cicada_year != 0 ~ pmin(emone, emtwo, emthree, emfour, na.rm = TRUE)
             )) %>%
      dplyr::select(-emone, -emtwo, -emthree, -emfour)
    
  } else {
    #error out
    assertthat::assert_that(1 == 2, msg = "There's a brood with more than 4 BROOD_NAMES or an unexpected number of broods")
  }
  new_nests_year_county <- bind_rows(new_nests_year_county, temp_syc)
}


## Okay, I'm getting close but not quite there yet 

