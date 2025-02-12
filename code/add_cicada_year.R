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
  mutate(em1_cicada_year = NA) %>%
  mutate(em2_cicada_year = NA) %>% #add new columns that we will fill in
  dplyr::select (-emergence_two) %>% # get rid of emergence Years that we dont have nest data for 
  dplyr::relocate(Year, .before = emergence_three)

# okay so the idea is create cicada year values for both emergence 1 and 2 and then use the minimum absolute value between the two to create a standardized cicada year value. 

nestboxes_county_cicada <- nestboxes_county_cicada %>%
  mutate(
    em1_cicada_year = case_when(
      Year == emergence_three ~ 0,
      FALSE ~ em1_cicada_year
    ),
    em2_cicada_year = case_when(
      Year == emergence_four ~ 0,
      FALSE ~ em2_cicada_year
    )
  )
# I think that worked ? So now each emergence Year is either 0 or NA, for both emergence 1 and 2... 
# So now, fill in NA with some integer that indicates how far away it is from an emergence year 
# em1_cicada_year == Year - emergence_three

nestboxes_county_cicada <- nestboxes_county_cicada %>% 
  mutate(em1_cicada_year = ifelse(is.na(em1_cicada_year), Year - emergence_three, em1_cicada_year),
         em2_cicada_year = ifelse(is.na(em2_cicada_year), Year - emergence_four, em2_cicada_year)) 

# Freaking awesome !! I think that worked !!
# ok so now take the minimum of the absolute values of those two columns and that is our cicada year 


nestboxes_county_cicada <- nestboxes_county_cicada %>%
  rowwise() %>%
  mutate(cicada_year = ifelse(
    abs(em1_cicada_year) <= abs(em2_cicada_year),  # Condition: Compare absolute values
    em1_cicada_year,                               # If TRUE, use em1_cicada_year
    em2_cicada_year                                # If FALSE, use em2_cicada_year
  ))



# hooray ! cicada year ! 
