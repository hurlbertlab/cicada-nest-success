#########################################
#
#
# Cicadas may affect nesting birds in three ways: 
# (i) percent of nests that succeed or fail, 
# (ii) getting a little more specific to success or failure
#     a) keep all observations and code full failure as 0% fledge and then calculate % fledge for everything else. Always based on the number HATCHED not the number of eggs laid. 
#
# This script creates datasets and runs these models.
#
#
##########################################

library(dplyr)
library(tidyverse)
library(assertthat)
library(statuser) # table2
library(stringr) # text selection
library(lubridate) # handling dates
library(ggplot2) # plotting
library(png) #for the cicada image

# Read in location.ID climate data
climate_data<- read.csv("data/filtered_climate_data.csv")

# Dataframe with nest success info and cicada emergence info for nestboxes that are in counties with cicadas
analysis_df <- read.csv("data/nestboxes_w_county+cicada.csv",
                                    sep = ",",
                                    header = TRUE, 
                                    stringsAsFactors = FALSE,
                                    row.names = NULL) %>%
  mutate(em1_cicada_year = NA) %>%
  mutate(em2_cicada_year = NA) %>% #add new columns that we will fill in
  dplyr::relocate(Year, .before = emergence_three) %>%
  mutate(
    em1_cicada_year = case_when(
      Year == emergence_three ~ 0,
      FALSE ~ em1_cicada_year
    ),
    em2_cicada_year = case_when(
      Year == emergence_four ~ 0,
      FALSE ~ em2_cicada_year
    )
  ) %>%
  mutate(em1_cicada_year = ifelse(is.na(em1_cicada_year), Year - emergence_three, em1_cicada_year),
         em2_cicada_year = ifelse(is.na(em2_cicada_year), Year - emergence_four, em2_cicada_year)) %>%
  rowwise() %>%
  # Create CicadaYear Column 
  mutate(cicada_year = ifelse(
    abs(em1_cicada_year) <= abs(em2_cicada_year),  # Condition: Compare absolute values
    em1_cicada_year,                               # If TRUE, use em1_cicada_year
    em2_cicada_year                                # If FALSE, use em2_cicada_year
  )) %>%
  ungroup() %>%
  #add in the climate data
  left_join(climate_data, by = c("Location.ID","Year")) %>%
  #filter out data if it's one of the few cases we failed to collect climate data
  #before = 272,698 rows
  filter(!is.na(y_anomaly_temp)) |>
  #after = 229,891 rows
  #able to get climate data for 84.3% of nests. Pretty great recovery amount. Discussed this in previous years working on this project, not too worried about recovering the other data. Have every reason to assume this error is randomly distributed.
  #I do treat a few locations differently because 2025 is missing about 1k locations. If those nests weren't laid IN 2025, then they still get included here and their y_anomaly_temp is based off 45 years instead of 46 years of climate data.
  #and I should also filter out those nestbox entries that occurred in non-cicada counties. 
  #If it's a nest that never would have experienced a cicada emergence
  #then the survival of those birds are not relevant to this experiment
  filter(!is.na(cicada_year),
         MULT_BROOD <= 2 #filter out multiple broods over 2
         ) |>
  #after = 150,765 nests.
  #As well, previous analyses looked multiple years in the past + future of a cicada emergence. But, there are a lot of things that could vary between years. I want to try and control for the effect of cicada emergence as much as possible. Therefore, just going to look at the year before, year of, and year after a cicada emergence
  filter(cicada_year %in% c(-1, 0, 1)) |>
  #after = 30,805 observations 
  #HM. And actually for the two-brood records we don't want to remove these completely, it's just that we want to keep only one record. Let's use distinct() but for the most part I've already handled it. Either it was a year of a cicada emergence, a year earlier, or a year after. Most multiple broods are not overlapping in years as well. And for those multiple broods that do overlap in years in some counties, we're solid b/c they're just duplicates of each other anyway.
  group_by(Attempt.ID, Location.ID, Year, Hatch.Date) |>
  distinct(Attempt.ID, Location.ID, Year, Hatch.Date, .keep_all = TRUE) |>
  ungroup() |> 
  # after = 30,764 observations
  mutate(cicada_year_binary = case_when(
    cicada_year == 0 ~ 0, #0 when cicadas have emerged
    TRUE ~ 1
  ),
  pre_emergence = case_when(
    cicada_year == -1 ~ 1,
    cicada_year == 0 ~ 0, #when cicadas have emerged
    cicada_year == 1 ~ NA
  ),
  post_emergence = case_when(
    cicada_year == -1 ~ NA,
    cicada_year == 0 ~ 0, #when cicadas have emerged
    cicada_year == 1 ~ 1
  )) |>
  #make our other variables of interest: pct_fledged and time_to_fledge
  mutate(pct_fledged = ifelse(Young.Total == 0, NA, Young.Fledged / Young.Total),
         time_to_fledge = (lubridate::interval(ymd(Hatch.Date), ymd(Fledge.Date)))/days()
  ) |>
  dplyr::relocate(pct_fledged:time_to_fledge, .after = Outcome) |>
  # and we only need to keep records that have at least one of our outcome variables
  filter(!is.na(pct_fledged) | !is.na(time_to_fledge)) |>
  #and remove unexpected outcomes (time to fledge is probably still too high but we can deal with that later at that analysis.)
  filter(time_to_fledge > 0 & time_to_fledge < 50 | is.na(time_to_fledge),
         pct_fledged >= 0 & pct_fledged <= 1 | is.na(pct_fledged)) |>
  #after = 25,346 records
  #make chickadees the same species.name, ~80 Black-capped observations per cicada_year.
  mutate(Species.Name = case_when(
    str_detect(Species.Name, "Chickadee") ~ "Black-capped and\n Carolina Chickadee",
    TRUE ~ Species.Name
  ))  |>
  # select just the info we need, don't need all 46 variables.
  dplyr::select(Attempt.ID, Location.ID, Species.Name, First.Lay.Date:time_to_fledge, BROOD_NAME, MULT_BROOD, ST_CNTY_CODE, cycle, Year, emergence_three, emergence_four, cicada_year:post_emergence) 

statuser::table2(analysis_df$cicada_year,
                 analysis_df$Species.Name)
#well, we probably won't be very confident with prothonotary warbler conclusions but we can include, we've still got 30ish data points a category.
#The same widespread, ubiquitous, insectivores we filtered to the first time (Chickadees, Eastern Blueblue, House Wren, House Sparrow, and Tree Swallow) are the best represented now as well.
#Chickadees are lowest at ~350 obs per cicada_year (-1, 0, 1)
#Purple Martin, Carolina Wren, and American Robin which we did not keep before all are only about 100 nests in each cicada_year. So we would be much less confident in the effects on those birds. But, an experimental n of 100 isn't nothing! Better to include.


# graph mean and standard deviation of pct fledged for each species over cicada year 
## group, calc mean & stdev
summary_data <- analysis_df %>%
  group_by(Species.Name, cicada_year) %>%
  summarise(
    mean_pct_survival = mean(pct_fledged, na.rm = TRUE),
    se_pct_survival = sd(pct_fledged, na.rm = TRUE) / sqrt(n()),
    n = n()
  ) |>
  ungroup() |>
  arrange(desc((n)))
# Get the original order of species
original_order <- unique(summary_data$Species.Name)

cicada_image = readPNG("figures/cicada_outline.png")

## ok now graph
ggplot(summary_data, aes(x = cicada_year, y = mean_pct_survival, color = Species.Name)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_pct_survival - se_pct_survival, ymax = mean_pct_survival + se_pct_survival), width = 0.2) +
  facet_wrap(~ reorder(Species.Name, n, decreasing = TRUE), ncol = 3) +  # Create separate plots for each species, 3 columns. Now, would like the colors to still go in typical ggplot order, but that's okay. Probably I will need to re-do this by hand to make that happen.
  labs(
    x = "Cicada Year",
    y = "Mean Percent Survival"
  ) +
  scale_x_continuous(breaks = c(-1, 0, 1),  # Numeric breaks at -1, 0, and 1
                    labels = c("-1", "X", "1")) +  # Custom text labels) 
  scale_color_discrete(limits = original_order) +  # Fix color order to original
  theme_minimal() +
  theme(
    legend.position = "none", # remove legend
    #panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
    ) #+
  #annotation_raster(cicada_image, xmin = -0.2, xmax = 0.2, ymin = 0, ymax = 0.2) #hm, guess that didn't work. Will need to test.
  #make all text bolder etc.


#okay in the test for the effects, make a loop that goes through species names or smthing. Would love to do it in dplyr :D. But anyway! Run two models. Filter for postcicada=0 to run the %survival ~ pre-cicada + temp + etc. and filter for precicada=0 to run the %survival ~ post-cicada + temp + etc.
