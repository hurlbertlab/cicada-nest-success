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



# Ok so, I want to create some sort of line graph where line for each species of interest and x axis is cicada year and y axis is survial rate (% of young fledged) 

#create % young fledged variable
nestboxes_county_cicada <- nestboxes_county_cicada %>%
  mutate(pct_fledged = ifelse(Young.Total == 0, NA, Young.Fledged / Young.Total)) %>%
  dplyr::relocate(pct_fledged, .after = Outcome)

# graph mean and standard deviation of pct fledged for each species over cicada year 
library(ggplot2)

## group, calc mean & stdev
summary_data <- nestboxes_county_cicada %>%
  group_by(Species.Name, cicada_year) %>%
  summarise(
    mean_pct_survival = mean(pct_fledged, na.rm = TRUE),
    se_pct_survival = sd(pct_fledged, na.rm = TRUE) / sqrt(n())
  )
summary_data <- summary_data %>%
  filter(cicada_year >= -4 & cicada_year <= 8)

## ok now graph
ggplot(summary_data, aes(x = cicada_year, y = mean_pct_survival, color = Species.Name)) +
  geom_line() +
  geom_errorbar(aes(ymin = mean_pct_survival - se_pct_survival, ymax = mean_pct_survival + se_pct_survival), width = 0.2) +
  facet_wrap(~ Species.Name, ncol = 2) +  # Create separate plots for each species
  labs(
    x = "Cicada Year",
    y = "Mean Percent Survival",
    color = "Species"
  ) +
  theme_minimal() +
  # Add arrow at cicada_year = 0
  annotate(
    "segment",
    x = 0, xend = 0,  # Arrow is vertical at cicada_year = 0
    y = max(filtered_data$mean_pct_survival), yend = max(filtered_data$mean_pct_survival) - 0.2 * diff(range(filtered_data$mean_pct_survival)),  # Arrow points downward
    arrow = arrow(type = "open", length = unit(0.15, "inches")),  # Shorter arrow
    color = "black",  # Arrow color
    size = 1  # Arrow thickness
  )
# yay! graph! 


