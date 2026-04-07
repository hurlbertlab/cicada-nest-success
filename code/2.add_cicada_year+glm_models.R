###########################
#
# Create a column of the nest was laid during a cicada year
# and run the glm models
#
#
##########################

library(dplyr)
library(tidyverse)
library(assertthat)
library(statuser)
library(stringr)

# Read in location.ID climate data
climate_data<- read.csv("data/filtered_climate_data.csv")

# Dataframe with nest success info and cicada emergence info for nestboxes that are in counties with cicadas
nestboxes_county_cicada <- read.csv("data/nestboxes_w_county+cicada.csv",
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
  mutate(pct_fledged = ifelse(Young.Total == 0, NA, Young.Fledged / Young.Total)) %>%
  dplyr::relocate(pct_fledged, .after = Outcome)%>%
  ungroup() %>%
  #let's filter out nests that didn't have anything happen 
  filter(!Outcome %in% c("u1", #unknown outcome
                         "u2", #nest monitoring stopped prior to expected fledge date while nest was still active
                         "u3", #no breeding behavior observed
                         "i", #inactive
                         "n" #not monitored
                         )) %>%
  #add in the climate data
  left_join(climate_data, by = c("Location.ID","Year")) %>%
  #filter out data if it's one of the few cases we failed to collect climate data
  #before = 317004 rows
  filter(!is.na(y_anomaly_temp)) |>
  #after = 273770 rows
  #able to get climate data for 86.36% of nests. Pretty great recovery amount. Discussed this in previous years working on this project, not too worried about recovering the other 14% of data. Have every reason to assume this error is randomly distributed.
  #and I should also filter out those nestbox entries that occurred in non-cicada counties. 
  #If it's a nest that never would have experienced a cicada emergence
  #then the survival of those birds are not relevant to this experiment
  filter(!is.na(cicada_year)) |>
  #after = 181,785 nests.
  #As well, previous analyses looked multiple years in the past + future of a cicada emergence. But, there are a lot of things that could vary between years. I want to try and control for the effect of ccada emergence as much as possible. Therefore, just going to look at the year before, year of, and year after a cicada emergence
  filter(cicada_year %in% c(-1, 0, 1))
  #after = 36,585 observations.

  statuser::table2(nestboxes_county_cicada$cicada_year,
                   nestboxes_county_cicada$Species.Name)
  #yup, here's where some of the filtering of species that we didn't do before becomes relevant.
  #Several of these species just don't have enough nests to be confident in our ..conclusions...
  #well. Hm. American Kestrel is dicey at under a 100 nests, I'm testing the effect of multiple variables
  #but Purple Martin? Might actually be O.K. at about 170 nests.
  
analysis_df <- nestboxes_county_cicada %>%
  #remove Kestrel
  filter(!Species.Name %in% c("American Kestrel")) |>
  mutate(Species.Name = case_when(
    str_detect(Species.Name, "Chickadee") ~ "Black-capped and Carolina Chickadee",
    TRUE ~ Species.Name
  )) |>
  dplyr::select(Attempt.ID, Location.ID, Species.Name, First.Lay.Date:pct_fledged, BROOD_NAME, MULT_BROOD, cycle, Year, emergence_three, emergence_four, cicada_year:y_anomaly_precip)

table(analysis_df$MULT_BROOD)
  #hey, am going to need to check for duplications because of multiple brood years.
  #just remove 3 and 4
  #and with 2brood counties remove any counties where the emergence years are not the same for the two broods.
  #something different is going on in those locations.

statuser::table2(analysis_df$cicada_year,
                 analysis_df$Species.Name)
#okay yes, the same widespread, ubiquitous, insectivores we filtered to the first time (Chickadees, Eastern Blueblue, House Wren, House Sparrow, and Tree Swallow) are the best represented now as well.
#Chickadees are lowest at ~400 obs per cicada_year (-1, 0, 1)
#Purple Martin, Carolina Wren, and American Robin which we did not keep before all are only about 200 nests in each cicada_year. So we would be much less confident in the effects on those birds. But, an experimental n of 200 isn't nothing! Discuss with Allen.


# Ok so, I want to create some sort of line graph where line for each species of interest and x axis is cicada year and y axis is survial rate (% of young fledged) 

#create % young fledged variable

# graph mean and standard deviation of pct fledged for each species over cicada year 
library(ggplot2)

## group, calc mean & stdev
summary_data <- nestboxes_county_cicada %>%
  group_by(Species.Name, cicada_year) %>%
  summarise(
    mean_pct_survival = mean(pct_fledged, na.rm = TRUE),
    se_pct_survival = sd(pct_fledged, na.rm = TRUE) / sqrt(n())
  )%>%
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
    y = max(summary_data$mean_pct_survival), yend = max(summary_data$mean_pct_survival) - 0.2 * diff(range(summary_data$mean_pct_survival)),  # Arrow points downward
    arrow = arrow(type = "open", length = unit(0.15, "inches")),  # Shorter arrow
    color = "black",  # Arrow color
    size = 1  # Arrow thickness
  )
# yay! graph! 

# Ok now I want to do the same thing but with mean probability of nest failure instead of pct survival rate
# so it will be something like # of nests with 0% survival/total nest obs
summary_data2 <- nestboxes_county_cicada %>%
  group_by(Species.Name, cicada_year) %>%
  summarise(
    total_nests = n(),
    failed_nests = sum(pct_fledged == 0, na.rm=TRUE),
    p_nest_failure = (failed_nests / total_nests),
  )%>%
  filter(cicada_year >= -4 & cicada_year <= 8)


# graph 
ggplot(summary_data2, aes(x = cicada_year, y = p_nest_failure, color = Species.Name)) +
  geom_line() +
  facet_wrap(~ Species.Name, ncol = 2) +  # Create separate plots for each species
  labs(
    x = "Cicada Year",
    y = "Probability of Nest Failure",
    color = "Species"
  ) +
  theme_minimal() +
  # Add arrow at cicada_year = 0
  annotate(
    "segment",
    x = 0, xend = 0,  # Arrow is vertical at cicada_year = 0
    y = max(summary_data2$p_nest_failure), yend = max(summary_data2$p_nest_failure) - 0.1 * diff(range(summary_data2$p_nest_failure)),  # Arrow points downward
    arrow = arrow(type = "open", length = unit(0.15, "inches")),  # Shorter arrow
    color = "black",  # Arrow color
    size = 1  # Arrow thickness
  )
# Yay! graph pt.2!!


# Intitial stat Models

# ifelse(cicada_year == 0,1,0) # either is cicada year or not 
# foo$successfulnest = ifelse(foopct_fledged> 0,1,0)
#fledge = glm(formula = successfulnest ~ cicada + Species.Name + cicada*Species.Name, data = foo, family = binomial(link = "logit")) 
# run 5 dif glms, one for each species 
#eablfledge = glm(formula = successfulnest ~ cicada, data = foo[foos$Species.Name == "Eastern Bluebird"], family = binomial(link = "logit")) 

nestboxes_county_cicada$successfulnest = ifelse(nestboxes_county_cicada$pct_fledged>0,1,0)
nestboxes_county_cicada$cicada = ifelse(nestboxes_county_cicada$cicada_year!=0,1,0)

# general
fledge = glm(formula = successfulnest ~ cicada + Species.Name +cicada*Species.Name, data = nestboxes_county_cicada, family = binomial(link = "logit"))

# eastern bluebird
eablfledge = glm(formula = successfulnest ~ cicada, 
                  data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Eastern Bluebird", ], 
                  family = binomial(link = "logit"))
# Tree Swallow
trswfledge = glm(formula = successfulnest ~ cicada, 
                  data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Tree Swallow", ], 
                  family = binomial(link = "logit"))

# House Wren
howrfledge = glm(formula = successfulnest ~ cicada, 
                 data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "House Wren", ], 
                 family = binomial(link = "logit"))
# Carolina Chickadee
cachfledge = glm(formula = successfulnest ~ cicada, 
                 data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Carolina Chickadee", ], 
                 family = binomial(link = "logit"))
# Black Capped Chickadee
bcchfledge = glm(formula = successfulnest ~ cicada, 
                 data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Black-capped Chickadee", ], 
                 family = binomial(link = "logit"))

###########################
#Models with Climate
###########################


eablfledge.climate = glm(formula = successfulnest ~ cicada + y_anomaly_temp + y_anomaly_precip, 
                 data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Eastern Bluebird", ], 
                 family = binomial(link = "logit"))

trswfledge.climate = glm(formula = successfulnest ~ cicada + y_anomaly_temp + y_anomaly_precip, 
                         data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Tree Swallow", ], 
                         family = binomial(link = "logit"))
howrfledge.climate = glm(formula = successfulnest ~ cicada + y_anomaly_temp + y_anomaly_precip, 
                         data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "House Wren", ], 
                         family = binomial(link = "logit"))
cachfledge.climate = glm(formula = successfulnest ~ cicada + y_anomaly_temp + y_anomaly_precip, 
                         data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Carolina Chickadee", ], 
                         family = binomial(link = "logit"))
bcchfledge.climate = glm(formula = successfulnest ~ cicada + y_anomaly_temp + y_anomaly_precip, 
                         data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Black-capped Chickadee", ], 
                         family = binomial(link = "logit"))

 
# Getting predicted values from your models and then reverse logit transforming

# E.g.
eabl.climate.pred = predict(eablfledge.climate, nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Eastern Bluebird", c('cicada', 'y_anomaly_temp', 'y_anomaly_precip')])

# Reverse logit transform to get predicted values of success in terms of a probability
prob.predictions <- 1 / (1 + exp(-eabl.climate.pred))

# Visualing those in a histogram
hist(prob.predictions)
