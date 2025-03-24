# Create CicadaYear Column 

library(dplyr)
library(tidyverse)
library(assertthat)


# Dataframe with nest success info and cicada emergence info for nestboxes that are in counties with cicadas
climate_data<- read.csv("data/daymet_data.csv") %>%
  rename(Location.ID = site) %>%
  rename(Year = year) %>%
  rename(y_temp = mean_temp) %>%
  rename(y_precip = mean_precip) %>%
  group_by(Location.ID) %>%
  mutate(mean_temp = mean(y_temp))%>%
  mutate(mean_precip = mean(y_precip)) %>%
  mutate(y_anomaly_temp = y_temp-mean_temp) %>%
  mutate(y_anomaly_precip = y_precip-mean_precip)

nestboxes_county_cicada <- read.csv("data/nestboxes_county_cicada.csv",
                                    sep = ",",
                                    header = TRUE, 
                                    stringsAsFactors = FALSE,
                                    row.names = NULL) %>%
  mutate(em1_cicada_year = NA) %>%
  mutate(em2_cicada_year = NA) %>% #add new columns that we will fill in
  dplyr::select (-emergence_two) %>% # get rid of emergence Years that we dont have nest data for 
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
  mutate(cicada_year = ifelse(
    abs(em1_cicada_year) <= abs(em2_cicada_year),  # Condition: Compare absolute values
    em1_cicada_year,                               # If TRUE, use em1_cicada_year
    em2_cicada_year                                # If FALSE, use em2_cicada_year
  )) %>%
  mutate(pct_fledged = ifelse(Young.Total == 0, NA, Young.Fledged / Young.Total)) %>%
  dplyr::relocate(pct_fledged, .after = Outcome)%>%
  ungroup() %>%
  left_join(climate_data, by = c("Location.ID","Year"))




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


# Intitial Linear Models

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
eablfledge.climate = glm(formula = successfulnest ~ cicada + y_anomaly_temp + y_anomaly_precip, 
                         data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Carolina Chickadee", ], 
                         family = binomial(link = "logit"))
eablfledge.climate = glm(formula = successfulnest ~ cicada + y_anomaly_temp + y_anomaly_precip, 
                         data = nestboxes_county_cicada[nestboxes_county_cicada$Species.Name == "Black-capped Chickadee", ], 
                         family = binomial(link = "logit"))


 
 