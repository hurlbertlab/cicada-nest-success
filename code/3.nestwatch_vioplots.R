###########################
#
# show variation across years for each species with violin plots of nest success
# ALL non-cicada nests within EACH YEAR including years not close to cicada years.
# vs all cicada nests within EACH YEAR (2008, 2009, etc. NOT "cicada_year_binary == 1")
#
#
############################

library(dplyr)
library(vioplot)

# Read in location.ID climate data
climate_data<- read.csv("data/filtered_climate_data.csv")
# Read in the cicada emergence data
cicada_bounds <- read.csv("data/cicada_bounds_latitude_by_j_date.csv")

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
  #As well, previous analyses (Bella's work on this project) looked multiple years in the past + future of a cicada emergence. But, there are a lot of things that could vary between years. I want to try and control for the effect of cicada emergence as much as possible. Therefore, just going to look at the year before, year of, and year after a cicada emergence
  #DON"T filter here, because we actually want to keep ALL the years.
  #filter(cicada_year %in% c(-1, 0, 1)) |>
  #HM. And actually for the two-brood records we don't want to remove these completely, it's just that we want to keep only one record. Let's use distinct() but for the most part I've already handled it. Either it was a year of a cicada emergence, a year earlier, or a year after. Most multiple broods are not overlapping in years as well. And for those multiple broods that do overlap in years in some counties, we're solid b/c they're just duplicates of each other anyway.
  group_by(Attempt.ID, Location.ID, Year, Hatch.Date) |>
  distinct(Attempt.ID, Location.ID, Year, Hatch.Date, .keep_all = TRUE) |>
  ungroup() |>
  #make our other variables of interest: pct_fledged and time_to_fledge
  mutate(pct_fledged = ifelse(Young.Total == 0, NA, Young.Fledged / Young.Total),
         time_to_fledge = (lubridate::interval(ymd(Hatch.Date), ymd(Fledge.Date)))/days()
  ) |>
  # make a true/false nest success based on pct_fledged
  mutate(nest_success_tf = ifelse(pct_fledged > 0, 1, 0)) |> #if any young fledged = 1
  # and then account for cases when pct_fledged is not known, but because an Outcome has been recorded, we know if the nest was successful or not
  mutate(nest_success_tf = case_when(
    is.na(nest_success_tf) & Outcome %in% c("s1") ~ 1, 
    is.na(nest_success_tf) & Outcome %in% c("f", "f1", "f2", "f3", "f4", "f6", "f7") ~ 0, 
    #I went back and forth about f4 b/c only young of brood parasites fledged still == success in raising young (in the 10 relevant cases where it occurs) b/c SOME number of young were successfully raised. 1 HOSP, 2 CAWR, 7 EABL - BUT. there are likely nests categorized only as 'f' that are actually 'f4' - therefore since nestwatch considers f4 to be a nest failure state I will as well.
    #there's also 43 weird cases where b/c of ? entry error ? nestwatch error ? we have a !is.na(time_to_fledge) column, but a blank Outcome column + no information about the number of young. But, if any young FLEDGED, we know at least some made it, and should classify these as successful nests.
    is.na(nest_success_tf) & !is.na(time_to_fledge) ~ 1, #successful nest
    !is.na(nest_success_tf) ~ nest_success_tf
  ),
  #and, if we know the nest failed and otherwise don't have pct_survival, we know survival = 0
  pct_fledged = case_when(
    is.na(pct_fledged) & nest_success_tf == 0 ~ 0,
    TRUE ~ pct_fledged
  )
  ) |>
  # remove Outcome = f5, where there's human intervention to remove a nest for invasive sp management (cough cough House Sparrow)
  filter(!Outcome %in% "f5") |>
  # move columns around for my convience and interpretation..
  dplyr::relocate(pct_fledged:nest_success_tf, .after = Outcome) |>
  # and we only need to keep records that have at least one of our outcome variables
  filter(!is.na(nest_success_tf)) |>
  #and remove unexpected outcomes like pct_fledged that are not between 0 and 1 
  filter(pct_fledged >= 0 & pct_fledged <= 1 | is.na(pct_fledged)) |>
  #make chickadees the same species.name, ~80 Black-capped observations per cicada_year.
  mutate(Species.Name = case_when(
    str_detect(Species.Name, "Chickadee") ~ "Black-capped and\n Carolina Chickadee",
    TRUE ~ Species.Name
  ))  |>
  # select just the info we need, don't need all 46 variables.
  dplyr::select(Attempt.ID, Location.ID, Year, Species.Name, First.Lay.Date:nest_success_tf, BROOD_NAME, MULT_BROOD, ST_CNTY_CODE, cycle, emergence_three, emergence_four, cicada_year:y_anomaly_precip) |>
  mutate(cicada_year_binary = case_when(cicada_year == 0 ~ 1,
                                        cicada_year != 0 ~ 0))

hist(analysis_df$Year) #cool, yup

summary <- analysis_df |>
  group_by(Species.Name, cicada_year_binary, Year) %>%
  summarise(
    #nest success 0/1
    mean_pct_nest_success = mean(nest_success_tf, na.rm = TRUE),
    se_pct_nest_success = sd(nest_success_tf, na.rm = TRUE) / sqrt(n()),
    #pct fledged (although I'll say, b/c of na.rm this has fewer data points than nest success t/f)
    mean_pct_survival = mean(pct_fledged, na.rm = TRUE),
    se_pct_survival = sd(pct_fledged, na.rm = TRUE) / sqrt(n()),
    n = n(),
    n_pct_survival = sum(!is.na(pct_fledged))
  ) |>
  ungroup() |>
  arrange(desc((n)))

for(i in 1:length(unique(summary$Species.Name))) {
  one_sp <- summary |>
  filter(Species.Name == unique(summary$Species.Name)[i]) |>
  mutate(plot_size = case_when(n == 1 ~ .5,
                               TRUE ~ 1))

c <- one_sp |>
  filter(cicada_year_binary == 1)

nc <- one_sp |>
  filter(cicada_year_binary == 0)

#I would size the dots based on n but that makes them like, super hard to read.
#maybe better to scale the coloration of the dots by n?
#or don't.. worry about it.... because the sd is really wide for years without a lot of data.
#but I think this nicely shows that cicada nests are doing better for Eastern Bluebirds
#OR! When n = 1, plot_size = .5, else plot_size = 1

plot(
  x = nc$Year,
  y = nc$mean_pct_nest_success,
  pch = 16,
  ylim = c(0, 1),
  cex = nc$plot_size,
  main = nc$Species.Name[1],
  xlab = "Year",
  ylab = "% Nest Success",
  xaxt = "n"
)
axis(1, at = 2007:2025, labels = 2007:2025)
segments(x0 = nc$Year,
         x1 = nc$Year,
         y0 = nc$mean_pct_nest_success - nc$se_pct_nest_success,
         y1 = nc$mean_pct_nest_success + nc$se_pct_nest_success,
         lwd = 2)
points(
  x = c$Year + .25,
  y = c$mean_pct_nest_success,
  pch = 16,
  col = "orange",
  cex = c$plot_size
)
segments(x0 = c$Year + .25,
         x1 = c$Year + .25,
         y0 = c$mean_pct_nest_success - c$se_pct_nest_success,
         y1 = c$mean_pct_nest_success + c$se_pct_nest_success,
         col = "orange",
         lwd = 2)
segments(x0 = nc$Year + .5,
         x1 = nc$Year + .5,
         y0 = -.2,
         y1 = 1.3,
         col = "grey85",
         lty = "dashed")
legend("bottomright", 
       legend = c("No Cicada",
                  "Cicada Exposure"),
       fill = c("black", "orange"),
       bty = "n")
}
