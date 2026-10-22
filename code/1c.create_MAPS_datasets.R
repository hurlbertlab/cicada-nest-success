##############
#
# Exploring MAPS data
# and preparing datasets
# for use
#
##############

library(dplyr)
library(tidyverse)
library(sf)
library(stringr)
library(ggplot2)
library(daymetr)
library(scales)
library(assertthat)
library(statuser)

source("code/2.functions.R")

#read in cicada info
cicada_emergence_years <- read.csv("data/cicada/cicada_emergence_years_wide.csv")  %>% ## this has broods with 4 emergence years in 4 separate columns
  dplyr::select(-emergence_2019_through_2024)
cicada <- st_read(dsn = "data/cicada/periodical_cicada_with_county.gdb")
#st_crs(cicada) #awesome, already the same crs as MAPs

#load in maps data
locations <- read.csv("data/MAPS_1992-2018_cicada_region/MAPS_STATION_location_and_operations.csv") |>
  st_as_sf(coords = c('DECLNG', "DECLAT"), crs = 4269, remove = FALSE) |>
  #remove when we don't have lat/lon (only affects 4 sites)
  filter(LATITUDE != "-") |>
  rename(STATION_NAME = NAME) |>
  #filter to only locations within a cicada county
  st_join(cicada, join = st_within) |> #dataset gets longer b/c some sites are within the bounds of multiple broods.
  #filter out when it's not within a brood location
  filter(!is.na(BROOD_NAME))

ggplot(locations) + 
  geom_sf()

  n_distinct(locations$STATION_NAME)
  #283 locations.
  #alright now what!
  #we need to add in brood information.....
  #and using the D columns filter to locations that were surveyed in ... at least one cicada year ... hm ...in a way it didn't make sense to for the nestbox data I guess I could have a random effect of station?
  #ah, I think actually, no. I don't need to filter based on being sampled in at least one cicada year.
  #that's going to come later ok? At the least: here are the locations for which we need to get climate data.
  #LATER we can think about if filtering for stations with that kind of stuff is necessary. B/c it's going to all be either cicada year or year before or year after filtering later. And no problem b/c with nestwatch data, I allow for all that variation. I don't like, need a location to be sampled ALL THREE YEARS, I just say - if there's a boost it's gonna affect everywhere. And I just need a general idea of nest success across all years in the years before, year of, and year after a cicada emergence. 
  
  #write.csv(locations, "data/MAPS_locations.csv")

  #and hm. let's go ahead and do climate stuff in here b/c it's not an issue and doesn't need a big loop with only 283 locations.
if(file.exists("data/maps_climate_data.csv")) {
  #if the climate data already exists, just read it in.
  maps_climate <- read.csv("data/maps_climate_data.csv")
  print("climate data exists, reading in. variable name is maps_climate")
} else {
  distinct_locs <- locations |>
    distinct(STATION_NAME, .keep_all = TRUE) |>
    select(STATION_NAME, DECLNG, DECLAT) |>
    st_drop_geometry()
  #and this MAPS data spans from 1991 to 2018.
  #AH. but to calculate anomaly we need the data from 1980 onwards anyway. cheers!
  
  data_list <- list()
  error_list <- list()
  startYear <- 1980
  endYear <- 2025 #even though we won't need the 2025 data, it's used in the anomaly calculations for the nestwatch data so we should use it here too.

  start_idx <- 1
  end_idx <- nrow(distinct_locs)
  
  data_index <- 1

  # Loop through each point and download data
  for (i in start_idx:end_idx) {
    daymet_data <- tryCatch({
      download_daymet(
        site = as.character(distinct_locs$STATION_NAME[i]),  # Ensure it's a string
        lat = as.numeric(distinct_locs$DECLAT[i]),       # Ensure it's numeric
        lon = as.numeric(distinct_locs$DECLNG[i]),      # Ensure it's numeric
        start = startYear,
        end = endYear, 
        internal = TRUE
      )
    }, error = function(e) {
      df <- data.frame(site = rep(distinct_locs$STATION_NAME[i], endYear - startYear + 1),
                       year = startYear:endYear,
                       mean_temp = rep(NA, endYear - startYear + 1),
                       mean_precip = rep(NA, endYear - startYear + 1)
      )
      
      error_list[[length(error_list) + 1]] <<- distinct_locs[i, ]  
      return(NULL)  # Return NULL to show failure
    })
    
    # Skip if data retrieval failed
    if (is.null(daymet_data)) next
    
    # Convert to data frame and store in list
    df <- as.data.frame(daymet_data$data) %>%
      filter(yday >= 121 & yday <= 212) %>%  # Filter to relevant Julian days
      group_by(year) %>%
      summarize(
        mean_temp = mean((tmin..deg.c. + tmax..deg.c.) / 2, na.rm = TRUE),  
        mean_precip = mean(prcp..mm.day., na.rm = TRUE)
      )
    
    df$site <- distinct_locs$STATION_NAME[i]  # Add site ID column for reference
    data_list[[data_index]] <- df  # Store sequentially
    data_index <- data_index + 1  # Increment index
    
    timestamp()
    print(i)
  } #end loop downloading climate data for each MAPS location.
  
  climate_df <- bind_rows(data_list)
  #couldn't download data for one site: North 1/3 of Comp. 16 at 32.33083/-91.33611 latitude/longitude
  #tried to run it again to see if that fixed it but nah
  #that's okay, a recovery rate of 282/283 is not an issue.
  
  maps_climate <- climate_df |>
    #rename variables to be more informative
    rename(STATION_NAME = site) |>
    rename(Year = year) |>
    rename(y_temp = mean_temp) |>
    rename(y_precip = mean_precip) |>
    #for each location calculate long term temperature means and year anomalies from those means
    group_by(STATION_NAME) |>
    mutate(mean_temp = mean(y_temp, na.rm = TRUE))|>
    mutate(mean_precip = mean(y_precip, na.rm = TRUE)) |>
    mutate(y_anomaly_temp = y_temp-mean_temp) |>
    mutate(y_anomaly_precip = y_precip-mean_precip) |>
    mutate(n_years = n()) |>
    ungroup() |>
    group_by(STATION_NAME, Year) |>
    distinct(.keep_all = TRUE) |>
    ungroup() |>
    #and we also only are going to need data from 1992 to 2018
    filter(Year >= 1992) |>
    filter(Year <= 2018)
  
  #save the climate data csv
  write.csv(maps_climate, "data/maps_climate_data.csv", row.names = FALSE)

} #end if/else to get climate data

#OKAY. Now I have to actually um read in the dataset. 
  
# filter to the locations + years with suitable data:
location_years <- locations |>
  #remove Y columns because we only care about operation years if the data is also available to us
  select(-starts_with("Y0")) |>
  select(-starts_with("Y1")) |>
  #pivot
  tidyr::pivot_longer(cols = D89:D18, names_to = "year", names_prefix = "D", values_to = "run_in_year") |>
  #fix years so they're more interpretable when picking up the dataset
  mutate(year = case_when(as.numeric(year) > 70 ~ as.numeric(paste0("19",year)),
                          as.numeric(year) < 70 ~ as.numeric(paste0("20", year))
                          )
         ) |>
  #keep only year-location combinations when we have the data
  filter(run_in_year == "X") |>
  #add in all the information about all potential brood years...
  left_join(
    cicada_emergence_years,
    by = c("BROOD_NAME")
  ) |>
  #okay, and now we keep location-year combinations where they have data during one of the emergence years, or a year before, or a year after. hehe okay long filter...go!
  filter(
    year == emergence_one - 1 |
      year == emergence_one |
      year == emergence_one + 1 |
      year == emergence_two - 1 |
      year == emergence_two |
      year == emergence_two + 1 |
      year == emergence_three - 1 |
      year == emergence_three |
      year == emergence_three + 1 |
      year == emergence_four - 1 | 
      year == emergence_four |
      year == emergence_four + 1 
  ) |>
  #okay now similarly, we need a case_when to tell us this kinda year before/ year of / year after information.
  mutate(cicada_year = 
           case_when(
             year == emergence_one | year == emergence_two | year == emergence_three | year == emergence_four ~ 0,
             year == emergence_one -1 | year == emergence_two -1 | year == emergence_three -1 | year == emergence_four -1 ~ -1,
             year == emergence_one +1 | year == emergence_two +1 | year == emergence_three +1 | year == emergence_four +1 ~ 1
           )) |>
  #awright and now make a binary one while we're here..
  mutate(cicada_year_binary = 
           case_when(cicada_year == 0 ~ 1,
                     cicada_year == -1 | cicada_year == 1 ~ 0)) |>
  #ope, we also need to filter to years >= 1992 b/c there was another protocol used before then and capture efforts are not comprable.
  filter(year >= 1992) |>
  ungroup() |>
  mutate(loc_year = paste0(LOC, year))

#some quick visualization
hist(location_years$year)
hist(location_years$cicada_year)
table(location_years$cicada_year) #epic, all about the same.
table(location_years$cicada_year_binary)

n_distinct(location_years$STATION_NAME)
#cool, and we have 156 unique stations remaining where they banded in +-1 of a cicada year.

#k, and also, let's keep in mind stations are only suitable forr use if in SUITABILITY_station_and_year things look good.
station_suitability <- read.csv("data/MAPS_1992-2018_cicada_region/MAPS_Analysis_SUITABILITY_station_and_year.csv") |>
  #pivot
  tidyr::pivot_longer(cols = D92:D18, names_to = "year", names_prefix = "D", values_to = "suitable_in_year") |>
  #fix years so they're more interpretable when picking up the dataset
  mutate(year = case_when(as.numeric(year) > 70 ~ as.numeric(paste0("19",year)),
                          as.numeric(year) < 70 ~ as.numeric(paste0("20", year))
                          ) #end casewhen
  ) |> #end mutate
  mutate(loc_year = paste0(LOC, year),
         sta_year = paste0(STA, year)) |>
    #stations are only suitable for use if they have the value "B" or "C"
  filter(suitable_in_year %in% c("B", "C")) |> 
  #and there's only one record of "C", so I think that's fine to filter out. MAPS thinks the data is suitable for these purposes even if it doesn't quiiiitttee meet their established standards, but I don't want to have to worry about it.
  filter(suitable_in_year == "B") |> 
  #and now filter to the location years that are relevant for cicada...
  filter(loc_year %in% location_years$loc_year)
  

# add location and station sutability to the climate information;
maps_climate <- read.csv("data/maps_climate_data.csv") |>
  #add LOC for each STATION_NAME
  left_join(
    (locations |> 
       st_drop_geometry() |>
       select(LOC, STATION, STATION_NAME, STA, BROOD_NAME) |>
       distinct(STATION_NAME, .keep_all = TRUE)),
    by = c("STATION_NAME")
  ) 
assertthat::assert_that(sum(is.na(maps_climate$LOC) == TRUE) == 0)
  table(is.na(maps_climate$LOC)) #awesome, no errors.
  
#get climate by location
location_climate <- maps_climate |>
  group_by(LOC, Year) |>
  mutate(loc_y_temp = mean(y_temp),
         loc_y_precip = mean(y_precip)) |>
  ungroup() |>
  group_by(LOC) |>
  mutate(loc_mean_temp = mean(loc_y_temp),
         loc_mean_precip = mean(loc_y_precip)) |>
  mutate(loc_y_anomaly_temp = loc_y_temp - loc_mean_temp,
         loc_y_anomaly_precip = loc_y_precip - loc_mean_precip) |>
  ungroup() |>
  rename(year = Year) |>
  #remove duplicated columns for the various stations.
  distinct(LOC, year, .keep_all = TRUE) |>
  #keep just some columns
  select(LOC, year, loc_y_temp:loc_y_anomaly_precip)
#hist(location_climate$y_anomaly_temp - location_climate$loc_y_anomaly_temp)
#hm, yeah cool that histogram looks great. Stations within a location generally agree.

#get climate by brood
brood_climate <- maps_climate |>
  group_by(BROOD_NAME, Year) |>
  #average over all the stations within the brood.
  mutate(brood_y_temp = mean(y_temp),
         brood_y_precip = mean(y_precip)) |>
  ungroup() |>
  group_by(BROOD_NAME) |>
  mutate(brood_mean_temp = mean(brood_y_temp),
         brood_mean_precip = mean(brood_y_precip)) |>
  #make anomaly
  mutate(brood_y_anomaly_temp = brood_y_temp - brood_mean_temp,
         brood_y_anomaly_precip = brood_y_precip - brood_mean_precip) |>
  ungroup() |>
  rename(year = Year) |>
  #remove duplicated columns for the various broods
  distinct(BROOD_NAME, year, .keep_all = TRUE) |>
  #keep just some columns
  select(BROOD_NAME, year, brood_y_temp:brood_y_anomaly_precip)
  
#get the captures information 
#you may have to unzip the file if you haven't yet, b/c it is stored on github only in zipped form.
captures <- read.csv("data/MAPS_1992-2018_cicada_region/MAPS_BANDING_capture_data.csv") |>
  #start with 728096 total records
  #filter to just the year/location combos of interest
  mutate(loc_year = paste0(LOC, year),
         sta_year = paste0(STA, year)) |>
  filter(loc_year %in% location_years$loc_year) |> #cool, we retain about 10% of the capture data. YAY
  #73607/728096
  #and also filter only to the stations that are suitable for analysis
  filter(sta_year %in% station_suitability$sta_year) |>
  #72346/728096
  #add cicada year information based on location, I checked and all stations within a location experience the same cicada year.
  left_join(
    (location_years |>
       select(LOC, year, BROOD_NAME, MULT_BROOD, cicada_year, cicada_year_binary) |>
       distinct(LOC, year, .keep_all = TRUE)), by = c("LOC", "year")) |>
  #create month and day columns, we'll see if we want to filter on those later.
  mutate(month = as.numeric(str_remove_all(str_extract(DATE, "-[0-9][0-9]-"), "-")),
         day = as.numeric(str_remove_all(str_extract(DATE, "-[0-9][0-9]$"), "-"))
         ) |>
  #awesome, well table(captures$month) shows us we've only got information from May to August. Seems good to me!
  #filter to newly banded or recaptured birds. birds without bands we can't know anything about and will not keep records of?
  filter(C %in% c("N", "R"))  |>
  #and also filter to only records meant to be used in productivity or survivorship analyses
  filter(N %in% c("-")) |>
  #68788/728096, #9.4% of data retained.
  #combine the chickadees because I did that for the nestwatch data too and they're so ecologically similar ^u^
  mutate(SPEC = case_when(SPEC == "BCCH" ~ "BCCC", #for black-capped/carolina chickadee
                          SPEC == "CACH" ~ "BCCC",
                          TRUE ~ SPEC)) #mwhaha. MWHAHAHA

#let's start consensing all this information down
summary_captures <- captures |>
  #ah! before we summarize, we need to remove duplicate captures. keep just one unique capture of each bird per year per location to filter out recaptures of the same bird within the season + year
  distinct(year, BAND, LOC, .keep_all = TRUE) |>
  summarize(n_young = sum(AGE == 4 | AGE == 2), #local or hatching year birds
            n_adults = sum(AGE == 1 | AGE >= 5), #bird is at least an after-hatching-year bird through after-third-year bird. We don't worry about score 0s, indeterminable age birds (which accounts for only 2,770 records anyway. Like 1%)
            n_stations = n_distinct(STA), 
            .by = c(BROOD_NAME, year, SPEC, cicada_year, cicada_year_binary) # u know.. let's group by location rather than banding station? way fewer 0s -> advanced this to grouping by brood, since MAPS productivity analyses are often done over larger scales than single locations, like BCRs etc.
            ) |>
  mutate(productivity = n_young/n_adults) |>
  #and, we can't use any years where no adults were captured, as that brings productivity to infinity
  filter(!is.infinite(productivity)) |>
  #let's also remove weirdly high values, like a productivity of 32 where 98 baltimore oriole young were captured and only 3 adults. That's an outlier adn should not be counted. that still leaves us some cases of high productivity but .. those are .. okay?
  #filter(productivity < 20) |>
  #add in climate data
  left_join(brood_climate, by = c("BROOD_NAME", "year"))

table2(summary_captures$n_stations)
#hm, there's still some brood_years where productivity here is defined by just 1 station. Which, if it's an ABUNDANT SPECIES is fine. But we are going to need to filter based on like, a minimum bound of confidence. Where at least 5 adults are captured.. and maybe a minimum bound on those young too
table2(summary_captures$n_young >= 2, summary_captures$n_adults >= 5)


table(summary_captures$productivity > 2) #6,875 cases where productivity is less than 2, and 284 cases where productivity is higher than that. Eh, we may filter those out at the end, but I think perhaps okay for now. Probably a lot of those are going to be affect by filters to remove sp without enough data.
table(summary_captures$cicada_year_binary)

ggplot(summary_captures[summary_captures$productivity < 2,], aes(x = productivity, )) +
  geom_density(alpha = 0.5)

  #alright, and now we need to set some minimum filter band on species. We only want to look at species with X number of year/location observations where at least one young was captured. 
n_obser <- summary_captures |>
  group_by(SPEC) |>
  summarize(n = n(),
            n_y_gtr_1 = sum(n_young > 1),
            n_a_grt_5 = sum(n_adults >= 5),
            n_a_grt_10 = sum(n_adults >= 10),
            n_y1_a10 = sum(n_adults >= 10 & n_young > 1),
            perc_w_y = n_y_gtr_1/n,
            n_cicada = sum(cicada_year_binary == 1),
            perc_cicada = n_cicada/n) |>
  #well, first step, species where young are never observed should be cut
  filter(n_y_gtr_1 > 0) |>
  #and I want at least 30 total observations to work with
  filter(n >= 28) |> #cutoff at 28 as that lets me keep HOWR in the study, and have an additional species with a comparison btwn MAPS and Nestwatch data
  #And we need the young of these birds to get caught with some regularity in banding years across broods. Might have to raise this later, but for now let's say at least half of the banding-years young have to be caught. Max is like, 90% of brood-years
  filter(perc_w_y >= .50)
  #after looking at this, let's keep just species with n_a_grt_10 > 20. 

keep_species <- n_obser |>
  filter(n_a_grt_10 >= 20)


# I want a bar chart for species where each species has a plot showing both perc_w_y and perc_cicada in two different colors. 
#did use chatgpt to generate this plot.
# Reshape the two percentage columns into a long format
plot_data <- n_obser |>
  select(SPEC, perc_w_y, perc_cicada) |>
  pivot_longer(
    cols = c(perc_w_y, perc_cicada),
    names_to = "measure",
    values_to = "proportion"
  ) |>
  mutate(
    measure = recode(
      measure,
      perc_w_y = "% Banding Brood-Years \n with Young",
      perc_cicada = "% Banding Brood-Years \n with Cicada"
    ),
    # Order species by percentage of observations with young
    SPEC = reorder(SPEC, proportion)
  )
  
ggplot(plot_data, aes(x = SPEC, y = proportion, fill = measure)) +
  geom_col(
    position = position_dodge(width = .8),
    width = .9
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = c(
      "% Banding Brood-Years \n with Young" = "#2C7FB8",
      "% Banding Brood-Years \n with Cicada" = "#D95F02"
    )
  ) +
  labs(
    x = "Species",
    y = "Percentage of Brood-Years",
    fill = NULL,
    title = "Banding Brood-Year Stats by Species"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top",
    axis.text = element_text(color = "black")
  ) +
  #add average lines for the averages
  geom_hline(
    yintercept = mean(n_obser$perc_w_y, na.rm = TRUE),
    color = "#2C7FB8",
    linetype = "dashed",
    linewidth = 1
  ) +
  geom_hline(
    yintercept = mean(n_obser$perc_cicada, na.rm = TRUE),
    color = "#D95F02",
    linetype = "dashed",
    linewidth = 1
  )

#Now, I think the question is, do we limit this analysis to ONLY banding location-years where any young are captured? how informative are those zeros?
#well, we know banding occured with sufficient effort, so, there was every OPPORTUNITY to capture young.

#okay, and now last step! filter the summary_captures to only the species that are left aftering filtering out based on minimum data bounds from n_obser
maps_analysis_df <- summary_captures |>
  #keep only species with more than 20 observations with more than 10 adult captures
  filter(SPEC %in% keep_species$SPEC) |>
  #and then keep ONLY those >10 adult captures. bc when we're using proportion later, we're not going to be accounting for how many individuals were captured. If it's not at least 10, sampling effort wasn't high enough in that brood-year to include it in our model.
  filter(n_adults >= 10) |>
  group_by(SPEC, cicada_year_binary) |>
  mutate(n_sp_cyrbin = n()) |>
  ungroup() |>
  #also, let's add a full species name...
  left_join(
    (read.csv("data/MAPS_1992-2018_cicada_region/IBP-AOS-list25-birdcode-taxonomy.csv") |>
       select(SPEC, COMMONNAME, SCINAME)),
    by = c("SPEC")
  ) |>
  rename(Species.Name = COMMONNAME) |>
  mutate(Species.Name = case_when(SPEC == "HOWR" ~ "Northern House Wren",
                                  SPEC == "BCCC" ~ "Black-capped and\n Carolina Chickadee",
                                  TRUE ~ Species.Name))

write.csv(maps_analysis_df, "data/maps_analysis_df.csv", row.names = FALSE)

#Modeling time!! YAYYYY
maps_results <- make_trend_table(cols_list = 
                              c("Species.Name", "model", "model_desc", "intercept", "emergence", "e_sd", "e_p", "e_signif", "y_anomaly_temp", "yat_sd", "yat_p", "yat_signif", "y_anomaly_precip", "yap_sd", "yap_p", "yap_signif", "n_noncicada", "n_cicada"),
                            rows_list = unique(maps_analysis_df$Species.Name)) |>
  mutate(model = as.character(model),
         model_desc = as.character(model_desc),
         mutate(across(where(is.logical), as.numeric)))

maps_models = list()

#loop through the species!
for(i in 1:length(unique(maps_analysis_df$Species.Name))) {
  
  sp <- unique(maps_analysis_df$Species.Name)[i]
  print(i); print(sp)
  
  tmp <- maps_analysis_df |>
    filter(Species.Name == sp)
  
  tmp_glm <- glm(productivity ~ cicada_year_binary + brood_y_anomaly_temp + brood_y_anomaly_precip, 
                 data = tmp, 
                 family = gaussian) #gaussian?
  
  maps_models[[i]] = tmp_glm
  
  summary <- summary(tmp_glm)
  
  tmp_results <- maps_results |>
    filter(Species.Name == sp) |>
    mutate(model = as.character(tmp_glm$formula)[3],
           model_desc = "gaussian",
           intercept = summary$coefficients[1,1],
           emergence = summary$coefficients[2,1],
           e_sd = summary$coefficients[2,2],
           e_p = summary$coefficients[2,4], 
           e_signif = ifelse(summary$coefficients[2,4] < 0.05, "signif", "no"),
           y_anomaly_temp = summary$coefficients[3,1],
           yat_sd = summary$coefficients[3,2],
           yat_p = summary$coefficients[3,4],
           yat_signif = ifelse(summary$coefficients[3,4] < 0.05, "signif", "no"),
           y_anomaly_precip = summary$coefficients[4,1],
           yap_sd = summary$coefficients[4,2],
           yap_p = summary$coefficients[4,4],
           yap_signif = ifelse(summary$coefficients[4,4] < 0.05, "signif", "no"),
           n_noncicada = sum(tmp$cicada_year_binary == 0),
           n_cicada = sum(tmp$cicada_year_binary == 1)
           
           #           prob_intercept = plogis(summary$coefficients[1,1]),
           #           prob_post_e = plogis(summary$coefficients[2,1]),
           #           prob_pe_sd = plogis(summary$coefficients[2,2]),
           #           prob_yat = plogis(summary$coefficients[3,1]),
           #           prob_yat_sd = plogis(summary$coefficients[3,2]),
           #           prob_yap = plogis(summary$coefficients[4,1]),
           #           prob_yap_sd = plogis(summary$coefficients[4,2]),
           #           log_odds_emergence = exp(summary$coefficients[2,1])
    )
  
  #double-check nothing messed up in calculating the n() in each group.
  assert_that(tmp_results$n_noncicada + tmp_results$n_cicada == nrow(tmp))
  
  maps_results[maps_results$Species.Name == sp,] <- tmp_results
  #basically rows_update() without the issues caused by the postcicada_results originally all having logical() rows.
  #postcicada_results |>
  #rows_update(tmp_results, by = c("Species.Name"))
  
}

write.csv(maps_results, "model_results/maps_gaussian_results.csv")
save(maps_models, file = "model_results/maps_gaussian_glms.rds")



# graph mean and standard deviation of pct fledged for each species over cicada year 
## group, calc mean & stdev
{
  summary_data <- maps_analysis_df %>%
    group_by(Species.Name, cicada_year_binary) %>%
    summarise(
      #nest success 0/1
      mean_pct_young = mean(productivity, na.rm = TRUE),
      se_pct_young = sd(productivity, na.rm = TRUE) / sqrt(n()),
      n = n(),
    ) |>
    ungroup() |>
    arrange(desc((n)))
  
  summary_data <- summary_data |>
    group_by(Species.Name) |>
    mutate(lab = make_n_labels(filter_species = Species.Name, labs = "cicada/non")) |>
    ungroup() |>
    #add in results
    left_join(maps_results, by = c("Species.Name"))
}

ggplot(summary_data, aes(x = cicada_year_binary, y = mean_pct_young, color = e_signif)) +
  ylim(0, 2) + 
  geom_line(linewidth = 1.5) +
  geom_errorbar(aes(ymin = mean_pct_young - se_pct_young, ymax = mean_pct_young + se_pct_young), width = 0.2, linewidth = 1.5) +
  facet_wrap(~ reorder(Species.Name, n, decreasing = TRUE), ncol = 6) +  # Create separate plots for each species, 3 columns. Now, would like the colors to still go in typical ggplot order, but that's okay. Probably I will need to re-do this by hand to make that happen.
  labs(
    x = "Cicada Year",
    y = "Mean Productivity"
  ) +
  scale_x_continuous(breaks = c(0, 1),  # Numeric breaks at -1, 0, and 1
                     labels = c("0", "X")) +  # Custom text labels) 
  #scale_x_continuous(breaks = c(-1, 0, 1),  # Numeric breaks at -1, 0, and 1
  #                  labels = c("-1", "X", "1")) +  # Custom text labels) 
  scale_color_discrete() + #limits = original_order) +  # Fix color order to original
  theme_minimal(base_size = 19) + # increase text size) +
  theme(
    legend.position = "none", # remove legend
    #panel.grid.major = element_blank(), # Remove major gridlines
    panel.grid.minor = element_blank()  # Remove minor gridlines
  ) + 
  geom_text(data = (summary_data |>
                      group_by(Species.Name) |>
                      slice(1) |>
                      arrange(desc(n)) |>
                      ungroup() |>
                      mutate(
                        x = .2,
                        y = 1.9
                      )),
            aes(x = x, y = y, label = lab),
            size = 3, 
            vjust = 1,
            inherit.aes = FALSE) +
  geom_text(data = (summary_data |>
                      group_by(Species.Name) |>
                      slice(1) |>
                      arrange(desc(n)) |>
                      ungroup() |>
                      mutate(
                        x = .8,
                        y = 1.9
                      )),
            aes(x = x, y = y, label = as.character(round(e_p, 3))),
            size = 3, 
            vjust = 1,
            inherit.aes = FALSE)
