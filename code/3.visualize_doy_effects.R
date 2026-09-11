############
#
# Visualize the day of year effects for each species. Color code by latitude. Fit faint lm lines for latidudinal bands?
# Can also visualize the hatchday - mean cicada day to account for latitude, and see what that looks like. 
#
############

#get analysis df from 2.analysis_cicada_year.R
analysis_df <- read.csv("data/analysis_df.csv")

# graph mean and standard deviation of pct fledged for each species over cicada year 
## group, calc mean & stdev
{
  summary_data <- analysis_df %>%
    group_by(Species.Name, jday_hatch) %>%
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
  
  cicadayr_summary_data <- analysis_df |>
    group_by(Species.Name, jday_hatch, cicada_year) |>
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
  
  binary_summary_data <- analysis_df |>
    group_by(Species.Name, jday_hatch, cicada_year_binary) |>
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
}

# Get the original order of species
original_order <- unique(summary_data$Species.Name)
#okay have to do this a bit dif now b/c HOSP has a year with more obs than American Robin
original_order <- c("Eastern Bluebird", "Tree Swallow", "Northern House Wren", "Black-capped and\n Carolina Chickadee", "Purple Martin", "Carolina Wren", "American Robin", "House Sparrow", "Prothonotary Warbler")

data_m1 <- subset(cicadayr_summary_data, cicada_year == -1)
data_0 <- subset(cicadayr_summary_data, cicada_year == 0)
data_1 <- subset(cicadayr_summary_data, cicada_year == 1)

b_data_0 <- subset(binary_summary_data, cicada_year_binary == 0) #cicada
b_data_1 <- subset(binary_summary_data, cicada_year_binary == 1) #no cicada

## ok now graph
png(filename = "figures/2026.09.03_doy_effects.png", 
    width = 630,
    height = 630,
    units = "px", 
    type = "windows")
{
  ggplot(summary_data, aes(x = jday_hatch, y = mean_pct_nest_success, color = Species.Name)) +
    ylim(0, 1) + 
    geom_point(alpha = 0.5) +
    # add a line overtop
    # main line with all the data
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.4) +
    #geom_errorbar(aes(ymin = mean_pct_nest_success - se_pct_nest_success, ymax = mean_pct_nest_success + se_pct_nest_success), width = 0.2, linewidth = 1.5) +
    facet_wrap(~ reorder(Species.Name, n, decreasing = TRUE), ncol = 3) +  # Create separate plots for each species, 3 columns. Now, would like the colors to still go in typical ggplot order, but that's okay. Probably I will need to re-do this by hand to make that happen.
    labs(
      x = "Day of Year",
      y = "Mean Nest Success"
    ) +
    scale_color_discrete(limits = original_order) +  # Fix color order to original
    #theme_minimal() +
    theme_minimal(base_size = 19) + # increase text size) +
    theme(
      legend.position = "none", # remove legend
      #panel.grid.major = element_blank(), # Remove major gridlines
      panel.grid.minor = element_blank()  # Remove minor gridlines
    ) 
  
  #annotation_raster(cicada_image, xmin = 0.2, xmax = 0.4, ymin = 0, ymax = 0.2) #hm, guess that didn't work. Will need to test or add it in post.
  #make all text bolder etc.
} 
dev.off()

png(filename = "figures/2026.09.03_doy_cic_effects.png", 
    width = 630,
    height = 630,
    units = "px", 
    type = "windows")
{
  ggplot(summary_data, aes(x = jday_hatch, y = mean_pct_nest_success)) +
    ylim(0, 1) + 
    geom_point(aes(color = Species.Name), alpha = 0.2) +
    # add a line overtop
    # main line with all the data
    #geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.4) +
    # Additional lines for cicada_year == -1 (dashed lines)
    geom_smooth(data = data_m1, 
                aes(group = Species.Name, 
                    color = Species.Name, 
                    linetype = "Pre-Cicada"),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.3) +
    
    # Additional lines for cicada_year == 0 (dashed lines)
    geom_smooth(data = data_0, 
                aes(group = Species.Name, 
                    color = Species.Name,
                    linetype = "Cicada"),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.3) +
    
    # Additional lines for cicada_year == 1 (dotted lines)
    geom_smooth(data = data_1, 
                aes(group = Species.Name, 
                    color = Species.Name, 
                    linetype = "Post-Cicada"),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.3) +
    #geom_errorbar(aes(ymin = mean_pct_nest_success - se_pct_nest_success, ymax = mean_pct_nest_success + se_pct_nest_success), width = 0.2, linewidth = 1.5) +
    facet_wrap(~ reorder(Species.Name, n, decreasing = TRUE), ncol = 3) +  # Create separate plots for each species, 3 columns. Now, would like the colors to still go in typical ggplot order, but that's okay. Probably I will need to re-do this by hand to make that happen.
    labs(
      x = "Day of Year",
      y = "Mean Nest Success",
      linetype = "Cicada Year" #legend title
    ) +
    scale_color_discrete(limits = original_order, 
                         guide = "none") +  # Fix color order to original + hide color legend
    scale_linetype_manual(values = c(
      "Pre-Cicada" = "dotdash",
      "Post-Cicada" = "dotted",
      "Cicada" = "solid")) +
    #theme_minimal() +
    theme_minimal(base_size = 19) + # increase text size) +
    theme(
      legend.position = "bottom", # remove legend
      #panel.grid.major = element_blank(), # Remove major gridlines
      panel.grid.minor = element_blank()  # Remove minor gridlines
    ) 
  
  #annotation_raster(cicada_image, xmin = 0.2, xmax = 0.4, ymin = 0, ymax = 0.2) #hm, guess that didn't work. Will need to test or add it in post.
  #make all text bolder etc.
} 
dev.off()

#hm yeah okay! that makes a compelling argument to me that day of year should be included in the model. 
#to account for latitude.... we could do by hatch_day minus mean_cicada_day to account for latitude.

plot(analysis_df$h_mid, analysis_df$jday_hatch)
abline(0, 1)
#yeah okay, hatch_day minus the mean cicada date is different, and lets us capture that latitudinal variation in start timing. Let's use that and make another graph. 
cor(analysis_df$h_mid, analysis_df$jday_hatch)
#correlation is very strong (of course)

#Calculate summary
async_cicadayr_summary_data <- analysis_df |>
  mutate(h_mid = round(h_mid)) |> #round for grouping for the visualizations. 
  #mutate(h_mid = (floor((h_mid - 1) / 5) * 5 + 1)) |> #group by 5 days
  group_by(Species.Name, h_mid, cicada_year) |>
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

async_data_m1 <- subset(async_cicadayr_summary_data, cicada_year == -1)
async_data_0 <- subset(async_cicadayr_summary_data, cicada_year == 0)
async_data_1 <- subset(async_cicadayr_summary_data, cicada_year == 1)

png(filename = "figures/2026.09.03_hdate-midcicdate_effects.png", 
    width = 630,
    height = 630,
    units = "px", 
    type = "windows")
{
  ggplot(async_cicadayr_summary_data, aes(x = h_mid, y = mean_pct_nest_success)) +
    facet_wrap(~ reorder(Species.Name, n, decreasing = TRUE), ncol = 3) +  # Create separate plots for each species, 3 columns. Now, would like the colors to still go in typical ggplot order, but that's okay. Probably I will need to re-do this by hand to make that happen.
    ylim(0, 1) + 
    geom_point(aes(color = Species.Name), alpha = .2) +
    # add a line overtop
    # main line with all the data
    #geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.4) +
    # Additional lines for cicada_year == -1 (dashed lines)
    geom_smooth(data = async_data_m1, 
                aes(group = Species.Name, 
                    color = Species.Name, 
                    linetype = "Pre-Cicada"),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.3) +
    
    # Additional lines for cicada_year == 0 (dashed lines)
    geom_smooth(data = async_data_0, 
                aes(group = Species.Name, 
                    color = Species.Name,
                    linetype = "Cicada"),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.3) +
    
    # Additional lines for cicada_year == 1 (dotted lines)
    geom_smooth(data = async_data_1, 
                aes(group = Species.Name, 
                    color = Species.Name, 
                    linetype = "Post-Cicada"),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.3) +
    #geom_errorbar(aes(ymin = mean_pct_nest_success - se_pct_nest_success, ymax = mean_pct_nest_success + se_pct_nest_success), width = 0.2, linewidth = 1.5) +
    labs(
      x = "Hatch Date - Mid Cicada Date (Latitude Adjusted DOY)",
      y = "Mean Nest Success",
      linetype = "Cicada Year" #legend title
    ) +
    scale_color_discrete(limits = original_order, 
                         guide = "none") +  # Fix color order to original + hide color legend
    scale_linetype_manual(values = c(
      "Pre-Cicada" = "dotdash",
      "Post-Cicada" = "dotted",
      "Cicada" = "solid")) +
    #theme_minimal() +
    theme_minimal(base_size = 19) + # increase text size) +
    theme(
      legend.position = "bottom", # remove legend
      #panel.grid.major = element_blank(), # Remove major gridlines
      panel.grid.minor = element_blank()  # Remove minor gridlines
    ) 
  
  #annotation_raster(cicada_image, xmin = 0.2, xmax = 0.4, ymin = 0, ymax = 0.2) #hm, guess that didn't work. Will need to test or add it in post.
  #make all text bolder etc.
} 
dev.off()

#these graphs need lines for the cicada emergence timing bounds...
#like. SUPER fascinating something going on with bluebirds there. Nest success is HIGHEST at the edges? But! BUT! those are also. The locations where birds are either nesting reeeealllly early (-50 days from whenever in time the cicadas emerge) or birds nesting realllllyyyy late (+75 days from whenever in time the cicadas emerge). Putting this timing stuff in relative to cicada emergence is showing some interesting stuff. Are like, the bulk of birds breeding DURING the cicada period? Yes right?
library(statuser)
table2(analysis_df$Species.Name, analysis_df$asynchrony < 1)
df <- as.data.frame(table2(analysis_df$Species.Name, analysis_df$asynchrony < 1)$freq) |>
  group_by(Species.Name) |>
  pivot_wider(names_from = asynchrony.1,
              values_from = Freq,
              names_prefix = "Async_lessthan_1") |>
  mutate(perc_asynchronous = Async_lessthan_1FALSE / sum(Async_lessthan_1FALSE, Async_lessthan_1TRUE)) |>
  ungroup() |>
  arrange(desc(perc_asynchronous))
#this varies from chickadees being almost entirely synchronous, to House Wrens being half asynchronous with cicadas.

#OH. LMAO yeah and we can make these graphs for asynchrony as well.
#Calculate summary
async_cicadayr_summary_data <- analysis_df |>
  mutate(asynchrony = round(asynchrony, 1)) |>
  group_by(Species.Name, asynchrony, cicada_year) |>
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

async_data_m1 <- subset(async_cicadayr_summary_data, cicada_year == -1)
async_data_0 <- subset(async_cicadayr_summary_data, cicada_year == 0)
async_data_1 <- subset(async_cicadayr_summary_data, cicada_year == 1)

png(filename = "figures/2026.09.03_ASYNC_effects.png", 
    width = 630,
    height = 630,
    units = "px", 
    type = "windows")
{
  ggplot(async_cicadayr_summary_data, aes(x = asynchrony, y = mean_pct_nest_success)) +
    facet_wrap(~ reorder(Species.Name, n, decreasing = TRUE), ncol = 3) +  # Create separate plots for each species, 3 columns. Now, would like the colors to still go in typical ggplot order, but that's okay. Probably I will need to re-do this by hand to make that happen.
    ylim(0, 1) + 
    geom_point(aes(color = Species.Name), alpha = .2) +
    # add a line overtop
    # main line with all the data
    #geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.4) +
    # Additional lines for cicada_year == -1 (dashed lines)
    geom_smooth(data = async_data_m1, 
                aes(group = Species.Name, 
                    color = Species.Name, 
                    linetype = "Pre-Cicada"),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.3) +
    
    # Additional lines for cicada_year == 0 (dashed lines)
    geom_smooth(data = async_data_0, 
                aes(group = Species.Name, 
                    color = Species.Name,
                    linetype = "Cicada"),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.3) +
    
    # Additional lines for cicada_year == 1 (dotted lines)
    geom_smooth(data = async_data_1, 
                aes(group = Species.Name, 
                    color = Species.Name, 
                    linetype = "Post-Cicada"),
                method = "lm", se = FALSE, linewidth = 1.2, alpha = 0.3) +
    #geom_errorbar(aes(ymin = mean_pct_nest_success - se_pct_nest_success, ymax = mean_pct_nest_success + se_pct_nest_success), width = 0.2, linewidth = 1.5) +
    labs(
      x = "Asynchrony",
      y = "Mean Nest Success",
      linetype = "Cicada Year" #legend title
    ) +
    scale_color_discrete(limits = original_order, 
                         guide = "none") +  # Fix color order to original + hide color legend
    scale_linetype_manual(values = c(
      "Pre-Cicada" = "dotdash",
      "Post-Cicada" = "dotted",
      "Cicada" = "solid")) +
    #theme_minimal() +
    theme_minimal(base_size = 19) + # increase text size) +
    theme(
      legend.position = "bottom", # remove legend
      #panel.grid.major = element_blank(), # Remove major gridlines
      panel.grid.minor = element_blank()  # Remove minor gridlines
    ) 
  
  #annotation_raster(cicada_image, xmin = 0.2, xmax = 0.4, ymin = 0, ymax = 0.2) #hm, guess that didn't work. Will need to test or add it in post.
  #make all text bolder etc.
} 
dev.off()
