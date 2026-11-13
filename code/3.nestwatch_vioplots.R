###########################
#
# show variation across years for each species with violin plots of nest success
# ALL non-cicada nests within EACH YEAR including years not close to cicada years.
# vs all cicada nests within EACH YEAR (2008, 2009, etc. NOT "cicada_year_binary == 1")
#
#
############################

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(vioplot)

# Read in location.ID climate data
climate_data<- read.csv("data/filtered_climate_data.csv")
# Read in the cicada emergence data
cicada_bounds <- read.csv("data/cicada_bounds_latitude_by_j_date.csv")

nestwatch_df <- read.csv("data/nestwatch_analysis_df.csv")
nestwatch_results <- read.csv("model_results/binomial_BINARY_hmid_glmER.csv")
cicada_results <- nestwatch_results |>
  filter(term %in% c("cicada_year_binary", "(Intercept)")) |>
  mutate(term = case_when(term == "(Intercept)" ~ "Int",
                          term == "cicada_year_binary" ~ "cic")) |>
  pivot_wider(
    id_cols = c(Species.Name, formula, n_noncicada, n_cicada), # Keep static columns as single rows
    names_from = term, # Values here become prefixes
    values_from = c(effect, estimate, std_error, signif, p_value, 
                    conf.low, conf.high, odds_ratio, or_conf_low, 
                    or_conf_high, pct_change_odds, baseline_prob), # Varying columns
    names_sep = "_"
  ) |>
  arrange(desc(n_noncicada)) |>
  mutate(sp_id = row_number()) |>
  ungroup() 

plot(y = plogis(cicada_results$estimate_Int + cicada_results$estimate_cic),
     x = cicada_results$sp_id,
     ylim = c(0, 1),
     pch = 16,
     cex = 1.5,
     ylab = "Odds Ratio" #but it shouldn't be odds ratio. Convert to +/- percent nest success?
     )
abline(h = 1, lty = "dashed")
segments(y0 = cicada_results$or_conf_low,
         y1 = cicada_results$or_conf_high,
         x0 = cicada_results$sp_id,
         lwd = 3)
                       
hist(nestwatch_df$Year) #cool, yup

year_summary <- nestwatch_df |>
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

cicada_summary <- nestwatch_df |>
  group_by(Species.Name, cicada_year_binary) |>
  summarize(
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

pdf(file = "figures/2026.11.12.nestwatch_vioplots.pdf")

for(i in 1:length(unique(year_summary$Species.Name))) {

  par(mfrow = c(1,2))
  
  one_sp <- year_summary |>
  filter(Species.Name == unique(year_summary$Species.Name)[i]) |>
  mutate(plot_size = case_when(n == 1 ~ 1,
                               TRUE ~ 1))
  cic_sum <- cicada_summary |>
    filter(Species.Name == unique(year_summary$Species.Name)[i]) |>
    mutate(plot_size = case_when(n == 1 ~ .5,
                                 TRUE ~ 1))

c <- one_sp |>
  filter(cicada_year_binary == 1)

nc <- one_sp |>
  filter(cicada_year_binary == 0)

#cicada_result
tmp_cic <- nestwatch_results |>
  filter(Species.Name == unique(year_summary$Species.Name)[i]) |>
  filter(term %in% c("cicada_year_binary", "(Intercept)"))

#I would size the dots based on n but that makes them like, super hard to read.
#maybe better to scale the coloration of the dots by n?
#or don't.. worry about it.... because the sd is really wide for years without a lot of data.
#but I think this nicely shows that cicada nests are doing better for Eastern Bluebirds
#OR! When n = 1, plot_size = .5, else plot_size = 1

#to plot one species (need summary data from nestwatch_analyses cut to one_sp)
# ggplot(one_sp, aes(x = cicada_year_binary, y = mean_pct_nest_success, color = "orange")) +
#   +     ylim(0, 1) + 
#   +     geom_line(linewidth = 1.5, color = "orange") +
#   +     geom_errorbar(aes(ymin = mean_pct_nest_success - se_pct_nest_success, ymax = mean_pct_nest_success + se_pct_nest_success), width = 0.2, linewidth = 1.5, color = "orange") +
#   +     labs(
#     +         x = "Cicada Year",
#     +         y = "Mean Nest Success"
#     +     ) +
#   +     scale_x_continuous(breaks = c(0, 1),  # Numeric breaks at -1, 0, and 1
#                            +                        labels = c("0", "X")) +  # Custom text labels) 
#   +     #scale_x_continuous(breaks = c(-1, 0, 1),  # Numeric breaks at -1, 0, and 1
#   +     #                  labels = c("-1", "X", "1")) +  # Custom text labels) 
#   +     theme_minimal(base_size = 19) + # increase text size) +
#   +     theme(
#     +         legend.position = "none", # remove legend
#     +         #panel.grid.major = element_blank(), # Remove major gridlines
#       +         panel.grid.minor = element_blank()  # Remove minor gridlines
#     +     ) 

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

dev.off()



#okay, let's also actually do maps vioplots too while we're here.
maps <- read.csv("data/maps_nestwatch_df.csv") 

pdf(file = "figures/maps_vioplots.pdf")

for(i in 1:length(unique(maps$Species.Name))) {
  one_sp <- maps |>
    filter(Species.Name == unique(maps$Species.Name)[i]) |>
    group_by(Species.Name, year, cicada_year_binary)  |>
    summarize(mean_productivity = mean(productivity),
              se_productivity = sd(productivity),
              n = n()) |>
    #mutate(plot_size = case_when(n == 1 ~ .5,
    #                             TRUE ~ 1)) |>
    ungroup()
  
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
    x = nc$year,
    y = nc$mean_productivity,
    pch = 16,
    ylim = c(0, 5), #this does cut just one outlier productivity at like. 7 but that's okay, probably a species I should cut from analysis anyway.
    cex = nc$plot_size,
    main = nc$Species.Name[1],
    xlab = "Year",
    ylab = "Productivity",
    xaxt = "n"
  )
  axis(1, at = 1992:2025, labels = 1992:2025)
  segments(x0 = nc$year,
           x1 = nc$year,
           y0 = nc$mean_productivity - nc$se_productivity,
           y1 = nc$mean_productivity + nc$se_productivity,
           lwd = 2)
  points(
    x = c$year + .25,
    y = c$mean_productivity,
    pch = 16,
    col = "orange",
    cex = c$plot_size
  )
  segments(x0 = c$year + .25,
           x1 = c$year + .25,
           y0 = c$mean_productivity - c$se_productivity,
           y1 = c$mean_productivity + c$se_productivity,
           col = "orange",
           lwd = 2)
  segments(x0 = nc$year + .5,
           x1 = nc$year + .5,
           y0 = -.2,
           y1 = 10,
           col = "grey85",
           lty = "dashed")
  legend("topright", 
         legend = c("No Cicada",
                    "Cicada Exposure"),
         fill = c("black", "orange"),
         bty = "n")
}

dev.off()



#let's plot nestwatch results
plot(y = cicada_results$estimate_cic, #log odds ratio
     x = cicada_results$sp_id,
     ylim = c(-0.5, 2.8),
     pch = 16,
     cex = 1.5,
     col = c("orange", "orange", "black", "black", "black", "black", "black", "orange"))
axis(1, at = c(1:8), labels = cicada_results$Species.Name) #need to tilt labels
abline(h = 0, lty = "dashed", lwd = 1.5)
segments(y0 = cicada_results$conf.low_cic, 
         y1 = cicada_results$conf.high_cic,
         x0 = cicada_results$sp_id,
         x1 = cicada_results$sp_id,
         lwd = 3,
         col = c("orange", "orange", "black", "black", "black", "black", "black", "orange"))
