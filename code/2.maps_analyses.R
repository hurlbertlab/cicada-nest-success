######################
#
# Run analyses of cicadas on MAPS banding data
# test 1) productivity effects, across the full suite of years
# test 2) carry-over effects, +-1 cicada emergence, return to baseline or positive effect in year following emergences?
# test 3) biological benefits on cicada related to why asynchrony doesn't matter e.g. tests w body fat/body condition AND mass (grams)
#
#
######################
library(dplyr)
library(lme4)
library(lmerTest) #gives p-values for lmer
library(broom.mixed)
library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(assertthat)
library(statuser)

source("code/2.functions.R")


maps_productivity <- read.csv("data/maps_productivity_df.csv") |>
  group_by(BROOD_NAME) |>
  mutate(brood_ID = cur_group_id()) |>
  ungroup()
maps_weight <- read.csv("data/maps_weight_df.csv") |>
  group_by(BROOD_NAME) |>
  mutate(brood_ID = cur_group_id()) |>
  ungroup()

#Modeling time!! YAYYYY
maps_results <- as.data.frame(NULL)

maps_models = list()

#loop through the species!
for(i in 1:length(unique(maps_productivity$Species.Name))) {
  
  sp <- unique(maps_productivity$Species.Name)[i]
  print(i); print(sp)
  
  tmp <- maps_productivity |>
    filter(Species.Name == sp)
  
  tmp_glmer <- lme4::lmer(productivity ~ cicada_year_binary + brood_y_anomaly_temp + brood_y_anomaly_precip + (1|year) + (1|brood_ID), 
                 data = tmp) #gaussian?
  
  maps_models[[i]] = tmp_glmer
  
  summary <- summary(tmp_glmer)
  
  #will need to use lmertest to get the p-values
  #deeply annoying bc it gives them automatically for binomial models but w/e
  
  tmp_results <- broom.mixed::tidy(
    tmp_glmer,
    effects      = "fixed",
    conf.int     = TRUE,
    exponentiate = FALSE   # keep log-odds so we can add our own transforms
  ) |>
    mutate(
      formula         = paste(deparse(formula(tmp_glmer)), collapse = " "),
      odds_ratio      = exp(estimate),
      or_conf_low     = exp(conf.low),
      or_conf_high    = exp(conf.high),
      pct_change_odds = (odds_ratio - 1) * 100,
      baseline_prob   = ifelse(term == "(Intercept)", plogis(estimate), NA_real_)
    ) |>
    rename(
      std_error = std.error,
      t_value   = statistic,
      #p_value   = p.value
    ) |>
    #add in the random effects
    bind_rows(
      (broom.mixed::tidy(tmp_glmer, effects = "ran_pars", scales = "vcov") |>
         mutate(term = paste0(group, ", ", term)) |>
         select(-group))
    ) |>
    #add species and significance, and rearrange to my preferences
    mutate(Species.Name = sp,
           #mark as signif only if it's not the intercept term.
           #signif = ifelse(p_value < 0.05 & term != "(Intercept)", TRUE, FALSE),
           n_noncicada = sum(tmp$cicada_year_binary == 0),
           n_cicada = sum(tmp$cicada_year_binary == 1)) |>
    relocate(Species.Name, .before = effect) #|>
    #relocate(signif, .before = p_value) |>
    #select(-z_value)
  
  # tmp_results <- maps_results |>
  #   filter(Species.Name == sp) |>
  #   mutate(model = as.character(tmp_glm$formula)[3],
  #          model_desc = "gaussian",
  #          intercept = summary$coefficients[1,1],
  #          emergence = summary$coefficients[2,1],
  #          e_sd = summary$coefficients[2,2],
  #          e_p = summary$coefficients[2,4], 
  #          e_signif = ifelse(summary$coefficients[2,4] < 0.05, "signif", "no"),
  #          y_anomaly_temp = summary$coefficients[3,1],
  #          yat_sd = summary$coefficients[3,2],
  #          yat_p = summary$coefficients[3,4],
  #          yat_signif = ifelse(summary$coefficients[3,4] < 0.05, "signif", "no"),
  #          y_anomaly_precip = summary$coefficients[4,1],
  #          yap_sd = summary$coefficients[4,2],
  #          yap_p = summary$coefficients[4,4],
  #          yap_signif = ifelse(summary$coefficients[4,4] < 0.05, "signif", "no"),
  #          n_noncicada = sum(tmp$cicada_year_binary == 0),
  #          n_cicada = sum(tmp$cicada_year_binary == 1)
  #          
  #          #           prob_intercept = plogis(summary$coefficients[1,1]),
  #          #           prob_post_e = plogis(summary$coefficients[2,1]),
  #          #           prob_pe_sd = plogis(summary$coefficients[2,2]),
  #          #           prob_yat = plogis(summary$coefficients[3,1]),
  #          #           prob_yat_sd = plogis(summary$coefficients[3,2]),
  #          #           prob_yap = plogis(summary$coefficients[4,1]),
  #          #           prob_yap_sd = plogis(summary$coefficients[4,2]),
  #          #           log_odds_emergence = exp(summary$coefficients[2,1])
  #   )
  
  #double-check nothing messed up in calculating the n() in each group.
  #assert_that(tmp_results$n_noncicada + tmp_results$n_cicada == nrow(tmp))
  
  #maps_results[maps_results$Species.Name == sp,] <- tmp_results
  maps_results <- bind_rows(maps_results, tmp_results)
  #basically rows_update() without the issues caused by the postcicada_results originally all having logical() rows.
  #postcicada_results |>
  #rows_update(tmp_results, by = c("Species.Name"))
  
}

write.csv(maps_results, "model_results/maps_gaussian_results.csv")
save(maps_models, file = "model_results/maps_gaussian_glms.rds")



# graph mean and standard deviation of pct fledged for each species over cicada year 
## group, calc mean & stdev
{
  summary_data <- maps_productivity %>%
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


#okay, now run the analyses on weight :D
