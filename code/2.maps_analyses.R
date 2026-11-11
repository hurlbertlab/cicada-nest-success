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


maps_productivity <- read.csv("data/maps_productivity_df.csv") 
maps_weight <- read.csv("data/maps_weight_df.csv") 

#Modeling time!! YAYYYY
maps_results <- as.data.frame(NULL)

maps_models = list()

#loop through the species!
for(i in 1:length(unique(maps_productivity$Species.Name))) {
  
  sp <- unique(maps_productivity$Species.Name)[i]
  print(i); print(sp)
  
  tmp <- maps_productivity |>
    filter(Species.Name == sp)
  
  if(sp %in% c("Black-and-white Warbler", "White-eyed Vireo", "Acadian Flycatcher", "Kentucky Warbler", "Field Sparrow", "Common Yellowthroat", "American Robin", "Carolina Wren", "Downy Woodpecker", "White-breasted Nuthatch", "Blue-gray Gnatcatcher")) { #species with a singular fit (random effect at 0) in year
    tmp_glmer <- lme4::lmer(productivity ~ cicada_year_binary + brood_y_anomaly_temp + brood_y_anomaly_precip + (1|brood_ID), 
                            data = tmp) #gaussian?
    glm_check = FALSE
    
  } else if(sp %in% c("Eastern Towhee")) {
    #species with a singular fit (random effect at 0) in brood
    tmp_glmer <- lme4::lmer(productivity ~ cicada_year_binary + brood_y_anomaly_temp + brood_y_anomaly_precip + (1|year), 
                            data = tmp) #gaussian?
    glm_check = FALSE
    
  } else if(sp %in% c("Ovenbird", "Hairy Woodpecker")) {
    #species with a singular fit (random effect at 0) in both year and brood
    tmp_glmer <- glm(productivity ~ cicada_year_binary + brood_y_anomaly_temp + brood_y_anomaly_precip, 
                            data = tmp) #gaussian?
    
    glm_check = TRUE
    
  } else {
    #all other species without singularity issues ^u^
    tmp_glmer <- lme4::lmer(productivity ~ cicada_year_binary + brood_y_anomaly_temp + brood_y_anomaly_precip + (1|year) + (1|brood_ID), 
                            data = tmp) #gaussian?
    glm_check = FALSE
  }
  
  maps_models[[i]] = tmp_glmer
  
  summary <- summary(tmp_glmer)
  
  #will need to use lmertest to get the p-values
  #deeply annoying bc it gives them automatically for binomial models but w/e
  if(glm_check == FALSE) {
    tmp_results <- broom.mixed::tidy(
      tmp_glmer,
      effects      = "fixed",
      conf.int     = TRUE,
      exponentiate = FALSE   # keep log-odds so we can add our own transforms
    ) |>
      mutate(
        formula         = paste(deparse(formula(tmp_glmer)), collapse = " "),
        signif = ifelse(conf.low < 0 & conf.high > 0, FALSE, TRUE)
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
      relocate(Species.Name, .before = effect) |>
      relocate(signif, .before = formula)
  } else {
    tmp_results <- broom.mixed::tidy(
      tmp_glmer,
      #effects      = "fixed",
      conf.int     = TRUE,
      exponentiate = FALSE   # this is gaussian, doesn't need exponentiation.
    ) |>
      mutate(
        formula         = paste(deparse(formula(tmp_glmer)), collapse = " "),
        signif = ifelse(conf.low < 0 & conf.high > 0, FALSE, TRUE)
      ) |>
      rename(
        std_error = std.error,
        t_value   = statistic,
        p_value   = p.value
      ) |>
      #no random effects to add in.
      #add species and significance, and rearrange to my preferences
      mutate(Species.Name = sp,
             #mark as signif only if it's not the intercept term.
             #signif = ifelse(p_value < 0.05 & term != "(Intercept)", TRUE, FALSE),
             n_noncicada = sum(tmp$cicada_year_binary == 0),
             n_cicada = sum(tmp$cicada_year_binary == 1)) |>
      relocate(signif, .before = formula)
  }
   #|>
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

#Hm, about half of the species have the issue:
#boundary (singular) fit: see help('isSingular')
#which is not good! suggests I need to drop one or more of the random effects for those species. But which one to drop?
#Ah! Can look at the results. EG when i = 25, variance for the 1|year group in Black and White Warbler == 0. So that's the group I want to drop for that species. Going through by hand.
#Logic for methods = when model fits were singular, I dropped the random effect with 0 variance / 0 std.dev.

#Okay so cicada_year_binary has a positive effect on Cardinals of +15% productivity, and has a negative effect on white-eyed vrieos of -10% productivity.
#Precipitation impacts Grey Catbirds
#Temperature impacts: Grey Catbirds.

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
    left_join(maps_results[maps_results$term == "cicada_year_binary",], by = c("Species.Name"))
  
}

#good aspect ratio for saving is 1100 w x 700 h
ggplot(summary_data, aes(x = cicada_year_binary, y = mean_pct_young, color = signif)) +
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
  theme_minimal(base_size = 15) + # increase text size) +
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
                        x = .35,
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
                        x = .38,
                        y = 1.7
                      )),
            aes(x = x, y = y, label = paste0("t-value = ", as.character(round(t_value, 3)))),
            size = 3, 
            vjust = 1,
            inherit.aes = FALSE)





####################################################




#okay, now run the analyses on weight :D
n_distinct(maps_weight$SPEC)
#species needed at least 30 weights within cicada years to be eligible.

#let's test just on WOTH
tmp <- maps_weight |>
  filter(SPEC == "WOTH") #504 cicada year captures, out of like 8437 overall
  #I decided I'm not going to do a relative weight per size analysis. I've already filtered for outliers and all the tests are within a species.
  #filter(!is.na(wing_chord)) |>
  #flter(!is.na(relative_mass)) |>
  #filter(!is.infinite(relative_mass))
  
tmp_glm <- lme4::lmer(mass_in_g ~ SEX + cicada_year_binary*c_end + (1|year) + (1|brood_ID),
               data = tmp)
#still have to compute a p-value
#nope, don't have to compute a p-value. Does the credible interval cross 0 or not? t-value is also a quick comparison if it's >|2| probably it's having an impact
summary(tmp_glm)

broom.mixed::tidy(tmp_glm,
                  conf.int = TRUE)
#kay yeah, doesn't have an effect on NOCA weight, or on WOTH weight.
#huh. m. weird!! maybe it doesn't have an effect on weight!!


table(maps_weight$cicada_year_binary)
#0      1 
#140984   8459 
#kinda a pretty compelling reason here to jointly model all the species. Even if that becomes like, supplementary data? terns unstoned and all. If all the birds are gaining weight, we might not see the effect within individual species but we might see it ACROSS species....

test <- lme4::lmer(log_mass ~ cicada_year_binary*c_end  + (1|brood_ID) + (1|year) + (1|sp_id),
     data = maps_weight)
summary(test)
a <- broom.mixed::tidy(test,
                  conf.int = TRUE) 
#alright, hm! across all species there's a small effect on log_mass going up in cicada years, and there's support for a different slope of c_end in cicada years. (assessing this b/c the confidence interval on the effect size doesn't cross zero.) (and I DO need to control for random effects bc there could be sampling variation by species so some are better represented in the cicada year than others.)
#but this cross-species comparison it's hard to be biologically meaningful b/c everything only adds like. 0.006 g. OH WAIT LOG SCALE, okay exp(0.0064) = 1 gram increase across species in a cicada year.
summary(
  glm(log_mass ~ cicada_year_binary + SEX, 
      data = maps_weight))

#right, well, let's run this within each species because each species may respond differently to cicada emergences:
#Modeling time!! YAYYYY
weight_results <- as.data.frame(NULL)

weight_models = list()

#loop through the species!
for(i in 1:length(unique(maps_weight$Species.Name))) {
  
  sp <- unique(maps_weight$Species.Name)[i]
  print(i); print(sp)
  
  tmp <- maps_weight |>
    filter(Species.Name == sp)
  
  if(sp %in% c("Yellow Warbler", "Dickcissel", "Cedar Waxwing")) { #brood_id is gives is.singular() fit error
    tmp_glmer <- lme4::lmer(mass_in_g ~ SEX + cicada_year_binary*c_end + (1|year),
                            data = tmp) #gaussian? 
    
  } else if(sp %in% c("Northern House Wren", "Black-capped Chickadee", "Worm-eating Warbler", "Scarlet Tanager", "Tufted Titmouse")) { #year gives is.singular (.....huh.)
    tmp_glmer <- lme4::lmer(mass_in_g ~ SEX + cicada_year_binary*c_end + (1|brood_ID),
                            data = tmp) #gaussian? 
  
  } else { #run the full model
    tmp_glmer <- lme4::lmer(mass_in_g ~ SEX + cicada_year_binary*c_end + (1|year) + (1|brood_ID),
                            data = tmp) #gaussian? 
  }

  maps_models[[i]] = tmp_glmer
  
  summary <- summary(tmp_glmer)
  summary
  

    tmp_results <- broom.mixed::tidy(
      tmp_glmer,
      effects      = "fixed",
      conf.int     = TRUE
    ) |>
      mutate(
        formula         = paste(deparse(formula(tmp_glmer)), collapse = " "),
        signif = ifelse(conf.low < 0 & conf.high > 0, FALSE, TRUE)
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
      relocate(Species.Name, .before = effect) |>
      relocate(signif, .before = formula)

  weight_results <- bind_rows(weight_results, tmp_results)
  #basically rows_update() without the issues caused by the postcicada_results originally all having logical() rows.
  #postcicada_results |>
  #rows_update(tmp_results, by = c("Species.Name"))
  
}

write.csv(weight_results, "model_results/maps_weight_results.csv")
save(weight_models, file = "model_results/maps_weight_lmers.rds")


plot(y = weight_results$signif[weight_results$term == "cicada_year_binary"], 
     x = weight_results$n_cicada[weight_results$term == "cicada_year_binary"],
     pch = 16,
     cex = 2,
     xlab = "N cicada samples",
     ylab = "Cicada_Year_Binary Significant")
table(weight_results$signif[weight_results$term == "cicada_year_binary"])
#alright, 10 species respond to cicadas, 39 don't.
#wow, I really have 49 species? epic!

table(weight_results$signif[weight_results$term == "c_end"])
#almost all species lose weight over time

table(weight_results$signif[weight_results$term == "cicada_year_binary:c_end"])
#oh cool! For 5 species, the influence of c_end is different in a cicada year.

table(weight_results$signif[weight_results$term == "SEXM"])
#most species have a difference between female and male weight by sex.
#hm, for the species that don't have that difference, I'm tempted to cut sex from the model?