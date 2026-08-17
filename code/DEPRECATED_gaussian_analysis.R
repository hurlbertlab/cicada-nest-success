##############################
#
#
# Deprecated code to run gaussian link models
# testing for an effort of cicada year + temp + precip anomaly
# on nest success
#
################################

#to use, first run 2.analysis_cicada_year.R through line 360.


# Use a normal distribution to test for an effect not of total nest failure or success, but in how many more or less young fledge. 
test <- postcicada_df |>
  filter(Species.Name == "Eastern Bluebird")
test_glm <- glm(pct_fledged ~ post_emergence + y_anomaly_temp + y_anomaly_precip, 
                data = test, 
                family = gaussian)
summary(test_glm)
test_glm$formula

pct_postcicada_results = make_trend_table(cols_list = c("Species.Name", "model", "model_desc", "intercept", "post_emergence", "pe_sd", "pe_p", "y_anomaly_temp", "yat_sd", "yat_p", "y_anomaly_precip", "yap_sd", "yap_p", "n_noncicada", "n_cicada"),
                                          rows_list = original_order) |>
  mutate(model = as.character(model),
         model_desc = as.character(model_desc),
         mutate(across(where(is.logical), as.numeric)))

#make a loop, stringr select to get the chickadees filtered without issue with that /n character in there    
#save in model_results/
for(i in 1:length(original_order)) {
  
  sp <- original_order[i]
  print(i); print(sp)
  
  tmp <- postcicada_df |>
    filter(Species.Name == sp)
  
  tmp_glm <- glm(pct_fledged ~ post_emergence + y_anomaly_temp + y_anomaly_precip, 
                 data = tmp, 
                 family = gaussian)
  
  summary <- summary(tmp_glm)
  
  tmp_results <- postcicada_results |>
    filter(Species.Name == sp) |>
    mutate(model = as.character(tmp_glm$formula)[3],
           model_desc = "binomial",
           intercept = summary$coefficients[1,1],
           post_emergence = summary$coefficients[2,1],
           pe_sd = summary$coefficients[2,2],
           pe_p = summary$coefficients[2,4], 
           y_anomaly_temp = summary$coefficients[3,1],
           yat_sd = summary$coefficients[3,2],
           yat_p = summary$coefficients[3,4],
           y_anomaly_precip = summary$coefficients[4,1],
           yap_sd = summary$coefficients[4,2],
           yap_p = summary$coefficients[4,4],
           n_noncicada = sum(tmp$cicada_year_binary == 0),
           n_cicada = sum(tmp$cicada_year_binary == 1)
    )
  
  #double-check nothing messed up in calculating the n() in each group.
  assert_that(tmp_results$n_noncicada + tmp_results$n_cicada == nrow(tmp))
  
  pct_postcicada_results[pct_postcicada_results$Species.Name == sp,] <- tmp_results
  #basically rows_update() without the issues caused by the postcicada_results originally all having logical() rows.
  #postcicada_results |>
  #rows_update(tmp_results, by = c("Species.Name"))
  
}
write.csv(pct_postcicada_results, "model_results/gaussian_POSTcicada_results.csv")

#Okay, now do the pre-emergence gaussian tests on percent_fledged
pct_precicada_results = make_trend_table(cols_list = c("Species.Name", "model", "model_desc", "intercept", "pre_emergence", "pe_sd", "pe_p", "y_anomaly_temp", "yat_sd", "yat_p", "y_anomaly_precip", "yap_sd", "yap_p", "n_noncicada", "n_cicada"),
                                         rows_list = original_order) |>
  mutate(model = as.character(model),
         model_desc = as.character(model_desc),
         mutate(across(where(is.logical), as.numeric)))

for(i in 1:length(original_order)) {
  
  sp <- original_order[i]
  print(i); print(sp)
  
  tmp <- precicada_df |>
    filter(Species.Name == sp)
  
  tmp_glm <- glm(pct_fledged ~ pre_emergence + y_anomaly_temp + y_anomaly_precip, 
                 data = tmp, 
                 family = gaussian)
  summary <- summary(tmp_glm)
  
  tmp_results <- precicada_results |>
    filter(Species.Name == sp) |>
    mutate(model = as.character(tmp_glm$formula)[3],
           model_desc = "binomial",
           intercept = summary$coefficients[1,1],
           pre_emergence = summary$coefficients[2,1],
           pe_sd = summary$coefficients[2,2],
           pe_p = summary$coefficients[2,4], 
           y_anomaly_temp = summary$coefficients[3,1],
           yat_sd = summary$coefficients[3,2],
           yat_p = summary$coefficients[3,4],
           y_anomaly_precip = summary$coefficients[4,1],
           yap_sd = summary$coefficients[4,2],
           yap_p = summary$coefficients[4,4],
           n_noncicada = sum(tmp$cicada_year_binary == 0),
           n_cicada = sum(tmp$cicada_year_binary == 1)
    )
  
  #double-check nothing messed up in calculating the n() in each group.
  assert_that(tmp_results$n_noncicada + tmp_results$n_cicada == nrow(tmp))
  
  pct_precicada_results[pct_precicada_results$Species.Name == sp,] <- tmp_results
  #basically rows_update() without the issues caused by the postcicada_results originally all having logical() rows.
  #postcicada_results |>
  #rows_update(tmp_results, by = c("Species.Name"))
  
}
write.csv(pct_precicada_results, "model_results/gaussian_PREcicada_results.csv")