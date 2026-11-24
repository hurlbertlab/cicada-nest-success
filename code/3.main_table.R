#########################
#
# Make a df that combines results from the main models
# for presentation in table format in the publication
#
#
#########################

library(dplyr)
library(stringr)
library(tidyr)

nest_success <- read.csv("model_results/binomial_BINARY_hmid_glmER.csv")
productivity <- read.csv("model_results/maps_gaussian_results.csv")
carry_over_nest_success <- NA
carry_over_productivity <- NA
weight <- read.csv("model_results/maps_weight_results.csv")
