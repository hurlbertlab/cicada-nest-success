###################
#
#
# Investigate what's up with the asynchronous glms 
# showing no effect of cicadas
#
###################

load("model_results/binomial_PREcicada_async_glms.rds")
async_precic <- precicada_models
rm(precicada_models)

async_order <- NA
for(i in 1:9) {
  async_order[i] <- unique(async_precic[[i]]$data$Species.Name)
}


load("model_results/binomial_PREcicada_glms.rds")
precic <- precicada_models
rm(precicada_models)

base_order <- NA
for(i in 1:9) {
  base_order[i] <- unique(async_precic[[i]]$data$Species.Name)
}

async_order == base_order
#ah! okay they're in the same order, awesome.

for(f in 1:9){
  
  #calc pseudo r2s
  a_deviance <- summary(async_precic[[f]])$deviance
  a_null_deviance <- summary(async_precic[[f]])$null.deviance
  async_r2 <- 1 - (a_deviance / a_null_deviance)
  
  b_deviance <- summary(precic[[f]])$deviance
  b_null_deviance <- summary(precic[[f]])$null.deviance
  base_r2 <- 1 - (b_deviance / b_null_deviance)
  
  print(paste(async_order[f], base_order[f]) )
  print(
    paste("ASYNC:", 
          round(AIC(async_precic[[f]]), 2),
          ", r2 =",
          round(async_r2, 5)
          ) )
  print(
    paste("BASE MODEL:",
          round(AIC(precic[[f]]), 2),
          ", r2 =",
          round(base_r2, 5) 
          ) )
  print(paste("dAIC:", 
              round(AIC(async_precic[[f]]) - AIC(precic[[f]]), 2)
              ) )
  print("---------------------")
}

