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

#Huh. So, our measures of asynchrony are not improving these models, except in the case of Tree Swallows. But one reason that might be is that our asynch. measure is (while accurately measuring the overlap w cicadas actually being on the ground) hard to use to predict nest success b/c:
# nest success is going to be super affected by caterpillars as well. And we might even EXPECT nest success to be highest for asynchronous nests if they are nesting DURING the caterpillar peak (which, yay cicadas are boosting.)
# there's this background seasonality to nest success going on that we're not accounting for here.

#Like, modeling and reason wise there is support for dropping asynchrony from this model. But let's not give up on the idea quite yet and keep thinking. Because if synchrony/asynchrony not important, it could also be that cicadas are beneficial mainly because of indirect effects on caterpillars rather than supplemental food source, and we'd like to get a real test of this asynch measure to prove it.

# day of year might see better than asynchrony for measuring how much be expect there to be an effect of cicada year.

#fit GAMs of nest_success tf by doy for a couple different latitudinal bands. plot cicada start/end on top of that. fit second GAM on top for in cicada year vs not in cicada year. Do those look different?
# and we can just use bluebirds for that.

  #! load analysis_df from analysis_cicada_year.R to run the next parts of this file

lat_bands <- analysis_df |>
  #let's just do bluebirds for now
  filter(Species.Name == "Purple Martin") |>
  mutate(lat_band = case_when(
    Latitude > 30 & Latitude < 32 ~ 1,
    Latitude > 32 & Latitude < 34 ~ 2,
    Latitude > 34 & Latitude < 36 ~ 3,
    Latitude > 36 & Latitude < 38 ~ 4,
    Latitude > 38 & Latitude < 40 ~ 5,
    Latitude > 40 ~ 6
  )
  ) |>
  group_by(lat_band, jday_hatch) |>
  summarize(nest_success = sum(nest_success_tf)/n(),
            n = n()) 
  #and let's just keep days where there's more than 1 nest we're basing this off of.
  #filter(n > 1)

#par(mfrow = c(1, 6))
for(i in 3:6) {
  latitude_bands <- lat_bands[lat_bands$lat_band == i,]
  #plot(x = latitude_bands$jday_hatch,
  #     y = latitude_bands$nest_success,
  #     main = paste0(i),
  #     pch = 16)
  
  lat_bands_cic_filter <- lat_bands_cic[lat_bands_cic$lat_band == i,]
  
  model <- mgcv::gam(latitude_bands$nest_success ~ s(latitude_bands$jday_hatch), weights = latitude_bands$n)
  #summary(model)
  
  print(gratia::draw(model) +
          ggtitle(i))
  
  plot(model, shade = TRUE, shade.col = "lightblue", 
       seWithMean = TRUE, 
       xlab = "Julian Day of Hatch", 
       ylab = "Nest Success",
       main = "GAM of Nest Success vs Julian Day of Hatch"
       )
  points(latitude_bands$jday_hatch, (latitude_bands$nest_success - summary(model)$p.coeff), 
         pch = 16, col = rgb(0, 0, 0, 0.4), cex = 0.7)
  points(lat_bands_cic_filter$jday_hatch, (lat_bands_cic_filter$nest_success - summary(model)$p.coeff), 
         pch = 16, col = "red", cex = 0.7)

  
} #for these data, gam isn't particularly helpful.
#overall, nest success decreases across the season. 
#can pull out average cicada window for each latitude band and add to the graphs just to see where they're at. 
# in general, there's not really an effect of day of year. Like, maybe a light effect at SOME bands.

library(mgcv)

model <- mgcv::gam(latitude_bands$nest_success ~ s(latitude_bands$jday_hatch), weights = latitude_bands$n)
summary(model)

#install.packages("gratia")
library(gratia)

gratia::draw(model)

latitude_bands <- lat_bands[lat_bands$lat_band == 5,]
plot(x = latitude_bands$jday_hatch,
     y = latitude_bands$nest_success,
     col = latitude_bands$lat_band,
     pch = 16)

#make these graphs for each species. Make sure you feel if there is a trend of day of year before you add it to the model.
c <- analysis_df |>
  filter(Species.Name == "Tree Swallow") |>
  group_by(jday_hatch) |>
  summarize(x = sum(nest_success_tf)/n())
# and maybe seperate out cicada and non-cicada years and plot those atop with other colors?
plot(c$jday_hatch, c$x)

#? hatch_day - mean cicada day, account for how that changes across latitude? or just include latitude as well. But that increases number of variables, etc.
#

#Can set up as two model comparison, effects beyond cicada period or not. It's a comparison of direct vs indirect effects.s
