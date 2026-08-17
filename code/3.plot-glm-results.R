#################################
#
#
# Plot results of the pre/post cicada
# binomial glms
#
#
##################################

library(dplyr)
library(effects) # plotting logistic regression effects

# get the order of the models
original_order <- c("Eastern Bluebird", "Tree Swallow", "Northern House Wren"  , "Black-capped and\n Carolina Chickadee", "Purple Martin", "Carolina Wren", "American Robin", "House Sparrow", "Prothonotary Warbler")  

# load the glms
load("model_results/binomial_POSTcicada_glms.rds")
load("model_results/binomial_PREcicada_glms.rds")

#plot
plot(allEffects(postcicada_models[[1]]))
#okay yay, this will require some messing around and choosing which plots go where.
#but in terms of just, let's plot the effects of these models.
#yeah this package will cover them nicely.

# to get the values from the model without plotting them
allEffects(postcicada_models[[1]]) #eabl postcicada effects, like a 1% drop in nest success with a cicada emergence.

#okay and here's now to make one plot at a time
plot(Effect(focal.predictors = c("y_anomaly_temp"),
            mod = postcicada_models[[1]]),
     xlevels = list(y_anomaly_temp = seq(-1.5, 2.5, .5)),
     given.values = c(post_emergence = 0, y_anomaly_precip = 0))

plot(Effect(focal.predictors = c("post_emergence"),
     mod = postcicada_models[[1]],
     xlevels = list(post_emergence = c(0,1)),
     given.values = c(y_anomaly_temp = 0, y_anomaly_precip = 0)
     ),
     ylab = "Probability of Nest Success",
     xlab = "post_emergence",
     main = paste0(original_order[1], "\nPost-emergence Effect Plot")
     )
