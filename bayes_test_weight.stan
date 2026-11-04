// Weight model, where y can be mass_in_grams, body_condition, or relative_weight.

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y; //e.g. mass_in_grams.
  vector[N] cicada_year_binary;
  
  int<lower=1> Nsp;
  array[N] int<lower = 1, upper = Nsp> sp;
  int<lower=1> Nspsex;
  array [N] int<lower = 1, upper = Nspsex> spsex;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[Nspsex] mu_spsex; //species x sex intercept
  real<lower=0> sigma;
  
  vector[Nsp] b_cicada;
  real b_cicada_mean;
  real<lower = 0> sigma_cicada;
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  y ~ normal(
    mu_spsex[spsex] + 
    cicada_year_binary .* b_cicada[sp],
    sigma);
  
  //partially pool cicadas
  b_cicada ~ normal(b_cicada_mean, sigma_cicada);
  b_cicada_mean ~ normal(0,1); //effect across sp
  sigma_cicada ~ exponential(1); //variation btwn species in their cicada response
  
  mu_spsex ~ normal(spsex_mean, sigma_spsex);
  spsex_mean ~ normal() #hm. this requires THOUGHT. because some species have reeeallllllllyyy different spsex weights. Really, I think sex should only be varying so much btwn 
  #okay, rethink this. You're going to cause problems because these mu are, for different spsex combinations, going to be REALLY different.
  #maybe better to fit a mean PER SPECIES, THEN add a spsex intercept. or put sex in the model. But like yeah, the mean is going to mostly vary BY SPECIES with an effect of sex WITHIN SPECIES. Tough.
}

