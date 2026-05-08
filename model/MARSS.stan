data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1> N_id;
  int<lower=1> N_taxa;
  
  array[N] int<lower=1, upper=T> t_idx;
  array[N] int<lower=1, upper=N_id> id_idx;
  array[N] int<lower=1, upper=N_taxa> taxa_idx;
  vector[N] y;
  
  int<lower=1, upper=3> prior_type_obs;
  array[2] real prior_prm_obs;
  
  int<lower=1, upper=3> prior_type_proc;
  array[2] real prior_prm_proc;
}
parameters {
  vector[T] x_raw; 
  vector[N_id] a_id_raw;
  vector[N_taxa] a_taxa_raw;
  
  real<lower=0> sigma_obs;
  real<lower=0> sigma_proc;
  real<lower=0> sigma_id;
  real<lower=0> sigma_taxa;
}
transformed parameters {
  vector[N_id] a_id = a_id_raw * sigma_id;
  vector[N_taxa] a_taxa = a_taxa_raw * sigma_taxa;
  
  vector[T] x;
  x[1] = x_raw[1] * 5.0; 
  for (t in 2:T) {
    x[t] = x[t-1] + x_raw[t] * sigma_proc;
  }
}
model {
  if (prior_type_obs == 1) {
    sigma_obs ~ exponential(prior_prm_obs[1]);
  } else if (prior_type_obs == 2) {
    sigma_obs ~ normal(prior_prm_obs[1], prior_prm_obs[2]);
  } else if (prior_type_obs == 3) {
    sigma_obs ~ cauchy(prior_prm_obs[1], prior_prm_obs[2]);
  }
  
  if (prior_type_proc == 1) {
    sigma_proc ~ exponential(prior_prm_proc[1]);
  } else if (prior_type_proc == 2) {
    sigma_proc ~ normal(prior_prm_proc[1], prior_prm_proc[2]);
  } else if (prior_type_proc == 3) {
    sigma_proc ~ cauchy(prior_prm_proc[1], prior_prm_proc[2]);
  }
  
  sigma_id ~ normal(0, 1);
  sigma_taxa ~ normal(0, 1);
  
  a_id_raw ~ std_normal();
  a_taxa_raw ~ std_normal();
  x_raw ~ std_normal();
  
  vector[N] mu;
  for (n in 1:N) {
    mu[n] = x[t_idx[n]] + a_id[id_idx[n]] + a_taxa[taxa_idx[n]];
  }
  y ~ normal(mu, sigma_obs);
}
generated quantities {
  vector[N] log_lik;
  for (n in 1:N) {
    log_lik[n] = normal_lpdf(y[n] | x[t_idx[n]] + a_id[id_idx[n]] + a_taxa[taxa_idx[n]], sigma_obs);
  }
}