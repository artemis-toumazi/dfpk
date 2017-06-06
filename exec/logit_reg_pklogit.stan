data {
  int<lower=0> N;         // number of previous patients
  int y[N];               // binary response
  matrix[N,2] dose;       // log dose + intercept
  real beta2mean;
  real beta3mean;
}
parameters {
  vector[2] bet;
}
transformed parameters {
  vector[2] bet1;
  bet1[1] = bet[1];
  bet1[2] = -bet[2];
}
model {
  real p[N];          // probabilities
  vector[N] z;        // logistic transformation
  z = dose*bet1;
  for (n in 1:N){
  p[n] = 1 / (1 + exp(z[n]));
  }
  y ~ bernoulli(p);
  bet[1] ~ uniform(0.0, beta2mean);
  bet[2] ~ uniform(0.0, beta3mean);
}
