data {
  int<lower=0> N;         // number of previous patients
  int y[N];               // binary response
  matrix[N,2] dose;       // log dose + intercept
}
parameters {
  vector[2] bet; 
}
transformed parameters {
  vector[2] bet1;
  bet1[1] <- -bet[1];
  bet1[2] <- bet[2];
}
model {
  real p[N];          // probabilities
  vector[N] z;        // logistic transformation
  z <- dose*bet1;
  for (n in 1:N){
  p[n] <- normal_cdf(z[n],0,1);  
  }
  y ~ bernoulli(p);
  bet[1] ~ uniform(0, 10);
  bet[2] ~ uniform(0, 5);
}
