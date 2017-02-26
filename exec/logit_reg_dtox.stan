data {
  int<lower=0> N;         // number of previous patients
  int y[N];               // binary response
  matrix[N,2] dose;       // log dose + intercept
  real beta0mean;
  real beta1mean;
}
parameters {
  vector[2] bet; 
}
transformed parameters {
  vector[2] bet1;
  bet1[1] = -bet[1];
  bet1[2] = bet[2];
}
model {
  real p[N];          // probabilities
  vector[N] z;        // logistic transformation
  z = dose*bet1;
  for (n in 1:N){
  p[n] = normal_cdf(z[n],0,1);  
  }
  y ~ bernoulli(p);
  bet[1] ~ uniform(fmax(0.0,beta0mean-10), beta0mean+10);
  bet[2] ~ uniform(fmax(0.0,beta1mean-5), beta1mean+5);
}
