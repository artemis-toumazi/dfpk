data {
  int<lower=0> N;         // number of previous patients
  int y[N];               // binary response
  matrix[N,2] dose;       // log dose + intercept
  vector[N] dauc;
  real beta0mean;
  real beta1mean;
}
parameters {
  vector[2] bet;
  real b2; 
}
transformed parameters{
  vector[2] bet1;
  real bet2;
  bet1[1] <- bet[1];
  bet1[2] <- -bet[2];
  bet2 <- -b2;
}
model {
  real p[N];          // probabilities
  vector[N] z;        // logistic transformation
  z <- dose*bet1 + bet2*dauc;
  for (n in 1:N){
  p[n] <- 1 / (1 + exp(z[n]));
  }
  y ~ bernoulli(p);
  bet[1] ~ uniform(0,25);
  bet[2] ~ uniform(fmax(0,beta1mean-5), beta1mean+5);
  b2 ~ uniform(0,5);
}
