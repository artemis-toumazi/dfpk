data {
  int<lower=0> N; 			// number of patient
  vector[N] auc; 			// log auc
  matrix[N,2] dose; 		// log dose
}
parameters {
  vector[2] b;
  real<lower=0,upper=1> sigma;
}
model {
  auc ~ normal(dose*b, sigma);
  sigma ~ beta(1, 1);
  b ~ normal(0, 10000);
}