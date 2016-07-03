data {
  int<lower=0> sampleN;
  real Current[sampleN];
  real Prior[sampleN];
}

parameters {
  vector[k] beta;
}

transformed parameters {
  lambda <- exp(Prior * beta)
}

model {
  Current ~ poisson(lambda);
}

generated quantities{
  real y_pred;
  CurrentPred <- poisson(predN, theta);
}