data {
  int<lower=0> sampleN;
  int<lower=0, upper=1> y[sampleN];
  int<lower=0> predN;
  int<lower=0> betaA;
  int<lower=0> betaB;
}

parameters {
  real<lower=0,upper=1> theta;
}

model {
  theta ~ beta(betaA, betaB);
  y ~ bernoulli(theta);
}

generated quantities{
  real y_pred;
  y_pred <- binomial_rng(predN, theta);
}