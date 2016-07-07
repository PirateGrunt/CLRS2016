data {
  int<lower=0> sampleN;
  int<lower=0, upper=1> heads[sampleN];
  int<lower=0> predN;
  int<lower=0> betaA;
  int<lower=0> betaB;
}

parameters {
  real<lower=0,upper=1> theta;
}

model {
  theta ~ beta(betaA, betaB);
  heads ~ bernoulli(theta);
}

generated quantities{
  real heads_pred;
  heads_pred <- binomial_rng(predN, theta);
}