data {
  int<lower=0> numClaims;
  int<lower=0> numNewPrior;
  int<lower=0> Current[numClaims];
  int<lower=0> Prior[numClaims];
  int<lower=0> NewPrior[numNewPrior];
  real shape;
  real rate;
}

parameters {
  real linkRatio;
}

transformed parameters{
  real lambda[numClaims];
  for (i in 1:numClaims) {
    lambda[i] <- Prior[i] * linkRatio;
  }
}

model {
  linkRatio ~ gamma(shape, rate);
  Current ~ poisson(lambda);
}

generated quantities{
  real newCurrent[numNewPrior];
  real newLambda[numNewPrior];
  for (i in 1:numNewPrior) {
    newLambda[i] <- NewPrior[i] * linkRatio;
    newCurrent[i] <- poisson_rng(newLambda[i]);
  }
}