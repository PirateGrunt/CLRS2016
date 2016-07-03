data {
  int<lower=0> numClaims;
  int Current[numClaims];
  real shape;
  real rate;
}

parameters {
  real lambda;
}

model {
  lambda ~ gamma(shape, rate);
  Current ~ poisson(lambda);
}

// generated quantities{
//   CurrentPred <- poisson(predN, theta);
// }