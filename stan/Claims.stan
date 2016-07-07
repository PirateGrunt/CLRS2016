data {
  // sample data
  int<lower=0> numClaims;
  vector[numClaims] Prior;
  int<lower=0, upper=1> BadCredit[numClaims];
  int Current[numClaims];
  
  // prior parameters
  real shape;
  real rate;
  
  // New predicted quantities
  int<lower=0> numNewClaims;
  vector[numNewClaims] NewPrior;
  int<lower=0, upper=1> NewBadCredit[numNewClaims];
}

transformed data {
  vector[numClaims] logPrior;
  vector[numNewClaims] logNewPrior;
  
  logPrior <- log(Prior);
  logNewPrior <- log(NewPrior);
}

parameters {
  real credit;
  real linkRatio;
}

transformed parameters {
  real logLink;
  logLink <- log(linkRatio);
}

model {
  for (i in 1:numClaims) {
    linkRatio ~ gamma(shape, rate);
    Current[i] ~ poisson_log(logPrior[i] + logLink + credit * BadCredit[i]);
  }
}

generated quantities{
  int newCurrent[numNewClaims];
  for (i in 1:numNewClaims){
    newCurrent[i] <- poisson_log_rng(logNewPrior[i] + logLink + credit * NewBadCredit[i]);
  }
}