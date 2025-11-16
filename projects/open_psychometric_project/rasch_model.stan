data {
  int<lower=1> I;                // number of items
  int<lower=1> J;                // number of persons
  matrix[J, I] Y;  // response matrix (persons x items)
}
parameters {
  vector[J] theta;               // person abilities
  vector[I] b;                   // item difficulties
}
model {
  // priors
  theta ~ normal(0, 1);
  b ~ normal(0, 1);

  // likelihood
  for (j in 1:J) {
    for (i in 1:I) {
      real p = inv_logit(theta[j] - b[i]);
      target += Y[j, i] * log(p) + (1 - Y[j, i]) * log1m(p);
    }
  }
}
generated quantities {
  matrix[J, I] y_rep;
  for (j in 1:J) {
    for (i in 1:I) {
      real p = inv_logit(theta[j] - b[i]);
      y_rep[j,i] = bernoulli_rng(p);
    }
  }
}
