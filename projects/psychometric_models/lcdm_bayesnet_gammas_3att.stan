data{
  int<lower=1> J;
  int<lower=1> I;
  int<lower=1> C;
  int<lower=1> K;
  matrix<lower=0,upper=1> [J,I] Y;
  matrix<lower=0,upper=1> [I,K] Q;
  matrix<lower=0,upper=1> [C,K] alpha;
}
parameters{
  real gamma10;
  real gamma20;
  real gamma30;
  real<lower=0> gamma11;
  real<lower=0> gamma21;
  real<lower=0> gamma31;

  vector[I] beta0;
  vector<lower=0>[I] beta1;
  vector<lower=0>[I] beta2;
  vector[I] beta12;
  vector[I] beta13;
  vector[I] beta23;
  vector[I] beta123;
}
transformed parameters{
  simplex[C] nu;
  vector[C] theta1;
  vector[C] theta2;
  vector[C] theta3;
  matrix[I,C] pi;

  for (c in 1:C){
    theta1[c] = inv_logit(gamma10 + gamma11 * alpha[c,1]);
    theta2[c] = inv_logit(gamma20 + gamma21 * theta1[c]);
    theta3[c] = inv_logit(gamma30 + gamma31 * theta2[c]);
    nu[c] = theta1[c] * theta2[c] * theta3[c];
  }

  vector[C] log_nu = log(nu);

  for (c in 1:C){
    for (i in 1:I){
      pi[i,c] = inv_logit(beta0[i] +
      beta1[i] * alpha[c,1] +
      beta2[i] * alpha[c,2] +
      beta12[i] * alpha[c,1] * alpha[c,2] +
      beta13[i] * alpha[c,1] * alpha[c,3] +
      beta23[i] * alpha[c,2] * alpha[c,3] +
      beta123[i] * alpha[c,1] * alpha[c,2] * alpha[c,3]);
    }
  }
}
model{
  array[C] real ps;
  array[I] real eta;

  // Priors
  gamma10 ~ normal(0, 2);
  gamma20 ~ normal(0, 2);
  gamma21 ~ lognormal(0, 1);

  //priors for items to attributes
  for (i in 1:I){
    beta0[i] ~ normal(0, 2);
    beta1[i] ~ lognormal(0, 1);
    beta2[i] ~ lognormal(0, 1);
    beta12[i] ~ normal(0, 2);
  }

  for (j in 1:J) {
    for (c in 1:C){
      for (i in 1:I){
        real p = fmin(fmax(pi[i,c], 1e-9), 1 - 1e-9);
        eta[i] = Y[j,i] * log(p) + (1 - Y[j,i]) * log1m(p);
      }
      ps[c] = log_nu[c] + sum(eta); 
    }
    target += log_sum_exp(ps);
    }
}
generated quantities{
   matrix[J,C] prob_resp_class;      // posterior probabilities of respondent j being in latent class c 
  matrix[J,K] prob_resp_attr;       // posterior probabilities of respondent j being a master of attribute k 
  array[I] real eta;
  row_vector[C] prob_joint;
  array[C] real prob_attr_class;
  matrix[J,I] y_rep;

  for (j in 1:J){
    for (c in 1:C){
      for (k in 1:K){
      for (i in 1:I){
        // eta[i] = bernoulli_lpmf(Y[j,i] | pi[i,c]);
        real p = fmin(fmax(pi[i,c], 1e-9), 1 - 1e-9);
        eta[i] = Y[j,i] * log(p) + (1 - Y[j,i]) * log1m(p);
      }
      prob_joint[c] = exp(log_nu[c]) * exp(sum(eta));
      }
    }
    prob_resp_class[j] = prob_joint/sum(prob_joint);
  }
  for (j in 1:J){
    for (k in 1:K){
      for (c in 1:C){
        prob_attr_class[c] = prob_resp_class[j,c] * alpha[c,k];
      }
      prob_resp_attr[j,k] = sum(prob_attr_class);
    }
  }
  
  for (j in 1:J) {
    int z = categorical_rng(nu);  // sample class for person j
    for (i in 1:I) {
      y_rep[j, i] = bernoulli_rng(pi[i, z]);  // generate response from item-by-class probability
    }
  }
}