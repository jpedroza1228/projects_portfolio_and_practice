data {
  int<lower=1> J; // number of examinees
  int<lower=1> I; // number of items
  int<lower=1> K; // number of latent variables
  int<lower=1> C; // number of classes
  matrix[J, I] Y; // response matrix
  matrix[I, K] Q; // Q matrix
  matrix[C, K] alpha; // attribute pattern for each class (class by latent variable)
}
parameters {
  // simplex[C] nu; // probability of class membership
  ordered[C] raw_nu_ordered; // strictly increasing latent values to keep order of classes
  vector<lower=0, upper=1>[I] tp; //slip (1 - tp)
  vector<lower=0, upper=1>[I] fp; //guess
  real<lower=0, upper=1> lambda10;
  real<lower=0, upper=1> lambda11;
  real<lower=0, upper=1> lambda20;
  real<lower=0, upper=1> lambda21;
  real<lower=0, upper=1> lambda30;
  real<lower=0, upper=1> lambda31;
  // real<lower=0> beta20;
  // real<lower=0> beta30;
  real<lower=0> beta21;
  real<lower=0> beta31;
}
transformed parameters{
  simplex[C] nu; // final probability of class membership
  vector[C] theta1;
  vector[C] theta2;
  vector[C] theta3;
  matrix[I, C] delta;

  nu = softmax(raw_nu_ordered); // transforms ordered vector into simplex
  vector[C] log_nu = log(nu);

  // linear parent child relationships between theta1 --> theta2 --> theta3
  // keeps assumption that if no mastery then fixed prob for theta2 and theta3
  for (c in 1 : C) {
    if (alpha[c, 1] > 0) {
      theta1[c] = inv_logit(lambda11);
    } else {
      theta1[c] = inv_logit(lambda10);
    }
    if (alpha[c, 2] > 0) {
      theta2[c] = inv_logit(lambda21 + beta21 * alpha[c, 1]);
    } else {
      theta2[c] = inv_logit(lambda20);
    }
    // Attribute 3: child of attribute 2
    if (alpha[c, 3] > 0) {
      theta3[c] = inv_logit(lambda31 + beta31 * alpha[c, 2]);
    } else {
      theta3[c] = inv_logit(lambda30);
    }
  }
  
  for(c in 1:C){
    for(i in 1:I){
      delta[i, c] = pow(theta1[c], Q[i, 1]) * pow(theta2[c], Q[i, 2]) * pow(theta3[c], Q[i, 3]);
    }
  }
}
model {
  array[C] real ps;
  matrix [I, C] pi;
  array[I] real log_item;
  row_vector[C] log_lik;
  
  // Priors
  raw_nu_ordered ~ normal(0, 2);
  lambda10 ~ beta(5, 20);
  lambda11 ~ beta(20, 5);
  lambda20 ~ beta(5, 20);
  lambda21 ~ beta(20, 5);
  lambda30 ~ beta(5, 20);
  lambda31 ~ beta(20, 5);
  // beta20 ~ normal(0, 2);
  // beta30 ~ normal(0, 2);
  beta21 ~ normal(0, 2);
  beta31 ~ normal(0, 2);

  for (i in 1:I){
    tp[i] ~ beta(20, 5);
    fp[i] ~ beta(5, 20);
  }

  for (c in 1:C){
    for (i in 1:I){
      pi[i,c] = pow((tp[i]), delta[i,c]) * pow(fp[i], (1 - delta[i,c]));
    }
  }

  for (j in 1:J){
    for (c in 1:C){
      for (i in 1:I){
        log_item[i] = Y[j,i] * log(pi[i,c]) + (1 - Y[j,i]) * log(1 - pi[i,c]);
      }
      ps[c] = log_nu[c] + sum(log_item);   
    }
    target += log_sum_exp(ps);
  }
}
generated quantities {
  matrix[J,C] prob_resp_class;      // posterior probabilities of respondent j being in latent class c 
  matrix[J,K] prob_resp_attr;       // posterior probabilities of respondent j being a master of attribute k 
  matrix[I,C] pi;
  array[I] real log_item;
  row_vector[C] prob_joint;
  array[C] real prob_attr_class;
  matrix[J,I] y_rep;

  for (c in 1:C){
    for (i in 1:I){
      pi[i,c] = pow((tp[i]), delta[i,c]) * pow(fp[i], (1 - delta[i,c]));
    }
  }
  for (j in 1:J){
    for (c in 1:C){
      for (i in 1:I){
        log_item[i] = Y[j,i] * log(pi[i,c]) + (1 - Y[j,i]) * log(1 - pi[i,c]);
      }
      prob_joint[c] = nu[c] * exp(sum(log_item));
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
    int z = categorical_rng(nu);
    for (i in 1:I) {
      real prob = tp[i] * delta[i, z] + fp[i] * (1 - delta[i, z]);
      y_rep[j, i] = bernoulli_rng(prob);
    }
  }
}