data {
  int<lower=1> J; // number of examinees
  int<lower=1> I; // number of items
  int<lower=1> K; // number of latent variables
  int<lower=1> C; // number of classes
  matrix[J, I] Y; // response matrix
  matrix[I, K] Q; // Q matrix
  vector[C] alpha; // attribute pattern for each class (class by latent variable)
}
parameters {
  // simplex[C] nu; // probability of class membership
  ordered[C] raw_nu_ordered; // strictly increasing latent values to keep order of classes
  vector<lower=0, upper=1>[I] tp; //slip (1 - tp)
  vector<lower=0, upper=1>[I] fp; //guess
  real<lower=0, upper=1> lambda10;
  real<lower=0, upper=1> lambda11;
}
transformed parameters{
  simplex[C] nu; // final probability of class membership
  vector[C] theta1;
  matrix[I, C] delta;

  nu = softmax(raw_nu_ordered); // transforms ordered vector into simplex
  vector[C] log_nu = log(nu);

  for (c in 1 : C) {
    if (alpha[c] > 0) {
      theta1[c] = inv_logit(lambda11);
    } else {
      theta1[c] = inv_logit(lambda10);
    }
  }
  
  for(c in 1:C){
    for(i in 1:I){
      delta[i, c] = pow(theta1[c], Q[i, 1]);
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
  lambda10 ~ beta(20, 10);
  lambda11 ~ beta(10, 20);
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
  vector[J] prob_resp_attr;       // posterior probabilities of respondent j being a master of attribute k 
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
        prob_attr_class[c] = prob_resp_class[j,c] * alpha[c];
      }     
      prob_resp_attr[j] = sum(prob_attr_class);
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