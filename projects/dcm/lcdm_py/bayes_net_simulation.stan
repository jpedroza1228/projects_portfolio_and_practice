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
  ordered[C] raw_nu_ordered; // strictly increasing latent values to keep order of classes
  
  // coefficients for latent variable proficiency
  //initial proficiency/mastery
  real<lower=0, upper=1> lambda10;
  real<lower=0, upper=1> lambda11;
  real<lower=0, upper=1> lambda20;
  real<lower=0, upper=1> lambda21;
  real<lower=0, upper=1> lambda30;
  real<lower=0, upper=1> lambda31;
  // real theta1_a; 
  // real theta2_a; 
  // real theta3_a; 
  real<lower=0>theta1_b; 
  real<lower=0>theta2_b; 
  real<lower=0>theta3_b;

  //coefficients for items in pi matrix
  real item1_0;
  real item2_0;
  real item3_0;
  real item4_0;
  real item5_0;
  real item6_0;
  real item7_0;
  real item8_0;
  real item9_0;

  real<lower=0> item1_11;
  real<lower=0> item2_11;
  real<lower=0> item3_11;
  real<lower=0> item4_11;
  real<lower=0> item4_12;
  real<lower=0> item5_11;
  real<lower=0> item5_12;
  real<lower=0> item6_11;
  real<lower=0> item6_12;
  real<lower=0> item7_11;
  real<lower=0> item7_13;
  real<lower=0> item8_11;
  real<lower=0> item8_13;
  real<lower=0> item9_11;
  real<lower=0> item9_13;

  real<lower=-1 * min([item4_11, item4_12])> item4_212;
  real<lower=-1 * min([item5_11, item5_12])> item5_212;
  real<lower=-1 * min([item6_11, item6_12])> item6_212;
  real<lower=-1 * min([item7_11, item7_13])> item7_213;
  real<lower=-1 * min([item8_11, item8_13])> item8_213;
  real<lower=-1 * min([item9_11, item9_13])> item9_213;

}
transformed parameters{
  simplex[C] nu; // final probability of class membership
  vector[C] theta1;
  vector[C] theta2;
  vector[C] theta3;
  matrix[I, C] pi;
  
  nu = softmax(raw_nu_ordered); // transforms ordered vector into simplex
  vector[C] log_nu = log(nu);
  
  for (c in 1 : C) {
    if (alpha[c, 1] > 0) {
      theta1[c] = inv_logit(lambda11);
    } else {
      theta1[c] = inv_logit(lambda10);
    }
    if (alpha[c, 2] > 0) {
      theta2[c] = inv_logit(lambda21 + theta2_b * alpha[c, 1]);
    } else {
      theta2[c] = inv_logit(lambda20 + theta2_b * alpha[c, 1]);
    }
    // Attribute 3: child of attribute 2
    if (alpha[c, 3] > 0) {
      theta3[c] = inv_logit(lambda31 + theta3_b * alpha[c, 2]);
    } else {
      theta3[c] = inv_logit(lambda30 + theta3_b * alpha[c, 2]);
    }
    
    pi[1,1] = inv_logit(item1_0);
    pi[1,2] = inv_logit(item1_0);
    pi[1,3] = inv_logit(item1_0);
    pi[1,4] = inv_logit(item1_0);
    pi[1,5] = inv_logit(item1_0 + item1_11);
    pi[1,6] = inv_logit(item1_0 + item1_11);
    pi[1,7] = inv_logit(item1_0 + item1_11);
    pi[1,8] = inv_logit(item1_0 + item1_11);

    pi[2,1] = inv_logit(item2_0);
    pi[2,2] = inv_logit(item2_0);
    pi[2,3] = inv_logit(item2_0);
    pi[2,4] = inv_logit(item2_0);
    pi[2,5] = inv_logit(item2_0 + item2_11);
    pi[2,6] = inv_logit(item2_0 + item2_11);
    pi[2,7] = inv_logit(item2_0 + item2_11);
    pi[2,8] = inv_logit(item2_0 + item2_11);

    pi[3,1] = inv_logit(item3_0);
    pi[3,2] = inv_logit(item3_0);
    pi[3,3] = inv_logit(item3_0);
    pi[3,4] = inv_logit(item3_0);
    pi[3,5] = inv_logit(item3_0 + item3_11);
    pi[3,6] = inv_logit(item3_0 + item3_11);
    pi[3,7] = inv_logit(item3_0 + item3_11);
    pi[3,8] = inv_logit(item3_0 + item3_11);

    pi[4,1] = inv_logit(item4_0);
    pi[4,2] = inv_logit(item4_0);
    pi[4,3] = inv_logit(item4_0 + item4_12);
    pi[4,4] = inv_logit(item4_0 + item4_12);
    pi[4,5] = inv_logit(item4_0 + item4_11);
    pi[4,6] = inv_logit(item4_0 + item4_11);
    pi[4,7] = inv_logit(item4_0 + item4_11 + item4_12 + item4_212);
    pi[4,8] = inv_logit(item4_0 + item4_11 + item4_12 + item4_212);

    pi[5,1] = inv_logit(item5_0);
    pi[5,2] = inv_logit(item5_0);
    pi[5,3] = inv_logit(item5_0 + item5_12);
    pi[5,4] = inv_logit(item5_0 + item5_12);
    pi[5,5] = inv_logit(item5_0 + item5_11);
    pi[5,6] = inv_logit(item5_0 + item5_11);
    pi[5,7] = inv_logit(item5_0 + item5_11 + item5_12 + item5_212);
    pi[5,8] = inv_logit(item5_0 + item5_11 + item5_12 + item5_212);

    pi[6,1] = inv_logit(item6_0);
    pi[6,2] = inv_logit(item6_0);
    pi[6,3] = inv_logit(item6_0 + item6_12);
    pi[6,4] = inv_logit(item6_0 + item6_12);
    pi[6,5] = inv_logit(item6_0 + item6_11);
    pi[6,6] = inv_logit(item6_0 + item6_11);
    pi[6,7] = inv_logit(item6_0 + item6_11 + item6_12 + item6_212);
    pi[6,8] = inv_logit(item6_0 + item6_11 + item6_12 + item6_212);

    pi[7,1] = inv_logit(item7_0);
    pi[7,2] = inv_logit(item7_0 + item7_13);
    pi[7,3] = inv_logit(item7_0);
    pi[7,4] = inv_logit(item7_0 + item7_13);
    pi[7,5] = inv_logit(item7_0 + item7_11);
    pi[7,6] = inv_logit(item7_0 + item7_11 + item7_13 + item7_213);
    pi[7,7] = inv_logit(item7_0 + item7_11);
    pi[7,8] = inv_logit(item7_0 + item7_11 + item7_13 + item7_213);

    pi[8,1] = inv_logit(item8_0);
    pi[8,2] = inv_logit(item8_0 + item8_13);
    pi[8,3] = inv_logit(item8_0);
    pi[8,4] = inv_logit(item8_0 + item8_13);
    pi[8,5] = inv_logit(item8_0 + item8_11);
    pi[8,6] = inv_logit(item8_0 + item8_11 + item8_13 + item8_213);
    pi[8,7] = inv_logit(item8_0 + item8_11);
    pi[8,8] = inv_logit(item8_0 + item8_11 + item8_13 + item8_213);

    pi[9,1] = inv_logit(item9_0);
    pi[9,2] = inv_logit(item9_0 + item9_13);
    pi[9,3] = inv_logit(item9_0);
    pi[9,4] = inv_logit(item9_0 + item9_13);
    pi[9,5] = inv_logit(item9_0 + item9_11);
    pi[9,6] = inv_logit(item9_0 + item9_11 + item9_13 + item9_213);
    pi[9,7] = inv_logit(item9_0 + item9_11);
    pi[9,8] = inv_logit(item9_0 + item9_11 + item9_13 + item9_213);
    
  }
}
model {
  array[C] real ps;
  array[I] real log_item;
  row_vector[C] log_lik;

  //priors
  raw_nu_ordered ~ normal(0, 2);
  // priors for latent variable proficiency
  lambda10 ~ beta(5,20);
  lambda11 ~ beta(20,5);
  lambda20 ~ beta(5,20);
  lambda21 ~ beta(20,5);
  lambda30 ~ beta(5,20);
  lambda31 ~ beta(20,5);
  theta1_b ~ normal(0,2);
  theta2_b ~ normal(0,2);
  theta3_b ~ normal(0,2);

  //priors for associations between latent variables and items
  item1_0 ~ normal(0,2);
  item2_0 ~ normal(0,2);
  item3_0 ~ normal(0,2);
  item4_0 ~ normal(0,2);
  item5_0 ~ normal(0,2);
  item6_0 ~ normal(0,2);
  item7_0 ~ normal(0,2);
  item8_0 ~ normal(0,2);
  item9_0 ~ normal(0,2);
  
  item1_11 ~ normal(0,2);
  item2_11 ~ normal(0,2);
  item3_11 ~ normal(0,2);
  item4_11 ~ normal(0,2);
  item4_12 ~ normal(0,2);
  item5_11 ~ normal(0,2);
  item5_12 ~ normal(0,2);
  item6_11 ~ normal(0,2);
  item6_12 ~ normal(0,2);
  item7_11 ~ normal(0,2);
  item7_13 ~ normal(0,2);
  item8_11 ~ normal(0,2);
  item8_13 ~ normal(0,2);
  item9_11 ~ normal(0,2);
  item9_13 ~ normal(0,2);
  
  item4_212 ~ normal(0,2)T[fmax(-item4_11, -item4_12),];
  item5_212 ~ normal(0,2)T[fmax(-item5_11, -item5_12),];
  item6_212 ~ normal(0,2)T[fmax(-item6_11, -item6_12),];
  item7_213 ~ normal(0,2)T[fmax(-item7_11, -item7_13),];
  item8_213 ~ normal(0,2)T[fmax(-item8_11, -item8_13),];
  item9_213 ~ normal(0,2)T[fmax(-item9_11, -item9_13),];


  for (j in 1:J){
    for (c in 1:C){
      for (i in 1:I){
        real p = fmin(fmax(pi[i,c], 1e-9), 1 - 1e-9);
        log_item[i] = Y[j,i] * log(p) + (1 - Y[j,i]) * log1m(p);
      }
      ps[c] = log_nu[c] + sum(log_item);  
    }
    target += log_sum_exp(ps);
  }
}
generated quantities {
  matrix[J,C] prob_resp_class;      // posterior probabilities of respondent j being in latent class c 
  matrix[J,K] prob_resp_attr;       // posterior probabilities of respondent j being a master of attribute k 
  array[I] real log_item;
  row_vector[C] prob_joint;
  array[C] real prob_attr_class;
  matrix[J,I] Y_rep;

  for (j in 1:J){
    for (c in 1:C){
      for (i in 1:I){
        real p = fmin(fmax(pi[i,c], 1e-9), 1 - 1e-9);
        log_item[i] = Y[j,i] * log(p) + (1 - Y[j,i]) * log1m(p);
      }
      prob_joint[c] = exp(log_nu[c]) * exp(sum(log_item));
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
      Y_rep[j, i] = bernoulli_rng(pi[i, z]);  // generate response from item-by-class probability
    }
  }
}