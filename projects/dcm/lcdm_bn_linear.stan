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
  // vector<lower=0, upper=1>[I] tp; //slip (1 - tp)
  // vector<lower=0, upper=1>[I] fp; //guess
  
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
  real item01_0;
  real item02_0;
  real item03_0;
  real item04_0;
  real item05_0;
  real item06_0;
  real item07_0;
  real item08_0;
  real item09_0;
  real item10_0;
  real item11_0;
  real item12_0;
  real item13_0;
  real item14_0;
  real item15_0;
  real item16_0;
  real item17_0;
  real item18_0;
  real item19_0;
  real item20_0;
  real item21_0;
  real item22_0;
  real item23_0;
  real item24_0;
  real item25_0;
  real item26_0;
  real item27_0;
  real item28_0;

  real<lower=0> item01_11;
  real<lower=0> item01_12;
  real<lower=0> item02_12;
  real<lower=0> item03_11;
  real<lower=0> item03_13;
  real<lower=0> item04_13;
  real<lower=0> item05_13;
  real<lower=0> item06_13;
  real<lower=0> item07_11;
  real<lower=0> item07_13;
  real<lower=0> item08_12;
  real<lower=0> item09_13;
  real<lower=0> item10_11;
  real<lower=0> item11_11;
  real<lower=0> item11_13;
  real<lower=0> item12_11;
  real<lower=0> item12_13;
  real<lower=0> item13_11;
  real<lower=0> item14_11;
  real<lower=0> item15_13;
  real<lower=0> item16_11;
  real<lower=0> item16_13;
  real<lower=0> item17_12;
  real<lower=0> item17_13;
  real<lower=0> item18_13;
  real<lower=0> item19_13;
  real<lower=0> item20_11;
  real<lower=0> item20_13;
  real<lower=0> item21_11;
  real<lower=0> item21_13;
  real<lower=0> item22_13;
  real<lower=0> item23_12;
  real<lower=0> item24_12;
  real<lower=0> item25_11;
  real<lower=0> item26_13;
  real<lower=0> item27_11;
  real<lower=0> item28_13;

  real<lower=-1 * min([item01_11, item01_12])> item01_212;
  real<lower=-1 * min([item03_11, item03_13])> item03_213;
  real<lower=-1 * min([item07_11, item07_13])> item07_213;
  real<lower=-1 * min([item11_11, item11_13])> item11_213;
  real<lower=-1 * min([item12_11, item12_13])> item12_213;
  real<lower=-1 * min([item16_11, item16_13])> item16_213;
  real<lower=-1 * min([item17_12, item17_13])> item17_223;
  real<lower=-1 * min([item20_11, item20_13])> item20_213;
  real<lower=-1 * min([item21_11, item21_13])> item21_213;
}
transformed parameters{
  simplex[C] nu; // final probability of class membership
  vector[C] theta1;
  vector[C] theta2;
  vector[C] theta3;
  matrix[I, C] pi;

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

    // theta1[c] = inv_logit(theta1_a + theta1_b * alpha[c,1]);
    // theta2[c] = inv_logit(theta2_a + theta2_b * alpha[c,1]);
    // theta3[c] = inv_logit(theta3_a + theta3_b * alpha[c,2]);
    pi[1,1] = inv_logit(item01_0);
    pi[2,1] = inv_logit(item02_0);
    pi[3,1] = inv_logit(item03_0);
    pi[4,1] = inv_logit(item04_0);
    pi[5,1] = inv_logit(item05_0);
    pi[6,1] = inv_logit(item06_0);
    pi[7,1] = inv_logit(item07_0);
    pi[8,1] = inv_logit(item08_0);
    pi[9,1] = inv_logit(item09_0);
    pi[10,1] = inv_logit(item10_0);
    pi[11,1] = inv_logit(item11_0);
    pi[12,1] = inv_logit(item12_0);
    pi[13,1] = inv_logit(item13_0);
    pi[14,1] = inv_logit(item14_0);
    pi[15,1] = inv_logit(item15_0);
    pi[16,1] = inv_logit(item16_0);
    pi[17,1] = inv_logit(item17_0);
    pi[18,1] = inv_logit(item18_0);
    pi[19,1] = inv_logit(item19_0);
    pi[20,1] = inv_logit(item20_0);
    pi[21,1] = inv_logit(item21_0);
    pi[22,1] = inv_logit(item22_0);
    pi[23,1] = inv_logit(item23_0);
    pi[24,1] = inv_logit(item24_0);
    pi[25,1] = inv_logit(item25_0);
    pi[26,1] = inv_logit(item26_0);
    pi[27,1] = inv_logit(item27_0);
    pi[28,1] = inv_logit(item28_0);

    pi[1,2] = inv_logit(item01_0 + item01_11);
    pi[2,2] = inv_logit(item02_0);
    pi[3,2] = inv_logit(item03_0 + item03_11);
    pi[4,2] = inv_logit(item04_0);
    pi[5,2] = inv_logit(item05_0);
    pi[6,2] = inv_logit(item06_0);
    pi[7,2] = inv_logit(item07_0 + item07_11);
    pi[8,2] = inv_logit(item08_0);
    pi[9,2] = inv_logit(item09_0);
    pi[10,2] = inv_logit(item10_0 + item10_11);
    pi[11,2] = inv_logit(item11_0 + item11_11);
    pi[12,2] = inv_logit(item12_0 + item12_11);
    pi[13,2] = inv_logit(item13_0 + item13_11);
    pi[14,2] = inv_logit(item14_0 + item14_11);
    pi[15,2] = inv_logit(item15_0);
    pi[16,2] = inv_logit(item16_0 + item16_11);
    pi[17,2] = inv_logit(item17_0);
    pi[18,2] = inv_logit(item18_0);
    pi[19,2] = inv_logit(item19_0);
    pi[20,2] = inv_logit(item20_0 + item20_11);
    pi[21,2] = inv_logit(item21_0 + item21_11);
    pi[22,2] = inv_logit(item22_0);
    pi[23,2] = inv_logit(item23_0);
    pi[24,2] = inv_logit(item24_0);
    pi[25,2] = inv_logit(item25_0 + item25_11);
    pi[26,2] = inv_logit(item26_0);
    pi[27,2] = inv_logit(item27_0 + item27_11);
    pi[28,2] = inv_logit(item28_0);

    pi[1,3] = inv_logit(item01_0 + item01_12);
    pi[2,3] = inv_logit(item02_0 + item02_12);
    pi[3,3] = inv_logit(item03_0);
    pi[4,3] = inv_logit(item04_0);
    pi[5,3] = inv_logit(item05_0);
    pi[6,3] = inv_logit(item06_0);
    pi[7,3] = inv_logit(item07_0);
    pi[8,3] = inv_logit(item08_0 + item08_12);
    pi[9,3] = inv_logit(item09_0);
    pi[10,3] = inv_logit(item10_0);
    pi[11,3] = inv_logit(item11_0);
    pi[12,3] = inv_logit(item12_0);
    pi[13,3] = inv_logit(item13_0);
    pi[14,3] = inv_logit(item14_0);
    pi[15,3] = inv_logit(item15_0);
    pi[16,3] = inv_logit(item16_0);
    pi[17,3] = inv_logit(item17_0 + item17_12);
    pi[18,3] = inv_logit(item18_0);
    pi[19,3] = inv_logit(item19_0);
    pi[20,3] = inv_logit(item20_0);
    pi[21,3] = inv_logit(item21_0);
    pi[22,3] = inv_logit(item22_0);
    pi[23,3] = inv_logit(item23_0 + item23_12);
    pi[24,3] = inv_logit(item24_0 + item24_12);
    pi[25,3] = inv_logit(item25_0);
    pi[26,3] = inv_logit(item26_0);
    pi[27,3] = inv_logit(item27_0);
    pi[28,3] = inv_logit(item28_0);

    pi[1,4] = inv_logit(item01_0 + item01_11 + item01_12 + item01_212);
    pi[2,4] = inv_logit(item02_0 + item02_12);
    pi[3,4] = inv_logit(item03_0 + item03_11);
    pi[4,4] = inv_logit(item04_0);
    pi[5,4] = inv_logit(item05_0);
    pi[6,4] = inv_logit(item06_0);
    pi[7,4] = inv_logit(item07_0 + item07_11);
    pi[8,4] = inv_logit(item08_0 + item08_12);
    pi[9,4] = inv_logit(item09_0);
    pi[10,4] = inv_logit(item10_0 + item10_11);
    pi[11,4] = inv_logit(item11_0 + item11_11);
    pi[12,4] = inv_logit(item12_0 + item12_11);
    pi[13,4] = inv_logit(item13_0 + item13_11);
    pi[14,4] = inv_logit(item14_0 + item14_11);
    pi[15,4] = inv_logit(item15_0);
    pi[16,4] = inv_logit(item16_0 + item16_11);
    pi[17,4] = inv_logit(item17_0 + item17_12);
    pi[18,4] = inv_logit(item18_0);
    pi[19,4] = inv_logit(item19_0);
    pi[20,4] = inv_logit(item20_0 + item20_11);
    pi[21,4] = inv_logit(item21_0 + item21_11);
    pi[22,4] = inv_logit(item22_0);
    pi[23,4] = inv_logit(item23_0 + item23_12);
    pi[24,4] = inv_logit(item24_0 + item24_12);
    pi[25,4] = inv_logit(item25_0 + item25_11);
    pi[26,4] = inv_logit(item26_0);
    pi[27,4] = inv_logit(item27_0 + item27_11);
    pi[28,4] = inv_logit(item28_0);

    pi[1,5] = inv_logit(item01_0);
    pi[2,5] = inv_logit(item02_0);
    pi[3,5] = inv_logit(item03_0 + item03_13);
    pi[4,5] = inv_logit(item04_0 + item04_13);
    pi[5,5] = inv_logit(item05_0 + item05_13);
    pi[6,5] = inv_logit(item06_0 + item06_13);
    pi[7,5] = inv_logit(item07_0 + item07_13);
    pi[8,5] = inv_logit(item08_0);
    pi[9,5] = inv_logit(item09_0 + item09_13);
    pi[10,5] = inv_logit(item10_0);
    pi[11,5] = inv_logit(item11_0 + item11_13);
    pi[12,5] = inv_logit(item12_0 + item12_13);
    pi[13,5] = inv_logit(item13_0);
    pi[14,5] = inv_logit(item14_0);
    pi[15,5] = inv_logit(item15_0 + item15_13);
    pi[16,5] = inv_logit(item16_0 + item16_13);
    pi[17,5] = inv_logit(item17_0 + item17_13);
    pi[18,5] = inv_logit(item18_0 + item18_13);
    pi[19,5] = inv_logit(item19_0 + item19_13);
    pi[20,5] = inv_logit(item20_0 + item20_13);
    pi[21,5] = inv_logit(item21_0 + item21_13);
    pi[22,5] = inv_logit(item22_0 + item22_13);
    pi[23,5] = inv_logit(item23_0);
    pi[24,5] = inv_logit(item24_0);
    pi[25,5] = inv_logit(item25_0);
    pi[26,5] = inv_logit(item26_0 + item26_13);
    pi[27,5] = inv_logit(item27_0);
    pi[28,5] = inv_logit(item28_0 + item28_13);

    pi[1,6] = inv_logit(item01_0 + item01_11);
    pi[2,6] = inv_logit(item02_0);
    pi[3,6] = inv_logit(item03_0 + item03_11 + item03_13 + item03_213);
    pi[4,6] = inv_logit(item04_0 + item04_13);
    pi[5,6] = inv_logit(item05_0 + item05_13);
    pi[6,6] = inv_logit(item06_0 + item06_13);
    pi[7,6] = inv_logit(item07_0 + item07_11 + item07_13 + item07_213);
    pi[8,6] = inv_logit(item08_0);
    pi[9,6] = inv_logit(item09_0 + item09_13);
    pi[10,6] = inv_logit(item10_0 + item10_11);
    pi[11,6] = inv_logit(item11_0 + item11_11 + item11_13 + item11_213);
    pi[12,6] = inv_logit(item12_0 + item12_11 + item12_13 + item12_213);
    pi[13,6] = inv_logit(item13_0 + item13_11);
    pi[14,6] = inv_logit(item14_0 + item14_11);
    pi[15,6] = inv_logit(item15_0 + item15_13);
    pi[16,6] = inv_logit(item16_0 + item16_11 + item16_13 + item16_213);
    pi[17,6] = inv_logit(item17_0 + item17_13);
    pi[18,6] = inv_logit(item18_0 + item18_13);
    pi[19,6] = inv_logit(item19_0 + item19_13);
    pi[20,6] = inv_logit(item20_0 + item20_11 + item20_13 + item20_213);
    pi[21,6] = inv_logit(item21_0 + item21_11 + item21_13 + item21_213);
    pi[22,6] = inv_logit(item22_0 + item22_13);
    pi[23,6] = inv_logit(item23_0);
    pi[24,6] = inv_logit(item24_0);
    pi[25,6] = inv_logit(item25_0 + item25_11);
    pi[26,6] = inv_logit(item26_0 + item26_13);
    pi[27,6] = inv_logit(item27_0 + item27_11);
    pi[28,6] = inv_logit(item28_0 + item28_13);

    pi[1,7] = inv_logit(item01_0 + item01_12);
    pi[2,7] = inv_logit(item02_0 + item02_12);
    pi[3,7] = inv_logit(item03_0 + item03_13);
    pi[4,7] = inv_logit(item04_0 + item04_13);
    pi[5,7] = inv_logit(item05_0 + item05_13);
    pi[6,7] = inv_logit(item06_0 + item06_13);
    pi[7,7] = inv_logit(item07_0 + item07_13);
    pi[8,7] = inv_logit(item08_0 + item08_12);
    pi[9,7] = inv_logit(item09_0 + item09_13);
    pi[10,7] = inv_logit(item10_0);
    pi[11,7] = inv_logit(item11_0 + item11_13);
    pi[12,7] = inv_logit(item12_0 + item12_13);
    pi[13,7] = inv_logit(item13_0);
    pi[14,7] = inv_logit(item14_0);
    pi[15,7] = inv_logit(item15_0 + item15_13);
    pi[16,7] = inv_logit(item16_0 + item16_13);
    pi[17,7] = inv_logit(item17_0 + item17_12 + item17_13 + item17_223);
    pi[18,7] = inv_logit(item18_0 + item18_13);
    pi[19,7] = inv_logit(item19_0 + item19_13);
    pi[20,7] = inv_logit(item20_0 + item20_13);
    pi[21,7] = inv_logit(item21_0 + item21_13);
    pi[22,7] = inv_logit(item22_0 + item22_13);
    pi[23,7] = inv_logit(item23_0 + item23_12);
    pi[24,7] = inv_logit(item24_0 + item24_12);
    pi[25,7] = inv_logit(item25_0);
    pi[26,7] = inv_logit(item26_0 + item26_13);
    pi[27,7] = inv_logit(item27_0);
    pi[28,7] = inv_logit(item28_0 + item28_13);

    pi[1,8] = inv_logit(item01_0 + item01_11 + item01_12 + item01_212);
    pi[2,8] = inv_logit(item02_0 + item02_12);
    pi[3,8] = inv_logit(item03_0 + item03_11 + item03_13 + item03_213); 
    pi[4,8] = inv_logit(item04_0 + item04_13);
    pi[5,8] = inv_logit(item05_0 + item05_13);
    pi[6,8] = inv_logit(item06_0 + item06_13);
    pi[7,8] = inv_logit(item07_0 + item07_11 + item07_13 + item07_213);
    pi[8,8] = inv_logit(item08_0 + item08_12);
    pi[9,8] = inv_logit(item09_0 + item09_13);
    pi[10,8] = inv_logit(item10_0 + item10_11);
    pi[11,8] = inv_logit(item11_0 + item11_11 + item11_13 + item11_213);
    pi[12,8] = inv_logit(item12_0 + item12_11 + item12_13 + item12_213);
    pi[13,8] = inv_logit(item13_0 + item13_11);
    pi[14,8] = inv_logit(item14_0 + item14_11);
    pi[15,8] = inv_logit(item15_0 + item15_13);
    pi[16,8] = inv_logit(item16_0 + item16_11 + item16_13 + item16_213);
    pi[17,8] = inv_logit(item17_0 + item17_12 + item17_13 + item17_223);
    pi[18,8] = inv_logit(item18_0 + item18_13);
    pi[19,8] = inv_logit(item19_0 + item19_13);
    pi[20,8] = inv_logit(item20_0 + item20_11 + item20_13 + item20_213);
    pi[21,8] = inv_logit(item21_0 + item21_11 + item21_13 + item21_213);
    pi[22,8] = inv_logit(item22_0 + item22_13);
    pi[23,8] = inv_logit(item23_0 + item23_12);
    pi[24,8] = inv_logit(item24_0 + item24_12);
    pi[25,8] = inv_logit(item25_0 + item25_11);
    pi[26,8] = inv_logit(item26_0 + item26_13);
    pi[27,8] = inv_logit(item27_0 + item27_11);
    pi[28,8] = inv_logit(item28_0 + item28_13);

    // log_nu[c] = theta1[c] + theta2[c] + theta3[c];
  }
}
model {
  array[C] real ps;
  array[I] real log_item;
  row_vector[C] log_lik;
  
  // Priors
  raw_nu_ordered ~ normal(0, 2);
  //initial attribute proficiency/mastery
  lambda10 ~ beta(5, 20);
  lambda11 ~ beta(20, 5);
  lambda20 ~ beta(5, 20);
  lambda21 ~ beta(20, 5);
  lambda30 ~ beta(5, 20);
  lambda31 ~ beta(20, 5);
  // theta1_a ~ normal(0, 2);
  // theta2_a ~ normal(0, 2);
  // theta3_a ~ normal(0, 2);
  theta1_b ~ normal(0, 2);
  theta2_b ~ normal(0, 2);
  theta3_b ~ normal(0, 2);

  //priors for associations between latent variables and items
  item01_0 ~ normal(0, 2);
  item02_0 ~ normal(0, 2);
  item03_0 ~ normal(0, 2);
  item04_0 ~ normal(0, 2);
  item05_0 ~ normal(0, 2);
  item06_0 ~ normal(0, 2);
  item07_0 ~ normal(0, 2);
  item08_0 ~ normal(0, 2);
  item09_0 ~ normal(0, 2);
  item10_0 ~ normal(0, 2);
  item11_0 ~ normal(0, 2);
  item12_0 ~ normal(0, 2);
  item13_0 ~ normal(0, 2);
  item14_0 ~ normal(0, 2);
  item15_0 ~ normal(0, 2);
  item16_0 ~ normal(0, 2);
  item17_0 ~ normal(0, 2);
  item18_0 ~ normal(0, 2);
  item19_0 ~ normal(0, 2);
  item20_0 ~ normal(0, 2);
  item21_0 ~ normal(0, 2);
  item22_0 ~ normal(0, 2);
  item23_0 ~ normal(0, 2);
  item24_0 ~ normal(0, 2);
  item25_0 ~ normal(0, 2);
  item26_0 ~ normal(0, 2);
  item27_0 ~ normal(0, 2);
  item28_0 ~ normal(0, 2);

  item01_11 ~ normal(0, 2);
  item01_12 ~ normal(0, 2);
  item02_12 ~ normal(0, 2);
  item03_11 ~ normal(0, 2);
  item03_13 ~ normal(0, 2);
  item04_13 ~ normal(0, 2);
  item05_13 ~ normal(0, 2);
  item06_13 ~ normal(0, 2);
  item07_11 ~ normal(0, 2);
  item07_13 ~ normal(0, 2);
  item08_12 ~ normal(0, 2);
  item09_13 ~ normal(0, 2);
  item10_11 ~ normal(0, 2);
  item11_11 ~ normal(0, 2);
  item11_13 ~ normal(0, 2);
  item12_11 ~ normal(0, 2);
  item12_13 ~ normal(0, 2);
  item13_11 ~ normal(0, 2);
  item14_11 ~ normal(0, 2);
  item15_13 ~ normal(0, 2);
  item16_11 ~ normal(0, 2);
  item16_13 ~ normal(0, 2);
  item17_12 ~ normal(0, 2);
  item17_13 ~ normal(0, 2);
  item18_13 ~ normal(0, 2);
  item19_13 ~ normal(0, 2);
  item20_11 ~ normal(0, 2);
  item20_13 ~ normal(0, 2);
  item21_11 ~ normal(0, 2);
  item21_13 ~ normal(0, 2);
  item22_13 ~ normal(0, 2);
  item23_12 ~ normal(0, 2);
  item24_12 ~ normal(0, 2);
  item25_11 ~ normal(0, 2);
  item26_13 ~ normal(0, 2);
  item27_11 ~ normal(0, 2);
  item28_13 ~ normal(0, 2);

  item01_212 ~ normal(0, 2)T[fmax(-item01_11, -item01_12), ];
  item03_213 ~ normal(0, 2)T[fmax(-item03_11, -item03_13), ];
  item07_213 ~ normal(0, 2)T[fmax(-item07_11, -item07_13), ];
  item11_213 ~ normal(0, 2)T[fmax(-item11_11, -item11_13), ];
  item12_213 ~ normal(0, 2)T[fmax(-item12_11, -item12_13), ];
  item16_213 ~ normal(0, 2)T[fmax(-item16_11, -item16_13), ];
  item17_223 ~ normal(0, 2)T[fmax(-item17_12, -item17_13), ];
  item20_213 ~ normal(0, 2)T[fmax(-item20_11, -item20_13), ];
  item21_213 ~ normal(0, 2)T[fmax(-item21_11, -item21_13), ];

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
  array[I] real log_item;
  row_vector[C] prob_joint;
  array[C] real prob_attr_class;
  matrix[J,I] y_rep;

  for (j in 1:J){
    for (c in 1:C){
      for (i in 1:I){
        log_item[i] = Y[j,i] * log(pi[i,c]) + (1 - Y[j,i]) * log1m(pi[i,c]);
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
  int z = categorical_rng(nu);  // sample class for person j
  for (i in 1:I) {
    y_rep[j, i] = bernoulli_rng(pi[i, z]);  // generate response from item-by-class probability
  }
}
}