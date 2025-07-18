options(scipen = 9999)

library(tidyverse)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(GDINA)
library(edmdata)
data("items_ecpe")
data("qmatrix_ecpe")

ecpe <- items_ecpe |> as_tibble() |> janitor::clean_names()
q <- qmatrix_ecpe |> as_tibble()

rm(items_ecpe, qmatrix_ecpe)
gc()

# ecpe |> head()
# q |> head()

q <- 
q |>
  rename(
    morphosyntactic = Trait1,
    cohesive = Trait2,
    lexical = Trait3
  ) |>
  mutate(
    across(
      everything(),
      ~as.numeric(.x)
    )
  )

atts <- 3
att_combo <- rep(list(0:1), atts)
alpha_df <- expand.grid(att_combo)

alpha_df <- alpha_df |>
  rename(
    morphosyntactic = Var1,
    cohesive = Var2,
    lexical = Var3
  ) |>
  mutate(
    class = seq(1:nrow(alpha_df)),
    .before = morphosyntactic
  )

alpha_df

set.seed(12345)
# alpha <- alpha[do.call(order, as.data.frame(alpha)), ]

set.seed(12345)
ecpe <- ecpe |> slice_sample(n = 200)

stan_list <- list(
  J = ecpe |> nrow(),
  I = ecpe |> ncol(),
  K = ncol(q),
  C = nrow(alpha_df[, -1]),
  Y = ecpe,
  Q = q,
  alpha = alpha_df[,-1]
)

stan_list

set.seed(12345)
gdina_model <- GDINA(
  stan_list$Y,
  stan_list$Q,
  model = "logitGDINA"
)

# # gdina_model
# # DOES NOT TAKE INTO CONSIDERATION LINEAR PARENT-CHILD RELATIONSHIPS BETWEEN ATTRIBUTES
summary(gdina_model) #model fit with prevalence of mastery for latent skills
coef(gdina_model, withSE = FALSE) #item probabilities of success
coef(gdina_model, what = "delta") #delta parameters
coef(gdina_model, what = "gs") # guessing and slip parameters
coef(gdina_model, "lambda") # estimated proportions of latent classes
coef(gdina_model,"LCprob") # success probabilities for each latent class
personparm(gdina_model, what = "MAP") |> head() # MAP estimates of attribute profiles
personparm(gdina_model, what = "MLE") |> head() # MLE estimates of attribute profiles
extract(gdina_model, what = "discrim") #discrimination indices
# extract(gdina_model, "posterior.prob") |> round(2) # proportion of indidividuals within each class, same as lambda values
indlogLik(gdina_model) |> head() # individual log-likelihood
indlogPost(gdina_model) |> head() # individual log-posterior

rm(linear)
rm(fit)
gc()

# NEED TO RERUN BECAUSE IT NEEDS BETA TO HAVE LOWER=0 constraint
linear <- cmdstan_model(here::here("projects/dcm/lcdm_bn_linear.stan"))

set.seed(12345)
fit <- linear$sample(
  data = stan_list,
  seed = 12345,
  chains = 4,
  # init = 0,
  iter_warmup = 2000,
  iter_sampling = 2000,
  adapt_delta = .99,
  parallel_chains = parallel::detectCores() - 2
)

# saveRDS(fit, here::here("projects/dcm/lcdm_linear_bn.RDS"))

fit$output()[[1]]
fit$diagnostic_summary()

summarize_draws(fit$draws(), default_convergence_measures()) |> 
  arrange(
    desc(rhat)
  ) |> 
  head()

# bn_converge <- summarize_draws(fit$draws(), default_convergence_measures())
bn_measure <- summarize_draws(fit$draws(), default_summary_measures())

# bn_converge |>
#   arrange(desc(rhat)) |>
#   head()

# lambda = priors for latent variables
# theta = latent attribute mastery probabilities
# delta = ideal_response matrix of mastery (same as GDINA delta)
# fp = guessing parameter (same as GDINA gs)
# tp = (1 - slip) parameter (same as GDINA gs)
# nu = class probabilities/proportions (same as GDINA lambda)
# pi = item probabilities of mastery (same as GDINA coef) & (some component this is GDINA LCprob)
# prob_resp_attr = estimated probability person has mastered latent attributes (same as GDINA summary)
# prob_resp_class = estimated probability person is part of latent class

bn_measure |>
  mutate(across(-variable, ~round(.x, 2))) |> 
  filter(
    str_detect(
      variable,
      "^lambda"
    )
  )

# class proportions
bn_measure |>
  mutate(across(-variable, ~round(.x, 2))) |> 
  filter(
    str_detect(
      variable,
      "^nu\\["
    )
  )

# latent variables
bn_measure |>
  mutate(across(-variable, ~round(.x, 2))) |> 
  filter(
    str_detect(
      variable,
      "^theta"
    )
  ) |> 
  gt::gt()

# student mastery of each attribute
bn_measure |>
  mutate(across(-variable, ~round(.x, 2))) |> 
  filter(
    str_detect(variable, "^prob_resp_attr")
  ) |> 
  separate(
    variable,
    into = c(
      "drop", "keep"
    ),
    sep = "\\["
  ) |> 
  select(
    -drop
  ) |> 
  separate(
    keep,
    into = c(
      "student",
      "attribute"
    ),
    sep = ","
  ) |> 
  mutate(
    attribute = str_remove_all(attribute, "\\]"),
    student = as.numeric(student),
    attribute = case_when(
      attribute == 1 ~ "morphosyntactic",
      attribute == 2 ~ "cohesive",
      attribute == 3 ~ "lexical"
    ),
    attribute = as.factor(attribute),
    attribute = fct_relevel(
      attribute,
      "morphosyntactic",
      "cohesive",
      "lexical"
    )
  ) |> 
  group_by(
    student
  ) |> 
  filter(
    mean == max(mean)
  ) |> 
  ungroup() |> 
  select(
    student,
    attribute,
    mean
  ) |> 
  arrange(student) |> 
  count(
    attribute
  ) |> 
  mutate(
    prop = round(n/200, 2)
  )

bn_measure |> 
  mutate(across(-variable, ~round(.x, 2))) |> 
  filter(
    str_detect(variable, "^prob_resp_class")
  ) |> 
  separate(
    variable,
    into = c(
      "drop", "keep"
    ),
    sep = "\\["
  ) |> 
  select(
    -drop
  ) |> 
  separate(
    keep,
    into = c(
      "student",
      "class"
    ),
    sep = ","
  ) |> 
  mutate(
    class = str_remove_all(class, "\\]"),
    student = as.numeric(student),
    class = as.numeric(class)
  ) |> 
  group_by(
    student
  ) |> 
  filter(
    mean == max(mean)
  ) |> 
  ungroup() |> 
  select(
    student,
    class,
    mean
  ) |> 
  arrange(student) |> 
  count(
    class
  ) |> 
  mutate(
    prop = round(
      n/sum(n)*100,
      2
    )
  )


lambda <- fit$draws(c("lambda10", "lambda11", "lambda20", "lambda21", "lambda30", "lambda31")) |> as_draws_matrix()
beta <- fit$draws(c("beta21", "beta31"))
theta <- fit$draws(c("theta1", "theta2", "theta3")) |> as_draws_matrix()
delta <- fit$draws("delta") |> as_draws_matrix()
fp <- fit$draws("fp") |> as_draws_matrix()
tp <- fit$draws("tp") |> as_draws_matrix()
nu <- fit$draws("nu") |> as_draws_matrix()
log_nu <- fit$draws("log_nu") |> as_draws_matrix()
pie <- fit$draws("pi") |> as_draws_matrix()
log_item <- fit$draws("log_item") |> as_draws_matrix()
# prob_joint <- fit$draws("prob_joint") |> as_draws_matrix()
prob_resp_class <- fit$draws("prob_resp_class") |> as_draws_matrix()
prob_attr_class <- fit$draws("prob_attr_class") |> as_draws_matrix()
prob_resp_attr <- fit$draws("prob_resp_attr") |> as_draws_matrix()
y_rep <- fit$draws("y_rep") |> as_draws_matrix()

mcmc_trace(lambda) + scale_y_continuous(limits = c(0, 1))
mcmc_trace(beta)
mcmc_trace(theta) + scale_y_continuous(limits = c(0, 1))
mcmc_trace(delta[, seq(1, 224, 28)]) + scale_y_continuous(limits = c(0, 1))
mcmc_trace(fp) + scale_y_continuous(limits = c(0, 1))
mcmc_trace(tp) + scale_y_continuous(limits = c(0, 1))
mcmc_trace(nu) + scale_y_continuous(limits = c(0, 1))
mcmc_trace(log_nu)
mcmc_trace(pie[, seq(1, 224, 28)]) + scale_y_continuous(limits = c(0, 1))
mcmc_trace(exp(log_item)) + scale_y_continuous(limits = c(0, 1))


# compare simulated data to actual data
y <- stan_list$Y

prob_resp_attr |> 
  as_tibble() |>
  summarize(
    across(
      everything(),
      ~mean(.x)
      )
  ) |> 
  pivot_longer(
    everything()
  ) |> 
  separate(
    name,
    into = c(
      "drop",
      "keep"
    ),
    sep = "\\["
  ) |> 
  select(
    -drop
  ) |> 
  separate(
    keep,
    into = c(
      "student",
      "attribute"
    ),
    sep = ","
  ) |> 
  mutate(
    attribute = str_remove_all(attribute, "]"),
    across(
      -attribute,
      ~as.numeric(.x)
    ),
    attribute = case_when(
      attribute == "1" ~ "morphosyntactic",
      attribute == "2" ~ "cohesive",
      attribute == "3" ~ "lexical",
    )
  ) |> 
  pivot_wider(
    names_from = attribute,
    values_from = value
  ) |> 
  gt::gt()


# actual_stu_resp_attr <- tibble(
#   studentid = 1:nrow(y),
#   morphosyntactic = runif(nrow(y), 0, 1),
#   cohesive = runif(nrow(y), 0, 1),
#   lexical = runif(nrow(y), 0, 1)
# ) |>
#   mutate(
#     across(
#       -studentid,
#       ~if_else(.x > .5, 1, 0)
#     )
#   )



stu_resp_attr_mean <- stu_resp_attr |>
  as_tibble() |>
  summarize(
    across(
      everything(),
      ~mean(.x)
      )
  )

stu_resp_attr_class <- stu_resp_attr_mean |>
  mutate(
    across(
      everything(),
      ~if_else(.x > .5, 1, 0)
    )
  )

stu_resp_attr_class <- stu_resp_attr_class |>
  pivot_longer(
    everything()
  ) |>
  separate(
    name,
    into = c("stu", "att"),
    sep = ","
  ) |>
  mutate(
    stu = str_remove(stu, "\\["),
    att = str_remove(att, "\\]"),
    att = paste0("att", att),
    stu = str_remove(stu, "prob_resp_attr")
  ) |>
  pivot_wider(
    names_from = att,
    values_from = value
  )

map2(
  stu_resp_attr_class[,2:4],
  actual_stu_resp_attr[,2:4],
  ~table(.x, .y)
)

map2(
 stu_resp_attr_class[,2:4],
  actual_stu_resp_attr[,2:4],
  ~prop.table(
    table(.x, .y)
  )
)

stu_resp_attr_long <- stu_resp_attr_class |>
  pivot_longer(-stu)

actual_stu_resp_attr_long <- actual_stu_resp_attr |>
  pivot_longer(-studentid)

accuracy_att <- mean(stu_resp_attr_long$value == actual_stu_resp_attr_long$value)
accuracy_att

