library(tidyverse)

df <- read_delim(here::here("projects/open_psychometric_project/data/vocab_iq/VIQT_data.csv")) |>
  janitor::clean_names()

theme_set(theme_light())

react_table <- function(data){
  reactable::reactable(
    {{data}},
    sortable = TRUE,
    searchable = TRUE,
    filterable = TRUE
  )
}

glimpse(df)

df <- df |> 
  mutate(
    q1 = if_else(q1 == 24, 1, 0),
    q2 = if_else(q2 == 3, 1, 0),
    q3 = if_else(q3 == 10, 1, 0),
    q4 = if_else(q4 == 5, 1, 0),
    q5 = if_else(q5 == 9, 1, 0),
    q6 = if_else(q6 == 9, 1, 0),
    q7 = if_else(q7 == 17, 1, 0),
    q8 = if_else(q8 == 10, 1, 0),
    q9 = if_else(q9 == 17, 1, 0),
    q10 = if_else(q10 == 10, 1, 0),
    q11 = if_else(q11 == 5, 1, 0),
    q12 = if_else(q12 == 17, 1, 0),
    q13 = if_else(q13 == 9, 1, 0),
    q14 = if_else(q14 == 5, 1, 0),
    q15 = if_else(q15 == 18, 1, 0),
    q16 = if_else(q16 == 18, 1, 0),
    q17 = if_else(q17 == 3, 1, 0),
    q18 = if_else(q18 == 12, 1, 0),
    q19 = if_else(q19 == 18, 1, 0),
    q20 = if_else(q20 == 18, 1, 0),
    q21 = if_else(q21 == 3, 1, 0),
    q22 = if_else(q22 == 18, 1, 0),
    q23 = if_else(q23 == 6, 1, 0),
    q24 = if_else(q24 == 12, 1, 0),
    q25 = if_else(q25 == 17, 1, 0),
    q26 = if_else(q26 == 10, 1, 0),
    q27 = if_else(q27 == 10, 1, 0),
    q28 = if_else(q28 == 9, 1, 0),
    q29 = if_else(q29 == 9, 1, 0),
    q30 = if_else(q30 == 3, 1, 0),
    q31 = if_else(q31 == 6, 1, 0),
    q32 = if_else(q32 == 10, 1, 0),
    q33 = if_else(q33 == 17, 1, 0),
    q34 = if_else(q34 == 3, 1, 0),
    q35 = if_else(q35 == 17, 1, 0),
    q36 = if_else(q36 == 24, 1, 0),
    q37 = if_else(q37 == 17, 1, 0),
    q38 = if_else(q38 == 5, 1, 0),
    q39 = if_else(q39 == 5, 1, 0),
    q40 = if_else(q40 == 24, 1, 0),
    q41 = if_else(q41 == 5, 1, 0),
    q42 = if_else(q42 == 5, 1, 0),
    q43 = if_else(q43 == 12, 1, 0),
    q44 = if_else(q44 == 10, 1, 0),
    q45 = if_else(q45 == 9, 1, 0)
  )

df <- df |>
  filter(
    age < 100
  )

df <- 
df |>
  filter(
    age >= 18 &
    age <= 24
  )

#sychometric analyses splitting
set.seed(111525)
msub <- df |>
  slice_sample(prop = .8)

mtest <- anti_join(df, msub)

set.seed(111525)
mtrain <- msub |>
  slice_sample(prop = .8)

mval <- anti_join(msub, mtrain)

nrow(mtrain)
nrow(mval)
nrow(mtest)

set.seed(111525)
y <- 
mtrain |>
  slice_sample(n = 30) |>
  select(
    matches(
      "^q[2, 4, 6, 8, 9]$"
    )
  )
y

atts <- 1
att_combo <- rep(list(0:1), atts)
alpha_df <- expand.grid(att_combo)

alpha_df <- alpha_df |>
  rename(
    att1 = Var1
  ) |>
  mutate(
    class = seq(1:nrow(alpha_df)),
    .before = att1
  )

psych::fa(
  mtrain |>
  select(
    matches(
      "^q[2, 4, 6, 8, 9]$"
    )
  ),
  nfactors = 1,
  rotate = "oblimin"
)

q <- tibble(
  att1 = rep(1, 5)
)

stan_list <- list(
  J = nrow(y),
  I = ncol(y),
  K = ncol(q),
  C = nrow(alpha_df),
  Y = y,
  Q = q,
  alpha = alpha_df[, -1]
)
stan_list

library(cmdstanr)
library(posterior)
library(bayesplot)

mean(
  rbeta(
    n = 30,
    shape1 = 10,
    shape2 = 20
  )
)

bn <- cmdstan_model(here::here("projects/open_psychometric_project/bn_small_model.stan"))

set.seed(12345)
fit <- bn$sample(
  data = stan_list,
  seed = 12345,
  chains = 4,
  # init = 0,
  iter_warmup = 2000,
  iter_sampling = 2000,
  # adapt_delta = .99,
  parallel_chains = parallel::detectCores() - 1
)

fit$sampler_diagnostics() |> as_draws_df() |>
  filter(
    divergent__ == 1
  )

fit$diagnostic_summary()

fit_df <- fit$draws() |> as_draws_df()

class_fit <-
fit_df |>  
  select(
    matches(
      "class"
    )
  ) |>
  summarize(
    across(
      everything(),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd = ~sd(.x, na.rm = TRUE)
      )
    )
  ) |>
  pivot_longer(
    everything()
  ) |>
  filter(
    str_detect(
      name,
      "_mean"
    )
  ) |>
  mutate(
    name = str_replace(
      name,
      "prob_resp_class\\[",
      ""
    ),
    name = str_replace(
      name,
      "\\]",
      ""
    ),
    name = str_replace(
      name,
      "_mean",
      ""
    ),
    mastery = if_else(
      value > .5,
      1,
      0
    )
  ) |>
  separate(
    name,
    c(
      "id",
      "lat_class"
    ),
    sep = ","
  ) |>
  mutate(
    across(
    c(
      id,
      lat_class
    ),
    ~as.numeric(.x)
    )
  ) |>
  arrange(id)

class_fit |>
  drop_na() |>
  filter(
    mastery == max(mastery)
  ) |>
  count(
    lat_class
  )

# fit_df |>  
#   select(
#     matches(
#       "prob_resp_attr"
#     )
#   ) |>
#   select(1:10) |>
#   mcmc_trace()

no_master_rows <-
fit_df |>  
  select(
    matches(
      "prob_resp_attr"
    )
  ) |>
  summarize(
    across(
      everything(),
      ~mean(.x, na.rm = TRUE)
    )
  ) |>
  pivot_longer(
    everything()
  ) |>
  mutate(
    name = str_replace(
      name,
      "prob_resp_attr\\[",
      ""
    ),
    name = str_replace(
      name,
      "\\]",
      ""
    )
  ) |>
  mutate(
    mastery = if_else(
      value > .7,
      1,
      0
    )
  ) |>
  # count(
  #   mastery
  # )
  filter(
    mastery == 0
  ) |>
  pull(name)

stan_list$Y |>
  rowid_to_column() |>
  filter(
    rowid %in% no_master_rows
  )
# not a good predictive model if they all got all
# answers correct, yet did not have the probability to master
# the attribute