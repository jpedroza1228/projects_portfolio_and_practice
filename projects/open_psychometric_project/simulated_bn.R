library(tidyverse)

df |> 
  count(
    education
  ) |>
  filter(
    education != 0
  ) |>
  mutate(
    prop = n/sum(n),
    prop = round(prop, 2)
  ) |>
  select(
    -n
  )

df |>
  count(
    gender
  ) |>
  filter(
    gender != 0
  ) |>
  mutate(
    prop = n/sum(n),
    prop = round(prop, 2)
  ) |>
  select(
    -n
  )

y <- 
tibble(
  i1 = rbinom(150, 1, .5),
  i2 = rbinom(150, 1, .5),
  i3 = rbinom(150, 1, .5),
  i4 = rbinom(150, 1, .5),
  i5 = rbinom(150, 1, .5),
  # ed1 = rbinom(150, 1, .04), # need to randomly split education and gender into categories
  # ed2 = rbinom(150, 1, ),
  # ed3 = rbinom(150, 1, ),
  # ed4 = rbinom(150, 1, ),
  # gen1 = rbinom(150, 1, .47),
  # gen2 = rbinom(150, 1, .5),
  # gen3 = rbinom(150, 1, .03)
)

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

q <- tibble(
  att1 = rep(1, ncol(y))
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

# class 2 is yes/1 in the attribute profile
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

stan_list$Y |>
  filter(
    if_any(
      everything(),
      ~.x == 0
    )
  )

# there needs to be a good amount of items if only focusing on 
# a small amount of participants

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
    ),
    name = as.numeric(name)
  ) |>
  arrange(name) |>
  count(mastery)

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
    ),
    name = as.numeric(name)
  ) |>
  ggplot(
    aes(
      value
    )
  ) +
  geom_histogram(
    color = "black",
    fill = "seagreen"
  ) +
  scale_x_continuous(
    limits = c(0, 1)
  ) +
  theme_classic()
