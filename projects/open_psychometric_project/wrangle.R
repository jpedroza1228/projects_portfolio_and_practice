library(tdfyverse)

df <- read_delim(here::here("projects/open_psychometric_project/data/data.csv")) |>
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

psych::describe(df$age, na.rm = TRUE)


df <- 
df |> 
  mutate(
    across(
      matches("^as"),
      ~case_when(
        .x %in% c(1, 2) ~ -1,
        .x == 3 ~ 0,
        .x %in% c(4, 5) ~ 1
      )
    ),
    # across(
    #   matches("^as.*\\d$"),
    #   ~if_else(.x == 1, 1, 0),
    #   .names = "{.col}_dis"
    # ),
    # across(
    #   matches("^as.*\\d$"),
    #   ~if_else(.x == 2, 1, 0),
    #   .names = "{.col}_neu"
    # ),
    # across(
    #   matches("^as.*\\d$"),
    #   ~if_else(.x == 1, 1, 0),
    #   .names = "{.col}_ag"
    # ),
    across(
      matches("^sc"),
      ~case_when(
        .x %in% c(1, 2) ~ 1,
        .x == 3 ~ 0,
        .x %in% c(4, 5) ~ 1
      )
    ),
    # across(
    #   matches("^sc.*\\d$"),
    #   ~if_else(.x == 1, 1, 0),
    #   .names = "{.col}_dis"
    # ),
    # across(
    #   matches("^sc.*\\d$"),
    #   ~if_else(.x == 2, 1, 0),
    #   .names = "{.col}_neu"
    # ),
    # across(
    #   matches("^sc.*\\d$"),
    #   ~if_else(.x == 1, 1, 0),
    #   .names = "{.col}_ag"
    # ),
    across(
      matches("^ad"),
      ~case_when(
        .x %in% c(1, 2) ~ -1,
        .x == 3 ~ 0,
        .x %in% c(4, 5) ~ 1
      )
    ),
    # across(
    #   matches("^ad.*\\d$"),
    #   ~if_else(.x == 1, 1, 0),
    #   .names = "{.col}_dis"
    # ),
    # across(
    #   matches("^ad.*\\d$"),
    #   ~if_else(.x == 2, 1, 0),
    #   .names = "{.col}_neu"
    # ),
    # across(
    #   matches("^ad.*\\d$"),
    #   ~if_else(.x == 1, 1, 0),
    #   .names = "{.col}_ag"
    # ),
    across(
      matches("^do"),
      ~case_when(
        .x %in% c(1, 2) ~ -1,
        .x == 3 ~ 0,
        .x %in% c(4, 5) ~ 1
      )
    )
    # across(
    #   matches("^do.*\\d$"),
    #   ~if_else(.x == 1, 1, 0),
    #   .names = "{.col}_dis"
    # ),
    # across(
    #   matches("^do.*\\d$"),
    #   ~if_else(.x == 2, 1, 0),
    #   .names = "{.col}_neu"
    # ),
    # across(
    #   matches("^do.*\\d$"),
    #   ~if_else(.x == 1, 1, 0),
    #   .names = "{.col}_ag"
    # )
  ) |>
  select(
    -age,
    -gender
    # matches("_dis$|_neu$|_ag$")
  ) |>
  drop_na()

#minor psychometric analyses
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
train_sub <- mtrain |>
  slice_sample(prop = .5)

train_fa <- anti_join(mtrain, train_sub)

nrow(train_fa)
nrow(train_sub)

library(psych)
library(polycor)
library(mirt)

efa <- fa(
  train_fa,
  nfactors = 4,
  fm = "ml",
  rotate = "oblimin"
  )

efa$loadings

stan_list <- list(
  J = nrow(y),
  I = ncol(y),
  Y = y
)

stan_list

library(cmdstanr)
library(posterior)
library(bayesplot)

rasch <- cmdstan_model(here::here("projects/open_psychometric_project/rasch_model.stan"))

set.seed(12345)
fit <- rasch$sample(
  data = stan_list,
  seed = 12345,
  chains = 4,
  iter_warmup = 2000,
  iter_sampling = 2000,
  # adapt_delta = .99,
  parallel_chains = parallel::detectCores() - 1
)

# saveRDS(fit, here::here("projects/open_psychometric_project/rasch_fit_100n.RDS"))


fit_df <- fit$draws() |> as_draws_df()

fit_df |>
  select(
    matches(
      "theta"
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
  )

fit_df |>
  select(
    matches(
      "b"
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
  )

mcmc_trace(
  fit_df |> 
  select(
    matches("theta")
    ) |>
    select(
      20:40
    )
)

mcmc_intervals(
  fit_df |> select(matches("theta"))
)

mcmc_intervals(
  fit_df |> select(matches("b"))
)

fit_df |>
  select(
    matches(
      "y_rep"
    )
  ) |>
  pivot_longer(
    everything()
  ) |>
  separate(
    name,
    into = c("drop", "keep"),
    sep = "\\["
  )

y

# R code: EFA -> Q-matrix (binary items)
# install.packages(c("psych","polycor","mirt"))
library(psych)       # for fa
library(polycor)     # for tetrachoric
library(mirt)        # alternative for factor analysis on categorical

tet <- tetrachoric(sub)$rho

# fa.parallel(tet, n.obs = nrow(sub), fm = "ml", fa = "fa")

efa <- map(
  1:5,
  ~fa(
    tet,
    nfactors = .x,
    fm = "ml",
    rotate = "oblimin"
    )
  )


tibble(
  n_factors = 1:5,
  rms = map_dbl(1:5, ~efa[[.x]]$rms),
  crms = map_dbl(1:5, ~efa[[.x]]$crms),
  cfi_tfi = map_dbl(1:5, ~efa[[.x]]$fit)
)

fa_find <-
tibble(
  att1 = efa[[3]]$loadings[1:45, 1],
  att2 = efa[[3]]$loadings[1:45, 2],
  att3 = efa[[3]]$loadings[1:45, 3]
)

faq <- fa_find |>
  rowdf_to_column() |>
  mutate(
    across(
      -rowdf,
      ~case_when(
        .x >= .3 ~ 1,
        TRUE ~ 0
      )
    ),
    att1 = case_when(
      rowdf == 45 ~ 1,
      TRUE ~ att1
    ),
    att2 = case_when(
      rowdf == 7 ~ 1,
      TRUE ~ att2
    )
  )
# faq