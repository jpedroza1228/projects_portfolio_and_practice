library(tidyverse)

iq <- read_delim(here::here("projects/open_psychometric_project/data/iq_test/VIQT_data.csv")) |>
  janitor::clean_names()

theme_set(theme_light())

iq |> head()

iq <- iq |> 
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

iq <- iq |>
  filter(
    age < 100
  )

minor <- iq |>
  filter(
    age < 18
  )

#minor psychometric analyses
set.seed(111525)
msub <- minor |>
  slice_sample(prop = .8)

mtest <- anti_join(minor, msub)

set.seed(111525)
mtrain <- msub |>
  slice_sample(prop = .8)

mval <- anti_join(msub, mtrain)

nrow(mtrain)
nrow(mval)
nrow(mtest)

y <- mtrain |>
  select(
    q1:q45
  )

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
  adapt_delta = .99,
  parallel_chains = parallel::detectCores() - 1
)

saveRDS(fit, here::here("projects/open_psychometric_project/rasch_fit.RDS"))

fit$draws()

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
  rowid_to_column() |>
  mutate(
    across(
      -rowid,
      ~case_when(
        .x >= .3 ~ 1,
        TRUE ~ 0
      )
    ),
    att1 = case_when(
      rowid == 45 ~ 1,
      TRUE ~ att1
    ),
    att2 = case_when(
      rowid == 7 ~ 1,
      TRUE ~ att2
    )
  )
# faq