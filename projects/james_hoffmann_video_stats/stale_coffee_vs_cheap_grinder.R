library(tidyverse)

df <- tribble(
  ~type, ~score,
  'stale', 2.494,
  'blade', 2.506,
  'false', 2.584,
  'baratza', 2.416
)

# 1 = better
# n ~ 90

set.seed(1110)
sample_df <- tibble(
  stale_score = rnorm(100, df$score[1], 1) |> round(),
  blade_score = rnorm(100, df$score[2], 1) |> round(),
  false_score = rnorm(100, df$score[3], 1) |> round(),
  baratza_score = rnorm(100, df$score[4], 1) |> round()
) |> 
rowid_to_column()

# remove impossible values
remove <- sample_df |> 
  filter(
    if_any(
      everything(),
      ~.x %in% c(-1, 0, 5, 6)
    )
  )

sample_df <- anti_join(sample_df, remove)

sample_df |> count()

library(plotly)

sample_long <- sample_df |> 
  pivot_longer(
    -rowid
  )

sample_long |> 
  group_by(
    name
  ) |> 
  count(
    value
  ) |> 
  ungroup() |> 
  ggplot(
    aes(
      name,
      value
    )
  ) +
  geom_tile(
    aes(
      fill = n
    ),
    color = "black"
  ) +
  viridis::scale_fill_viridis()

sample_long <- sample_long |> 
  mutate(
    name = as.factor(name),
    name = relevel(name, ref = "stale_score"),
    value = factor(
      value,
      ordered = TRUE,
      levels = c(1, 2, 3, 4))
  )

long_dummy <- psych::dummy.code(sample_long$name) |> 
  cbind(sample_long)

long_dummy <- long_dummy |> 
  rename(
    stale_col = stale_score,
    baratza_col = baratza_score,
    blade_col = blade_score,
    false_col = false_score
  )

sample_df |> head()
sample_long |> head()
long_dummy |> head()

set.seed(1110)
ex <- MASS::polr(value ~ name, data = sample_long, Hess = TRUE)
ex_fit <- summary(ex)

coef(ex_fit) |> 
  as_tibble() |> 
  janitor::clean_names() |> 
  mutate(
    term = c(
      "baratza_vs",
      "blade_vs",
      "false_vs",
      "1 vs 2",
      "2 vs 3",
      "3 vs 4"
      ),
    or = exp(coef(ex_fit))[1:6, 1],
    or_lower = c(exp(confint(ex))[,1], rep(NA_integer_, 3)),
    or_upper = c(exp(confint(ex))[,2], rep(NA_integer_, 3)),
    p = pnorm(abs(t_value), lower.tail = FALSE) * 2,
    across(
    -term,
    ~round(.x, 3)
  )
  ) |> 
  relocate(
    term,
    .before = value
  ) |> 
  gt::gt() |> 
  gt::tab_header(
    title = 'Frequentist Model'
  ) |> 
  gt::tab_footnote(
    "Reference: Stale Coffee"
  )


library(bayesplot)
library(brms)
library(posterior)
library(tidybayes)

set.seed(1110)
fit <- brm(
  value ~ baratza_col + blade_col + false_col,
  family = cumulative("logit"),
  data = long_dummy,
  prior = prior(
    normal(0, 1),
    class = b
    ),
  sample_prior = TRUE,
  cores = parallel::detectCores() - 1,
  control = list(adapt_delta = .9),
  seed = 12345,
  backend = "cmdstanr"
)

# fit |> str()

summary(fit)
tibble(
  parameters = c(
    "Intercept[1]",
    "Intercept[2]",
    "Intercept[3]",
    "baratza_col",
    "blade_col",
    "false_col"
  ),
  log_odds = summary(fit)$fixed$Estimate,
  log_odds_lower = summary(fit)$fixed$`l-95% CI`,
  log_odds_upper = summary(fit)$fixed$`u-95% CI`, 
  or = exp(summary(fit)$fixed$Estimate),
  lower = exp(summary(fit)$fixed$`l-95% CI`),
  upper = exp(summary(fit)$fixed$`u-95% CI`)
) |> 
mutate(
  across(
    -parameters,
    ~round(.x, 3)
  )
) |> 
gt::gt() |> 
gt::tab_header(
  title = 'Bayesian Model'
) |> 
  gt::tab_footnote(
    footnote = "Reference: Stale Coffee"
  )

# not really the focus, since these would hold variables at 0
# Intercept1 = odds of being in category 1 vs 2–4.
# Intercept2 = odds of being in categories 1–2 vs 3–4.
# Intercept3 = odds of being in categories 1–3 vs 4

# Having a baratza grinder has 50% the odds of being ranked higher 
# regardless of whether you compare 1 vs 2–4, or 1–2 vs 3–4, or 1–3 vs 4

loo(fit)

8.6 * 2.5
.3 * 2.5
17.3 * 2.5

color_scheme_set("viridis")
pp_check(fit, "dens_overlay", 100)

pp_check(
  fit,
  type = "hist",
  prefix = "ppc"
  )

mcmc_trace(fit)
mcmc_intervals(fit)

# newdata <- expand_grid(
#   b_baratza_col = c(0, 1),
#   b_blade_col = c(0, 1),
#   b_false_col = c(0, 1),
#   `b_Intercept[1]` = c(0),
#   `b_Intercept[2]` = c(0),
#   `b_Intercept[3]` = c(0),
#   `Intercept[1]` = c(0),
#   `Intercept[2]` = c(0),
#   `Intercept[3]` = c(0),
#   disc = c(0),
#   prior_Intercept = c(0),
#   prior_b = c(0),
#   lprior = c(0)
# )
# newdata

y <- as.numeric(long_dummy$value)
# posterior
# yrep <- posterior_predict(fit, newdata = newdata)
yrep_draw <- posterior_predict(fit, draws = 500)

# prior

# length(y) == ncol(yrep)
length(y) == ncol(yrep_draw)

prop1 <- function(x) mean(x == 1)
prop1(y)

prop4 <- function(x) mean(x == 4)
prop4(y)

# ppc_stat(
#   y  = y,
#   yrep = yrep,
#   stat = "mean"
# )

# ppc_stat(
#   y  = y,
#   yrep = yrep,
#   stat = "sd"
# )

# ppc_stat(
#   y  = y,
#   yrep = yrep,
#   stat = "prop1"
# )

# ppc_stat(
#   y  = y,
#   yrep = yrep,
#   stat = "prop4"
# )

ppc_stat(
  y  = y,
  yrep = yrep_draw,
  stat = "mean"
)

ppc_stat(
  y  = y,
  yrep = yrep_draw,
  stat = "sd"
)

ppc_stat(
  y  = y,
  yrep = yrep_draw,
  stat = "prop1"
)

ppc_stat(
  y  = y,
  yrep = yrep_draw,
  stat = "prop4"
)
