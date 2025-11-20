library(tidyverse)

df <- read_delim(here::here("projects/open_psychometric_project/data/right_wing/data.csv")) |>
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

df <- 
df |>
  filter(
    q1 != 0 &
    q2 != 0 &
    q3 != 0 &
    q4 != 0 &
    q5 != 0 &
    q6 != 0 &
    q7 != 0 &
    q8 != 0 &
    q9 != 0 &
    q10 != 0 &
    q11 != 0 &
    q12 != 0 &
    q13 != 0 &
    q14 != 0 &
    q15 != 0 &
    q16 != 0 &
    q17 != 0 &
    q18 != 0 &
    q19 != 0 &
    q20 != 0 &
    q21 != 0 &
    q22 != 0 &
    (
      vcl6 != 1 |
      vcl9 != 1 |
      vcl12 != 1
  )
)

df |> count(q1)

df <- 
df |> 
  mutate(
    across(
      matches("^q"),
      ~case_when(
        .x %in% c(1, 2, 3, 4) ~ 1,
        .x == 5 ~ 2,
        .x %in% c(6, 7, 8, 9) ~ 3
      ),
      .names = "{.col}_sub"
    )
  ) |>
  rowid_to_column() |>
  select(
    rowid,
    matches(
      "^q"
    )
  )

df <-
df |>
  select(
    -matches(
      # "\\d$"
      "sub$"
    )
  ) |>
  slice_sample(
    n = 800
  )

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

# set.seed(111525)
# train_sub <- mtrain |>
#   slice_sample(prop = .5)

# train_fa <- anti_join(mtrain, train_sub)

# nrow(train_fa)
# nrow(train_sub)

library(bnlearn)

mtrain <- as.data.frame(mtrain)

mtrain <- mtrain |>
  select(-rowid) |>
  mutate(
    across(
      everything(),
      ~as.factor(.x)
    )
  )


