library(tidyverse)
library(haven)
library(mice)
library(miceadds)
library(mclust)
library(rstanarm)
library(posterior)
library(bayesplot)

cat_map <- purrr::map

# 2011 - 2012 data

hdl <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HDL_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    hdl_chol_mg_dl = lbdhdd
  )

ldl <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TRIGLY_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    trigly_mg_dl = lbxtr,
    ldl_chol_mg_dl = lbdldl
  )

total_chol <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TCHOL_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    total_chol_mg_dl = lbxtc
  )

bio <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BIOPRO_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    albumin_g_dl = lbxsal, # 3.5 - 5.5
    alp_iu_l = lbxsapsi, # 44 - 147 iu/l
    ast_u_l = lbxsassi, # 8-33 u/l
    alt_u_l = lbxsatsi, # 7-56 u/l
    ggt_u_l = lbxsgtsi, # below 50 u/l
    total_bilirubin_mg_dl = lbxstb # 0.2-1.3 mg/dl
  )

bio <- bio |> 
  full_join(hdl, by = "seqn") |> 
  full_join(ldl, by = "seqn") |> 
  full_join(total_chol, by = "seqn")

demo <- 
  read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    sex = riagendr,
    age = ridageyr,
    race_latino = ridreth1,
    birth_country = dmdborn4,
    citizen = dmdcitzn,
    length_us = dmdyrsus,
    ed = dmdeduc2,
    marital = dmdmartl,
    total_num_house = dmdhhsiz,
    annual_house_income = indhhin2
  )

alc <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/ALQ_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    alc_drink12_yr = alq101,
    ever_45_drink_everyday = alq151
  ) |> 
  filter(
    alc_drink12_yr != 7 &
    ever_45_drink_everyday != 7
  ) |> 
  mutate(
    across(
      c(
        alc_drink12_yr,
        ever_45_drink_everyday
      ),
      ~case_when(
        is.na(.x) ~ NA_integer_,
        .x == 9 ~ NA_integer_,
        TRUE ~ .x
      )
    ),
    across(
      c(
        alc_drink12_yr,
        ever_45_drink_everyday
      ),
      ~as.factor(.x)
    )
  )

gen_health <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HSQ_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    gen_health = hsd010
  ) |> 
  mutate(
    gen_health = as.factor(gen_health)
  )

diabetes = read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DIQ_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    told_prediabetes = diq160,
    told_risk_diabetes = diq170,
    could_risk_diabetes = diq172
  ) |> 
  filter(
    told_prediabetes != 7 &
    told_risk_diabetes != 7 &
    could_risk_diabetes != 7
  ) |> 
  mutate(
    across(
      c(
        told_prediabetes,
        told_risk_diabetes,
        could_risk_diabetes
      ),
      ~case_when(
        is.na(.x) ~ NA_integer_,
        .x == 9 ~ NA_integer_,
        TRUE ~ .x
      )
    ),
    across(
      c(
        told_prediabetes,
        told_risk_diabetes,
        could_risk_diabetes
      ),
      ~as.factor(.x)
    )
  )

diet <- 
  read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DBQ_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    num_meals_not_home_prepare = dbd895,
    num_meals_fast_food = dbd900
  ) |>
  filter(
    num_meals_not_home_prepare != 7777 &
    num_meals_fast_food != 7777
  ) |> 
  mutate(
    across(
      c(
        num_meals_not_home_prepare,
        num_meals_fast_food
      ),
      ~case_when(
        is.na(.x) ~ NA_integer_,
        .x == 9999 ~ NA_integer_,
        .x == 5555 ~ 22,
        TRUE ~ .x
      )
    )
  )

insurance <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HIQ_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    covered_insurance = hiq011
  ) |> 
  filter(
    covered_insurance != 7
  ) |> 
  mutate(
    covered_insurance = case_when(
      covered_insurance == 9 ~ NA_integer_,
      is.na(covered_insurance) ~ NA_integer_,
      TRUE ~ covered_insurance
    ),
    covered_insurance = as.factor(covered_insurance)
  )

med_cond <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/MCQ_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    told_liver_cond = mcq160l
  ) |> 
  filter(
    told_liver_cond != 7
  ) |> 
  mutate(
    told_liver_cond = case_when(
      told_liver_cond == 9 ~ NA_integer_,
      is.na(told_liver_cond) ~ NA_integer_,
      TRUE ~ told_liver_cond
    ),
    told_liver_cond = as.factor(told_liver_cond)
  )

body <- read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BMX_I.xpt") |> 
  janitor::clean_names() |> 
  select(
    seqn,
    bmi = bmxbmi,
    waist_circumference = bmxwaist
  )


inspectdf::inspect_na(bio)

most_missing <- 
  bio |> 
  pivot_longer(
    -seqn
  ) |> 
  filter(
    is.na(value)
  ) |> 
  group_by(
    seqn
  ) |> 
  count(value, sort = TRUE) |>
  filter(
    n > 2
  ) |> 
  pull(seqn)

most_missing

bio <- bio |> 
  filter(
    !seqn %in% c(most_missing)
  )

glimpse(bio)
glimpse(demo)

latino <- demo |> 
  filter(
    race_latino %in% c(1, 2) &
    age >= 20
  ) |> 
  left_join(bio, by = "seqn")

latino <- latino |> 
  left_join(alc, by = "seqn")

latino <- latino |> 
  left_join(gen_health, by = "seqn")

latino <- 
  latino |> 
  left_join(diet, by = "seqn")

latino <- 
latino |> 
  left_join(insurance, by = "seqn")

latino <- 
latino |> 
  left_join(med_cond, by = "seqn")

latino <- 
latino |> 
  left_join(body, by = "seqn")

md.pattern(latino)

inspectdf::inspect_na(latino)

latino <- latino |> 
  filter(
    annual_house_income != 77 &
    marital != 77 &
    ed != 7 &
    length_us != 77 &
    citizen != 7 &
    birth_country != 77
  ) |>
  mutate(
    across(
      c(
        marital,
        annual_house_income,
        length_us,
        birth_country
    ),
    ~case_when(
        is.na(.x) ~ NA_integer_,
      .x == 99 ~ NA_integer_,
      TRUE ~ .x
    )
  ),
  across(
      c(
        ed,
        citizen
    ),
    ~case_when(
        is.na(.x) ~ NA_integer_,
      .x == 9 ~ NA_integer_,
      TRUE ~ .x
    )
  ),
  across(
      c(
        citizen,
        length_us,
        marital,
        ed,
        annual_house_income,
        birth_country,
        race_latino,
        sex,
        total_num_house
    ),
    ~as.factor(.x)
  )
)

glimpse(latino)

# saveRDS(latino, here::here("projects/nhanes/latino_20plus.rds"))

# start here with data
library(tidyverse)
library(haven)
library(mice)
library(miceadds)
library(mclust)
library(rstanarm)
library(posterior)
library(bayesplot)

cat_map <- purrr::map

latino <- read_rds(here::here("projects/nhanes/latino_20plus.rds"))

pred_matrix <- make.predictorMatrix(data = latino)
imp_method <- make.method(data = latino)

pred_matrix[, "seqn"] <- 0

imp_method[c( "albumin_g_dl", "alp_iu_l",
"ast_u_l", "alt_u_l", "ggt_u_l", "total_bilirubin_mg_dl",
"hdl_chol_mg_dl", "trigly_mg_dl", "ldl_chol_mg_dl", "total_chol_mg_dl",
"num_meals_not_home_prepare", "num_meals_fast_food", "bmi", "waist_circumference" )] <- "midastouch"

imp_method[c("citizen", "length_us", "ed",
"annual_house_income", "alc_drink12_yr", "ever_45_drink_everyday", "gen_health",
"covered_insurance", "told_liver_cond")] <- "cart"

# set.seed(12345)
# model_imp <- 
#   mice(
#     latino,
#     m = 60,
#     maxit = 30,
#     method = imp_method,
#     predictorMatrix = pred_matrix 
#   )

# saveRDS(model_imp, here::here("projects/nhanes/imputation_model.rds"))

model_imp <- read_rds(here::here("projects/nhanes/imputation_model.rds"))

set.seed(12345)
model_all <- complete(model_imp, "all")

library(mclust)

colnames(model_all[[1]])

indicators <- 
  cat_map(
  model_all,
  ~.x |> 
    select(
      albumin_g_dl,
      alp_iu_l,
      ast_u_l,
      alt_u_l,
      ggt_u_l,
      total_bilirubin_mg_dl,
      hdl_chol_mg_dl,
      trigly_mg_dl,
      ldl_chol_mg_dl,            
      total_chol_mg_dl
    )
)

lpa_bic <- cat_map(
  indicators,
  ~mclustBIC(.x)
)

# saveRDS(lpa_bic, here::here("projects/nhanes/lpa_model_selection.rds"))

react_table <- function(data){
  reactable::reactable(
    {{data}},
    filterable = TRUE,
    sortable = TRUE,
    searchable = TRUE
  )
}

map_dfr(
  1:length(lpa_bic),
  ~lpa_bic[[1]][1:9, 1:14] |> 
    as_tibble()
) |> 
  select(
    VVE
  ) |> 
  mutate(
    id = rep(seq(1, 9, 1), 60),
    imp = rep(1:60, each = 9),
    VVE = round(VVE, 2)
  ) |> 
  drop_na(VVE) |> 
  mutate(
    diff = VVE - lag(VVE, default = first(VVE)),
    diff = round(diff, 2)
  ) |> 
  react_table()
# 5 profiles is where the difference is not drastic from 5 to 6, so going with 5 profiles

lpa <- cat_map(
  indicators,
  ~Mclust(
    .x,
    G = 5
  )
)

saveRDS(lpa, here::here("projects/nhanes/lpa_model.rds"))

lpa[[1]]$parameters$mean |>
  as_tibble() |> 
  mutate(
    indicator = colnames(indicators[[1]])
  ) |> 
  relocate(
    indicator, 
    .before = V1
  )

# find the average means across all imputations for each indicator
all_profile_mean <- map_dfr(
  1:length(lpa_bic),
  ~lpa[[.x]]$parameters$mean |>
  as_tibble() |> 
  mutate(
    indicator = colnames(indicators[[.x]])
  ) |> 
  relocate(
    indicator, 
    .before = V1
  )
) |> 
  mutate(
    imp = rep(1:60, each = 10)
  )

# saveRDS(all_profile_mean, here::here("projects/nhanes/all_imputed_profile_means.rds"))

psych::describeBy(all_profile_mean[, 2:6], all_profile_mean$indicator)

all_profile_mean |> 
  group_by(
    indicator
  ) |> 
  summarize(
    across(
      matches("^V"),
      ~mean(.x)
    )
  )

# find mode of each id for all imputations
all_profile <- 
  map_dfr(
  1:length(indicators),
  ~lpa[[.x]]$z |> 
    as_tibble()
) |> 
  mutate(
    id = rep(seq(1, 938, 1), 60),
    imp = rep(1:60, each = 938)
  )

# saveRDS(all_profile, here::here("projects/nhanes/all_imputed_profile_probabilities.rds"))

mode_profile <- all_profile |> 
  pivot_longer(
    -c(
      id,
      imp
    )
  ) |> 
  arrange(id) |> 
  group_by(
    id,
    imp
  ) |>
  filter(
    value == max(value)
  ) |> 
  ungroup(imp) |> 
  count(
    name
  ) |> 
  filter(
    n == max(n)
  ) |> 
  ungroup()
  
# saveRDS(mode_profile, here::here("projects/nhanes/most_common_profile.rds"))

