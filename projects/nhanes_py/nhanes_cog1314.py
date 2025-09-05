import pandas as pd
import numpy as np
import plotnine as pn
from janitor import clean_names
from matplotlib import rcParams
import seaborn as sns
import matplotlib.pyplot as plt

# Set some options
pd.set_option('display.max_columns', None)
pd.set_option('mode.copy_on_write', True)
rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

cog_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/CFQ_H.xpt'
sleep_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/SLQ_H.xpt'
smoke_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/SMQ_H.xpt'
smokes_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/SMQRTU_H.xpt'
med_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/RXQ_RX_H.xpt'
phy_func_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/PFQ_H.xpt'
pa_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/PAQ_H.xpt'
depress_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DPQ_H.xpt'
med_cond_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/MCQ_H.xpt'
insure_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/HIQ_H.xpt'
care_access_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/HUQ_H.xpt'
drug_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DUQ_H.xpt'
diabetes_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DIQ_H.xpt'
diet_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DBQ_H.xpt'
alc_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/ALQ_H.xpt'
heart_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/CDQ_H.xpt'
demo_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DEMO_H.xpt'
hdl_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/HDL_H.xpt'
ldl_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/TRIGLY_H.xpt'
bp_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/BPQ_H.xpt'
act_bp_link = 'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/BPX_H.xpt'


# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/CFQ_H.htm
cog = pd.read_sas(cog_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/SLQ_H.htm
sleep = pd.read_sas(sleep_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/SMQ_H.htm
smoke = pd.read_sas(smoke_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/SMQRTU_H.htm
smokes = pd.read_sas(smokes_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/RXQ_RX_H.htm#RXDDRUG
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/1988/DataFiles/RXQ_DRUG.htm
med = pd.read_sas(med_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/PFQ_H.htm
phy_func = pd.read_sas(phy_func_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/PAQ_H.htm
pa = pd.read_sas(pa_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DPQ_H.htm
depress = pd.read_sas(depress_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/MCQ_H.htm
med_cond = pd.read_sas(med_cond_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/HIQ_H.htm
insure = pd.read_sas(insure_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/HUQ_H.htm
care_access = pd.read_sas(care_access_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DUQ_H.htm
drug = pd.read_sas(drug_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DIQ_H.htm
diabetes = pd.read_sas(diabetes_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DBQ_H.htm
diet = pd.read_sas(diet_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/ALQ_H.htm
alc = pd.read_sas(alc_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/CDQ_H.htm
heart = pd.read_sas(heart_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DEMO_H.htm
demo = pd.read_sas(demo_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/HDL_H.htm
hdl = pd.read_sas(hdl_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/TRIGLY_H.htm
ldl = pd.read_sas(ldl_link).clean_names(case_type = 'snake')
#https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/BPX_H.htm#PEASCST1
act_bp = pd.read_sas(act_pb_link).clean_names(case_type = 'snake')
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/BPQ_H.htm#BPQ020
bp = pd.read_sas(pb_link).clean_names(case_type = 'snake')

# -------------------------------------data wrangling--------------------------------------------

# Cognitive Functioning
cog.head()
cog.info()
cog.columns.tolist()

cog['seqn'] = cog['seqn'].astype('object')
cog = (
  cog
  .rename(columns = 
          {'cfastat': 'cog_func_status',
           'cfalang': 'cog_func_lang',
           'cfdccs': 'cerad_complete',
           'cfdcst1': 'cerad_score_trial1',
           'cfdcst2': 'cerad_score_trial2',
           'cfdcst3': 'cerad_score_trial3',
           'cfdcsr': 'cerad_score_delay',
           'cfdcit1': 'cerad_count_trial1',
           'cfdcit2': 'cerad_count_trial2',
           'cfdcit3': 'cerad_count_trial3',
           'cfdcir': 'cerad_count_recall',
           'cfdapp': 'animal_fluency_practice',
           'cfdarnc': 'animal_fluency_notdone',
           'cfdast': 'animal_fluency_score',
           'cfddpp': 'digit_symbol_practice',
           'cfddrnc': 'digit_symbol_notdone',
           'cfdds': 'digit_symbol_score'
           })
)

cog = cog.drop(columns = ['cfdcrnc'])


# Sleep Disorders
sleep.head()
sleep.columns
sleep['seqn'] = sleep['seqn'].astype('object')

sleep = (
  sleep.
  rename(columns = 
         {'sld010_h': 'hours_sleep',
          'slq050': 'dr_trouble_sleep',
          'slq060': 'dr_told_sleep_disorder'    
         })
)

sleep['hours_sleep'].value_counts()
sleep['hours_sleep'] = sleep['hours_sleep'].replace(99, np.nan)

sleep['dr_trouble_sleep'].value_counts()
sleep['dr_trouble_sleep'] = sleep['dr_trouble_sleep'].replace(9, np.nan)
sleep['dr_trouble_sleep'] = np.where(sleep['dr_trouble_sleep'] == 2, 0, 1)

sleep['dr_told_sleep_disorder'].value_counts()
sleep['dr_told_sleep_disorder'] = sleep['dr_told_sleep_disorder'].replace(9, np.nan)
sleep['dr_told_sleep_disorder'] = np.where(sleep['dr_told_sleep_disorder'] == 2, 0, 1)


# Smoking History
smoke['seqn'] = smoke['seqn'].astype('object')
smoke.head()

smoke = (
  smoke
  .rename(columns = 
          {'smq020': 'lifetime100cig'})
)

smoke = smoke[['seqn', 'lifetime100cig']]


# Recent Smoking
smokes['seqn'] = smokes['seqn'].astype('object')
smokes.head()

smokes = (
  smokes.rename(columns = 
                {'smq681': 'smoke_tob_5day',
                 'smq851': 'smokeless_tob_5day'
                 })
)

smokes = smokes[['seqn', 'smoke_tob_5day', 'smokeless_tob_5day']]


#Medications
med['seqn'] = med['seqn'].astype('object')
med.head()

med = (
  med.rename(columns = 
             {'rxduse': 'took_med_pastmonth',
               'rxddrug': 'generic_drug_name',
               'rxdcount': 'num_medicines'})
)

med = med[['seqn', 'took_med_pastmonth', 'generic_drug_name', 'num_medicines']]
med['generic_drug_name'] = med['generic_drug_name'].str.decode('utf-8')

med['generic_drug_name'] = med['generic_drug_name'].str.title()

from bs4 import BeautifulSoup as soup
import wikipedia as wp

benzo = wp.page('List_of_benzodiazepines').html().encode('UTF-8')
pd.read_html(benzo)

drug_names = ['Diphenhydramine', 'Adinazolam', 'Alprazolam',
              'Bentazepam', 'Bretazenil', 'Bromazepam',
              'Bromazolam', 'Brotizolam', 'Camazepam',
              'Chlordiazepoxide', 'Cinazepam', 'Cinolazepam',
              'Clobazam', 'Clonazepam', 'Clonazolam',
              'Clorazepate', 'Clotiazepam', 'Cloxazolam']
med.loc[med['generic_drug_name'].isin([drug_names])]


# Alcohol Use
alc['seqn'] = alc['seqn'].astype('object')
alc.head()

alc = alc[['seqn', 'alq101', 'alq130', 'alq151']]
alc = (
  alc
  .rename(columns = 
          {'alq101': 'alc_drink',
           'alq130': 'avg_alc_drink_day',
           'alq151':'ever_45_drink_everyday'
           })
)

alc['alc_drink'].value_counts()
alc['alc_drink'] = alc['alc_drink'].replace(9, np.nan)
alc['alc_drink'] = np.where(alc['alc_drink'] == 2, 0, 1)

alc['avg_alc_drink_day'] = alc['avg_alc_drink_day'].replace(999, np.nan)

alc['ever_45_drink_everyday'].value_counts()
alc['ever_45_drink_everyday'] = alc['ever_45_drink_everyday'].replace(9, np.nan)
alc['ever_45_drink_everyday'] = np.where(alc['ever_45_drink_everyday'] == 2, 0, 1)


# -------------------------------------joining & variable dtypes--------------------------------------------
# cognitive dysfunction
# family history
# age
# genetics
# stroke/transient ischemic attack (TIA)
# high blood pressure
# diabetes
# obesity
# physical inactivity
# smoking
# lack of social engagement
# vascular disease
# sleep disturbances
# depression
# anxiety
# traumatic brain injury (TBI)
# medications

cog.head()
sleep.head()
alc.head()

data = cog.merge(sleep, how = 'left', on = 'seqn')
data = data.merge(alc, how = 'left', on = 'seqn')

data.info()
data.head()

#correlation between predictors & outcome(s)
corr_df = pd.DataFrame(data.corr())
print(corr_df[np.abs(corr_df) > .5])

data['dr_trouble_sleep'] = pd.Categorical(data['dr_trouble_sleep'], ordered = True, categories = [0, 1])
data['ever_45_drink_everyday'] = pd.Categorical(data['ever_45_drink_everyday'], ordered = True, categories = [0, 1])
data['alc_drink'] = pd.Categorical(data['alc_drink'], ordered = True, categories = [0, 1])
data['dr_told_sleep_disorder'] = pd.Categorical(data['dr_told_sleep_disorder'], ordered = True, categories = [0, 1])

# -------------------------------------train/test split & EDA--------------------------------------------
from sklearn.model_selection import train_test_split

num_features = ['hours_sleep', 'avg_alc_drink_day']
cat_features = data.select_dtypes(include = 'category').columns

x = data[['hours_sleep', 'avg_alc_drink_day', 'dr_trouble_sleep', 'dr_told_sleep_disorder', 'alc_drink', 'ever_45_drink_everyday']]
y = data['cerad_score_delay']

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = .2, random_state = 7282025)

train = x_train.join(y_train)

pn.ggplot.show(
  pn.ggplot(train, pn.aes('avg_alc_drink_day', 'cerad_score_delay'))
  + pn.geom_point(position = pn.position_jitter(),
                  alpha = .3)
  + pn.geom_smooth(se = False)
)

pn.ggplot.show(
  pn.ggplot(train, pn.aes('hours_sleep', 'cerad_score_delay'))
  + pn.geom_point(position = pn.position_jitter(),
                  alpha = .3)
  + pn.geom_smooth(se = False)
)

for i in cat_features:
  pn.ggplot.show(
    pn.ggplot(train, pn.aes(i, 'cerad_score_delay'))
    + pn.geom_boxplot()
  )
  
# -------------------------------------pipeline--------------------------------------------
from sklearn.pipeline import make_pipeline, Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.ensemble import RandomForestRegressor as rand_forest
from sklearn.model_selection import train_test_split, GridSearchCV, cross_validate, cross_val_predict, permutation_test_score
from sklearn.metrics import mean_absolute_error, mean_squared_error, root_mean_squared_error, r2_score
from sklearn.inspection import permutation_importance

preprocess = ColumnTransformer([('num_trans', StandardScaler, num_features),
                                ('dummy_trans', OneHotEncoder, cat_features)])

# Imputing Missing Data
from sklearn.immpute import SimpleImputer
# from sklearn.experimental import enable_iterative_imputer
# from sklearn.impute import IterativeImputer
# from sklearn.linear_model import BayesianRidge

train.loc[(train.isna().any(axis = 1))]

for i in train.columns:
  (
    train[i]
    .value_counts(dropna = False, normalize = True)
  )

# x_train_comp = x_train.dropna()
# y_train_comp = y_train.dropna()

# def get_score(x, y, imputer = None):
#   if imputer is None:
#     estimator = make_pipeline(preprocess, BayesianRidge())
#   else: 
#     estimator = make_pipeline(preprocess, imputer, BayesianRidge())
#   return cross_val_score(
#     estimator, x, y, scoring = "neg_mean_squared_error", cv = 5
#   )

# get_score(x_train, y_train)

# score_train_data = pd.DataFrame(
#   get_score(x_train, y_train),
#   columns = ['train_data']
# )

imp_median = SimpleImputer(missing_values = np.nan, strategy = "median")
imp_median.fit(train)
train_impute = imp_median.transform(train)
train_impute = pd.DataFrame(train_impute, columns = train.columns)

y_train_imp = train_impute['cerad_score_delay']
x_train_imp = train_impute.drop(columns = 'cerad_score_delay')

pipe = (
  Pipeline([
    ('scale_num_features',
      StandardScaler(),
    ),
    ('rforest_regressor',
    rand_forest(
      n_estimators = 1000,
      min_samples_split = 2,
      min_samples_leaf = 1,
      max_features = 'sqrt',
      random_state = 7282025,
      verbose = 2,
      n_jobs = 10)
    )])
)

# -------------------------------------modeling--------------------------------------------
pipe.fit(x_train_imp, y_train_imp)
rf_pred = pipe.predict(x_train_imp)

print(round(mean_absolute_error(y_train_imp, rf_pred), 2)) #MAE
print(round(mean_squared_error(y_train_imp, rf_pred), 2)) #MSE
print(round(root_mean_squared_error(y_train_imp, rf_pred), 2)) #RMSE
print(round(r2_score(y_train_imp, rf_pred), 2)) # R Squared

# cross_val_score(pipe, x_train_imp, y_train_imp, scoring = 'neg_root_mean_squared_error', cv = 5)
# cross_val_scores = cross_validate(pipe,
#                x_train_imp,
#                y_train_imp,
#                scoring = ['r2', 'neg_root_mean_squared_error'],
#                return_train_score = True,
#                cv = 5)
# cross_val_scores['fit_time']
# cross_val_scores['score_time']
# cross_val_scores['train_r2']
# cross_val_scores['test_r2']
# cross_val_scores['train_neg_root_mean_squared_error']
# cross_val_scores['test_neg_root_mean_squared_error']

# rf_cross_pred = cross_val_predict(pipe, x_train_imp, y_train_imp, cv = 5)
# rf_cross_pred
# print(round(mean_absolute_error(y_train_imp, rf_cross_pred), 2)) #MAE
# print(round(mean_squared_error(y_train_imp, rf_cross_pred), 2)) #MSE
# print(round(root_mean_squared_error(y_train_imp, rf_cross_pred), 2)) #RMSE
# print(round(r2_score(y_train_imp, rf_cross_pred), 2)) # R Squared

# grid search
rf_grid = [
  {'rforest_regressor__n_estimators': [100, 500, 1000, 2000],
  'rforest_regressor__min_samples_split': [2],
  'rforest_regressor__min_samples_leaf': [1, 2],
  'rforest_regressor__max_features': ['sqrt', 'log2']},
  {'rforest_regressor__n_estimators': [100, 500, 1000, 2000],
  'rforest_regressor__min_samples_split': [2],
  'rforest_regressor__min_samples_leaf': [1, 2]}
  ]

tune_pipe = (
  Pipeline([
    ('scale_num_features',
      StandardScaler(),
    ),
    ('rforest_regressor',
    rand_forest(
      random_state = 7282025,
      verbose = 2,
      n_jobs = 10)
    )])
)

rf_search = (
  GridSearchCV(
    tune_pipe,
    param_grid = rf_grid,
    cv = 5,
    scoring = 'neg_root_mean_squared_error',
    verbose = 2
  )
)

rf_search.fit(x_train_imp, y_train_imp)

print(rf_search.best_score_)
print(rf_search.best_params_)

from sklearn.inspection import permutation_importance

important = (
  permutation_importance(
    rf_search.best_estimator_, 
    x_train_imp, 
    y_train_imp, 
    n_repeats = 20,
    n_jobs = 10,
    random_state = 7282025)
)

important_df = (
  pd
  .DataFrame({'var_names': x_train_imp.columns,
  'importance_mean': important.importances_mean})
  .sort_values('importance_mean', ascending = False)
)

print(important_df)

rmse_rf, perm_score_rf, pvalue_rf = permutation_test_score(
    pipe,
    x_train_imp,
    y_train_imp,
    scoring = "neg_root_mean_squared_error",
    cv = 5,
    n_permutations = 1000,
    random_state = 7282025,
    njobs = 10,
    verbose = 2
)

print(rmse_rf)
print(perm_score_rf)
print(pvalue_rf)

# -------------------------------------test set--------------------------------------------