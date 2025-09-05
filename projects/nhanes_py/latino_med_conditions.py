import pandas as pd
import numpy as np
import plotnine as pn
from janitor import clean_names
# from pgmpy.models import DiscreteBayesianNetwork
# from pgmpy.factors.discrete import TabularCPD
# from pgmpy.estimators import HillClimbSearch, ExpectationMaximization
from matplotlib import rcParams

# Set some options
pd.set_option('display.max_columns', None)
pd.set_option('mode.copy_on_write', True)
rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

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

# 2017-2018 data
# https://wwwn.cdc.gov/nchs/nhanes/continuousnhanes/default.aspx?BeginYear=2017

# hdl cholesterol
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/HDL_J.htm
hdl = (
  pd
  .read_sas('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/HDL_J.xpt')
  .clean_names(case_type = 'snake')
)

hdl = hdl[['seqn', 'lbdhdd']]
hdl = hdl.rename(columns = {'lbdhdd': 'hdl_chol_mg_dl'})

hdl['hdl_bi_ab'] = np.where(hdl['hdl_chol_mg_dl'] < 60, 1, 0)


# ldl cholesterol & triglycerides
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/TRIGLY_J.htm
ldl = (
  pd
  .read_sas('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/TRIGLY_J.xpt')
  .clean_names(case_type = 'snake')
)

ldl = ldl[['seqn', 'lbxtr', 'lbdldl']]
ldl = ldl.rename(columns = {'lbxtr': 'trigly_mg_dl', 'lbdldl': 'ldl_chol_mg_dl'})

ldl['trigly_bi_ab'] = np.where(ldl['trigly_mg_dl'] > 150, 1, 0)
ldl['ldl_bi_ab'] = np.where(ldl['ldl_chol_mg_dl'] > 100, 1, 0)


# blood work
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/BIOPRO_J.htm
bio = (
  pd
  .read_sas('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/BIOPRO_J.xpt')
  .clean_names(case_type = 'snake')
)

bio = bio[['seqn', 'lbxsal', 'lbxsapsi', 'lbxsassi', 'lbxsatsi', 'lbxsgtsi', 'lbxstb']]
bio = bio.rename(columns = {'lbxsal': 'albumin_g_dl',
'lbxsapsi': 'alp_iu_l',
'lbxsassi': 'ast_u_l',
'lbxsatsi': 'alt_u_l',
'lbxsgtsi': 'ggt_u_l',
'lbxstb': 'total_bilirubin_mg_dl'})

albumin_cond = [(bio['albumin_g_dl'] > 5.5),
(bio['albumin_g_dl'] < 3.5),
(bio['albumin_g_dl'].between(3.5, 5.5, inclusive = 'both'))]
bio['albumin_bi_ab'] = np.select(albumin_cond, [1, 1, 0])

alp_cond = [(bio['alp_iu_l'] > 147),
(bio['alp_iu_l'] < 44),
(bio['alp_iu_l'].between(44, 147, inclusive = 'both'))]
bio['alp_bi_ab'] = np.select(alp_cond, [1, 1, 0])

alt_cond = [(bio['alt_u_l'] > 56),
(bio['alt_u_l'] < 7),
(bio['alt_u_l'].between(7, 56, inclusive = 'both'))]
bio['alt_bi_ab'] = np.select(alt_cond, [1, 1, 0])

ast_cond = [(bio['ast_u_l'] > 33),
(bio['ast_u_l'] < 8),
(bio['ast_u_l'].between(8, 33, inclusive = 'both'))]
bio['ast_bi_ab'] = np.select(ast_cond, [1, 1, 0])

bio['ggt_bi_ab'] = np.where(bio['ggt_u_l'] > 50, 1, 0)

bilirubin_cond = [(bio['total_bilirubin_mg_dl'] > 1.3),
(bio['total_bilirubin_mg_dl'] < .2),
(bio['total_bilirubin_mg_dl'].between(.2, 1.3, inclusive = 'both'))]
bio['bilirubin_bi_ab'] = np.select(bilirubin_cond, [1, 1, 0])


# demographics
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/DEMO_J.htm
demo = (
  pd
  .read_sas('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/DEMO_J.xpt')
  .clean_names(case_type = 'snake')
)

demo = demo[['seqn', 'riagendr', 'ridageyr', 'ridreth1', 'dmdborn4', 'dmdcitzn', 'dmdyrsus', 'dmdeduc2', 'dmdmartl', 'dmdhhsiz', 'indhhin2']]
demo = demo.rename(columns = {'riagendr': 'sex', 
'ridageyr': 'age', 
'ridreth1': 'race_latino', 
'dmdborn4': 'birth_country', 
'dmdcitzn': 'citizen', 
'dmdyrsus': 'length_us', 
'dmdeduc2': 'ed', 
'dmdmartl': 'marital', 
'dmdhhsiz': 'total_num_house', 
'indhhin2': 'annual_house_income'})

demo['race_latino'].value_counts()
latino = demo.loc[demo['race_latino'].isin([1, 2])]

demo['adult'] = np.where(demo['age'] >= 18, 1, 0)
latino['adult'] = np.where(latino['age'] >= 18, 1, 0)

# alcohol use
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/ALQ_J.htm
alc = (
  pd
  .read_sas('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/ALQ_J.xpt')
  .clean_names(case_type = 'snake')
)
alc = alc[['seqn', 'alq111', 'alq151']]
alc = alc.rename(columns = {'alq111': 'alc_drink', 'alq151':'ever_45_drink_everyday'})

alc['alc_drink'].value_counts()
alc['alc_drink'] = np.where(alc['alc_drink'] == 2, 0, 1)

alc['ever_45_drink_everyday'].value_counts()
alc = alc.loc[alc['ever_45_drink_everyday'] != 7]
alc['ever_45_drink_everyday'] = alc['ever_45_drink_everyday'].replace(9, np.nan)
alc['ever_45_drink_everyday'] = np.where(alc['ever_45_drink_everyday'] == 2, 0, 1)

alc.head()

# Medical conditions
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/MCQ_J.htm
med_cond = (
  pd
  .read_sas('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/MCQ_J.xpt')
  .clean_names(case_type = 'snake')
)
med_cond = med_cond[['seqn', 
# 'mcq203', 'mcq220', 'mcq160_d', 'mcq160_e', 'mcq160_f',
'mcq160_l', 'mcq080', 'mcq366_a', 'mcq366_b', 'mcq366_c',
'mcq366_d', 'mcq371_a', 'mcq371_b', 'mcq371_c', 'mcq371_d',
'mcd180_l', 'mcq500', 'mcq510_a', 'mcq510_b',
'mcq510_c', 'mcq510_d', 'mcq510_e', 'mcq510_f']]
med_cond = med_cond.rename(columns = {'mcd180_l': 'age_told_liver_cond',
'mcq500': 'youth_told_liver_cond',
'mcq510_a': 'fatty_liver',
'mcq510_b': 'liver_fibrosis',
'mcq510_c': 'liver_cirrhosis',
'mcq510_d': 'viral_hepatitis',
'mcq510_e': 'auto_hepatitis',
'mcq510_f': 'other_liver_disease',
  # 'mcq160_d': 'told_angina',
# 'mcq160_e': 'told_heart_attack',
# 'mcq160_f': 'told_stroke',
'mcq160_l': 'told_liver_cond',
# 'mcq203': 'told_jaundice',
# 'mcq220': 'told_cancer',
'mcq080': 'dr_told_overweight',
'mcq366_a': 'dr_told_lose_wt',
'mcq366_b': 'dr_told_exercise',
'mcq366_c': 'dr_told_reduce_salt',
'mcq366_d': 'dr_told_reduce_fat',
'mcq371_a': 'you_control_wt',
'mcq371_b': 'you_increase_exercise',
'mcq371_c': 'you_reduce_salt',
'mcq371_d': 'you_reduce_fat'})

# med_cond = med_cond.loc[(med_cond['told_heart_attack'] != 7)]
# med_cond = med_cond.loc[(med_cond['told_stroke'] != 7)]
# med_cond = med_cond.loc[(med_cond['told_jaundice'] != 7)]
# med_cond = med_cond.loc[(med_cond['told_cancer'] != 7)]
med_cond = med_cond.loc[(med_cond['age_told_liver_cond'] != 77777)]
med_cond = med_cond.loc[(med_cond['youth_told_liver_cond'] != 7)]
med_cond = med_cond.loc[(med_cond['fatty_liver'] != 77)]
med_cond = med_cond.loc[(med_cond['liver_fibrosis'] != 77)]
med_cond = med_cond.loc[(med_cond['liver_cirrhosis'] != 77)]
med_cond = med_cond.loc[(med_cond['viral_hepatitis'] != 77)]
med_cond = med_cond.loc[(med_cond['auto_hepatitis'] != 77)]
med_cond = med_cond.loc[(med_cond['other_liver_disease'] != 77)]
med_cond = med_cond.loc[(med_cond['told_liver_cond'] != 7)]
med_cond = med_cond.loc[(med_cond['dr_told_overweight'] != 7)]
med_cond = med_cond.loc[(med_cond['dr_told_lose_wt'] != 7)]
med_cond = med_cond.loc[(med_cond['dr_told_exercise'] != 7)]
med_cond = med_cond.loc[(med_cond['dr_told_reduce_salt'] != 7)]
med_cond = med_cond.loc[(med_cond['dr_told_reduce_fat'] != 7)]
med_cond = med_cond.loc[(med_cond['you_control_wt'] != 7)]
med_cond = med_cond.loc[(med_cond['you_increase_exercise'] != 7)]
med_cond = med_cond.loc[(med_cond['you_reduce_salt'] != 7)]
med_cond = med_cond.loc[(med_cond['you_reduce_fat'] != 7)]

med_cond = med_cond.replace(9, np.nan)
med_cond = med_cond.replace(99, np.nan)
med_cond = med_cond.replace(99999, np.nan)

# med_cond['told_heart_attack'] = np.where(med_cond['told_heart_attack'] == 2, 0, 1)
# med_cond['told_stroke'] = np.where(med_cond['told_stroke'] == 2, 0, 1)
# med_cond['told_jaundice'] = np.where(med_cond['told_jaundice'] == 2, 0, 1)
# med_cond['told_cancer'] = np.where(med_cond['told_cancer'] == 2, 0, 1)

med_cond['age_told_liver_cond_adult'] = np.where(med_cond['age_told_liver_cond'] < 18, 0, 1)
med_cond['youth_told_liver_cond'] = np.where(med_cond['youth_told_liver_cond'] == 2, 0, 1)
med_cond['fatty_liver'] = np.where(med_cond['fatty_liver'] == 1, 1, 0)
med_cond['liver_fibrosis'] = np.where(med_cond['liver_fibrosis'] == 2, 1, 0)
med_cond['liver_cirrhosis'] = np.where(med_cond['liver_cirrhosis'] == 3, 1, 0)
med_cond['viral_hepatitis'] = np.where(med_cond['viral_hepatitis'] == 4, 1, 0)
med_cond['auto_hepatitis'] = np.where(med_cond['auto_hepatitis'] == 5, 1, 0)
med_cond['other_liver_disease'] = np.where(med_cond['other_liver_disease'] == 6, 1, 0)

med_cond['fatty_liver'] = med_cond['fatty_liver'].fillna(0)
med_cond['liver_fibrosis'] = med_cond['liver_fibrosis'].fillna(0)
med_cond['liver_cirrhosis'] = med_cond['liver_cirrhosis'].fillna(0)
med_cond['viral_hepatitis'] = med_cond['viral_hepatitis'].fillna(0)
med_cond['auto_hepatitis'] = med_cond['auto_hepatitis'].fillna(0)
med_cond['other_liver_disease'] = med_cond['other_liver_disease'].fillna(0)

med_cond['told_liver_cond'] = np.where(med_cond['told_liver_cond'] == 2, 0, 1)
med_cond['dr_told_overweight'] = np.where(med_cond['dr_told_overweight'] == 2, 0, 1)
med_cond['dr_told_lose_wt'] = np.where(med_cond['dr_told_lose_wt'] == 2, 0, 1)
med_cond['dr_told_exercise'] = np.where(med_cond['dr_told_exercise'] == 2, 0, 1)
med_cond['dr_told_reduce_salt'] = np.where(med_cond['dr_told_reduce_salt'] == 2, 0, 1)
med_cond['dr_told_reduce_fat'] = np.where(med_cond['dr_told_reduce_fat'] == 2, 0, 1)
med_cond['you_control_wt'] = np.where(med_cond['you_control_wt'] == 2, 0, 1)
med_cond['you_increase_exercise'] = np.where(med_cond['you_increase_exercise'] == 2, 0, 1)
med_cond['you_reduce_salt'] = np.where(med_cond['you_reduce_salt'] == 2, 0, 1)
med_cond['you_reduce_fat'] = np.where(med_cond['you_reduce_fat'] == 2, 0, 1)

# body measures
# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/BMX_J.htm
body = (
  pd
  .read_sas('https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/BMX_J.xpt')
  .clean_names(case_type = 'snake')
)
body = body[['seqn', 'bmxbmi', 'bmxwaist']]
body = body.rename(columns = {'bmxbmi': 'bmi', 'bmxwaist': 'waist_circumference'})

body['bmi_bi_obese'] = np.where(body['bmi'] > 30, 1, 0)

#double check column names
hdl.info()
ldl.info()
bio.info()
demo.info()
# latino.info()
alc.info()
med_cond.info()
body.info()

joined = latino.merge(ldl, how = 'left', on = 'seqn')
# joined = demo.merge(ldl, how = 'left', on = 'seqn')
joined = joined.merge(hdl, how = 'left', on = 'seqn')
joined = joined.merge(bio, how = 'left', on = 'seqn')
joined = joined.merge(alc, how = 'left', on = 'seqn')
joined = joined.merge(med_cond, how = 'left', on = 'seqn')
joined = joined.merge(body, how = 'left', on = 'seqn')

joined.info()
joined.shape

pn.ggplot.show(
  pn.ggplot(joined, pn.aes('age'))
  + pn.geom_histogram(color = 'black', fill = 'dodgerblue', bins = 40)
  + pn.geom_vline(xintercept = 16)
  + pn.theme_light()
)

# filtering for age because target group is 20+ for medical conditions
age_sub = joined.loc[joined['age'] >= 20]

pn.ggplot.show(
  pn.ggplot(age_sub, pn.aes('age'))
  + pn.geom_histogram(color = 'black', fill = 'dodgerblue', bins = 40)
  + pn.theme_light()
)

age_sub.shape

#pre-processing
from pgmpy.estimators import ExpertKnowledge
import statsmodels.api as sm
from sklearn.pipeline import make_pipeline, Pipeline
from sklearn.preprocessing import StandardScaler, OneHotEncoder, FunctionTransformer
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import StratifiedKFold, GridSearchCV, train_test_split
from sklearn.metrics import accuracy_score, roc_auc_score, mean_absolute_error, roc_curve, precision_score, recall_score, log_loss, confusion_matrix, balanced_accuracy_score, ConfusionMatrixDisplay
from stepmix.stepmix import StepMix

# BN Practice
sub = age_sub[[
  'seqn',
  'race_latino',
  'sex',
  # 'adult',
  'trigly_bi_ab',
  'ldl_bi_ab',
  'hdl_bi_ab',
  'albumin_bi_ab',
  'alp_bi_ab',
  'alt_bi_ab',
  'ast_bi_ab',
  'ggt_bi_ab',
  'bilirubin_bi_ab',
  'alc_drink',
  'ever_45_drink_everyday',
  'told_liver_cond',
  # 'age_told_liver_cond_adult',
  # 'youth_told_liver_cond',
  'fatty_liver',
  'liver_fibrosis',
  'liver_cirrhosis',
  'viral_hepatitis',
  'auto_hepatitis',
  'other_liver_disease',
  'dr_told_overweight',
  'dr_told_lose_wt',
  'dr_told_exercise',
  'dr_told_reduce_salt',
  'dr_told_reduce_fat',
  'you_control_wt',
  'you_increase_exercise',
  'you_reduce_salt',
  'you_reduce_fat',
  'bmi_bi_obese'
  ]]

imp = sm.MICEData(sub)

# only includes 1 imputed dataset, not recommended
impute_data = imp.data

train = impute_data.sample(frac = .75, random_state = 12345)

train.shape

outer = impute_data.merge(train, how = 'outer', indicator = True)
test = outer[(outer._merge == 'left_only')].drop('_merge', axis = 1)

train.columns.tolist()
test.columns.tolist()

impute_cat_train = train[[
  'trigly_bi_ab',
  'ldl_bi_ab',
  'hdl_bi_ab',
  'albumin_bi_ab',
  'alp_bi_ab',
  'alt_bi_ab',
  'ast_bi_ab',
  'ggt_bi_ab',
  'bilirubin_bi_ab'
  ]]

impute_cat_test = test[[
  'trigly_bi_ab',
  'ldl_bi_ab',
  'hdl_bi_ab',
  'albumin_bi_ab',
  'alp_bi_ab',
  'alt_bi_ab',
  'ast_bi_ab',
  'ggt_bi_ab',
  'bilirubin_bi_ab'
  ]]

lca_train = StepMix(n_components = 2, measurement = 'categorical')
lca_test = StepMix(n_components = 2, measurement = 'categorical')

lca_train.fit(impute_cat_train)
lca_test.fit(impute_cat_test)

train['lat_class'] = lca_train.predict(impute_cat_train)
test['lat_class'] = lca_test.predict(impute_cat_test)

train.columns.tolist()

train = train.drop(columns = ['trigly_bi_ab',
  'ldl_bi_ab',
  'hdl_bi_ab',
  'albumin_bi_ab',
  'alp_bi_ab',
  'alt_bi_ab',
  'ast_bi_ab',
  'ggt_bi_ab',
  'bilirubin_bi_ab'])

test = test.drop(columns = ['trigly_bi_ab',
  'ldl_bi_ab',
  'hdl_bi_ab',
  'albumin_bi_ab',
  'alp_bi_ab',
  'alt_bi_ab',
  'ast_bi_ab',
  'ggt_bi_ab',
  'bilirubin_bi_ab'])

expert_set = [
  # ('trigly_bi', 'lat_class'),
  # ('ldl_bi', 'lat_class'),
  # ('hdl_bi', 'lat_class'),
  # ('albumin_bi', 'lat_class'),
  # ('alp_bi', 'lat_class'),
  # ('alt_bi', 'lat_class'),
  # ('ast_bi', 'lat_class'),
  # ('ggt_bi', 'lat_class'),
  # ('bilirubin_bi', 'lat_class'),

  ('race_latino', 'alc_drink'),
  ('race_latino', 'ever_45_drink_everyday'),
  ('race_latino', 'bmi_bi_obese'),
  ('race_latino', 'lat_class'),

  ('sex', 'alc_drink'),
  ('sex', 'ever_45_drink_everyday'),
  ('sex', 'bmi_bi_obese'),
  ('sex', 'lat_class'),

  ('lat_class', 'alc_drink'),
  ('lat_class', 'ever_45_drink_everyday'),
  ('lat_class', 'bmi_bi_obese'),
  ('lat_class', 'other_liver_disease'), 
  ('lat_class', 'auto_hepatitis'),
  ('lat_class', 'viral_hepatitis'), 
  ('lat_class', 'liver_cirrhosis'), 
  ('lat_class', 'liver_fibrosis'), 
  ('lat_class', 'fatty_liver'),

  ('bmi_bi_obese', 'other_liver_disease'), 
  ('bmi_bi_obese', 'auto_hepatitis'),
  ('bmi_bi_obese', 'viral_hepatitis'), 
  ('bmi_bi_obese', 'liver_cirrhosis'), 
  ('bmi_bi_obese', 'liver_fibrosis'), 
  ('bmi_bi_obese', 'fatty_liver'),

  ('alc_drink', 'ever_45_drink_everyday'),

  ('ever_45_drink_everyday', 'liver_cirrhosis'),
  ('alc_drink', 'liver_cirrhosis'),

  ('ever_45_drink_everyday', 'other_liver_disease'),
  ('alc_drink', 'other_liver_disease')
  ]

expert_stop = [
  ('told_liver_cond', 'fatty_liver'),
  ('told_liver_cond', 'liver_fibrosis'),
  ('told_liver_cond', 'liver_cirrhosis'),
  ('told_liver_cond', 'viral_hepatitis'),
  ('told_liver_cond', 'auto_hepatitis'),
  ('told_liver_cond', 'other_liver_disease'),

  ('fatty_liver', 'liver_fibrosis'),
  ('fatty_liver', 'liver_cirrhosis'),
  ('fatty_liver', 'viral_hepatitis'),
  ('fatty_liver', 'auto_hepatitis'),
  ('fatty_liver', 'other_liver_disease'),

  ('liver_fibrosis', 'liver_cirrhosis'),
  ('liver_fibrosis', 'viral_hepatitis'),
  ('liver_fibrosis', 'auto_hepatitis'),
  ('liver_fibrosis', 'other_liver_disease'),

  ('liver_cirrhosis', 'viral_hepatitis'),
  ('liver_cirrhosis', 'auto_hepatitis'),
  ('liver_cirrhosis', 'other_liver_disease'),

  ('viral_hepatitis', 'auto_hepatitis'),
  ('viral_hepatitis', 'other_liver_disease'),

  ('auto_hepatitis', 'other_liver_disease'),

  ('fatty_liver', 'told_liver_cond'),
  ('liver_fibrosis', 'told_liver_cond'),
  ('liver_cirrhosis', 'told_liver_cond'),
  ('viral_hepatitis', 'told_liver_cond'),
  ('auto_hepatitis', 'told_liver_cond'),
  ('other_liver_disease', 'told_liver_cond'),
]

expert = ExpertKnowledge(required_edges = expert_set, forbidden_edges = expert_stop)

hc = HillClimbSearch(train)
dag = hc.estimate(expert_knowledge = expert)

dag.edges()


# cat_features = []
# num_features = []

# preprocessor = ColumnTransformer([
#     ('encoder', OneHotEncoder(), cat_features),
#     ('scale', StandardScaler(), num_features)
# ])

# pipe = (
#   Pipeline([
#     (
#       'preprocess', 
#       preprocessor
#       ),
#     (
#       'log_model',
#     )
#     ])
# )



bn_model = DiscreteBayesianNetwork([
('race_latino', 'lat_class'), 
('race_latino', 'alc_drink'), 
('race_latino', 'bmi_bi_obese'), 
('race_latino', 'ever_45_drink_everyday'), 
('sex', 'ever_45_drink_everyday'), 
('sex', 'lat_class'),
('sex', 'alc_drink'),
('sex', 'bmi_bi_obese'), 
('alc_drink', 'ever_45_drink_everyday'), 
('alc_drink', 'other_liver_disease'),
('alc_drink', 'liver_cirrhosis'),
('ever_45_drink_everyday', 'other_liver_disease'),
('ever_45_drink_everyday', 'liver_cirrhosis'),
('dr_told_overweight', 'dr_told_lose_wt'), 
('dr_told_overweight', 'you_control_wt'),
('dr_told_lose_wt', 'dr_told_exercise'), 
('dr_told_lose_wt', 'dr_told_reduce_fat'), 
('dr_told_lose_wt', 'you_control_wt'), 
('dr_told_exercise', 'dr_told_reduce_fat'), 
('dr_told_exercise', 'dr_told_reduce_salt'), 
('dr_told_exercise', 'told_liver_cond'),
('dr_told_reduce_salt', 'you_reduce_salt'), 
('dr_told_reduce_fat', 'dr_told_reduce_salt'),
('dr_told_reduce_fat', 'you_reduce_fat'), 
('dr_told_reduce_fat', 'fatty_liver'),
('you_control_wt', 'you_increase_exercise'), 
('you_control_wt', 'you_reduce_fat'), 
('you_reduce_fat', 'you_reduce_salt'), 
('you_reduce_fat', 'you_increase_exercise'),
('bmi_bi_obese', 'other_liver_disease'),
('bmi_bi_obese', 'liver_cirrhosis'), 
('bmi_bi_obese', 'viral_hepatitis'), 
('bmi_bi_obese', 'fatty_liver'), 
('bmi_bi_obese', 'auto_hepatitis'),
('bmi_bi_obese', 'liver_fibrosis'),
('bmi_bi_obese', 'dr_told_overweight'),
('bmi_bi_obese', 'dr_told_lose_wt'),
('lat_class', 'alc_drink'), 
('lat_class', 'viral_hepatitis'), 
('lat_class', 'liver_cirrhosis'),
('lat_class', 'fatty_liver'), 
('lat_class', 'bmi_bi_obese'), 
('lat_class', 'auto_hepatitis'),
('lat_class', 'liver_fibrosis'), 
('lat_class', 'ever_45_drink_everyday'),
('lat_class', 'other_liver_disease')
])

model_viz = bn_model.to_graphviz()
model_viz.draw('/home/jon/Documents/github_repos/projects_portfolio_and_practice/projects/nhanes_py/bn_example.png', prog = 'dot')

# print(bn_model.get_cpds('mcq160_l'))

em_estimate = ExpectationMaximization(bn_model, train)
# em_estimate.state_counts('told_liver_cond')
em_param = em_estimate.get_parameters()
em_param

for i in np.arange(0, len(em_param)):
  print(em_param[i])






# from pgmpy.inference import VariableElimination

# model_fit = bn_model.fit(sub)
# print(model_fit)
# inference = VariableElimination(bn_model)
# print(inference.induced_graph(['race_latino',
# 'trigly_bi',
# 'ldl_bi',
# 'hdl_bi',
# 'albumin_bi',
# 'alp_bi',
# 'alt_bi',
# 'ast_bi',
# 'ggt_bi',
# 'bilirubin_bi',
# 'told_liver_cond']))



# med_cond_sub['mcq365_a'].value_counts(normalize = True)
# cpd_drlosewt = TabularCPD(variable = 'mcq365_a', variable_card = 2, values = [[.28], [.72]])

# med_cond_sub['mcq365_b'].value_counts(normalize = True)
# cpd_drexercise = TabularCPD(variable = 'mcq365_b', variable_card = 2, values = [[.41], [.59]])

# med_cond_sub['mcq365_c'].value_counts(normalize = True)
# cpd_drreducesalt = TabularCPD(variable = 'mcq365_c', variable_card = 2, values = [[.29], [.71]])

# med_cond_sub['mcq365_d'].value_counts(normalize = True)
# cpd_drreducefat = TabularCPD(variable = 'mcq365_d', variable_card = 2, values = [[.31], [.69]])

# group_by_cpd = med_cond_sub.groupby(['mcq365_a', 'mcq365_b', 'mcq365_c', 'mcq365_d'])['mcq160_l'].value_counts(normalize = True).reset_index()
# group_by_cpd.loc[(group_by_cpd['mcq160_l'] == 1)]
# group_by_cpd.loc[(group_by_cpd['mcq160_l'] == 2)]

# cpd_livercond = TabularCPD(
#   variable = 'mcq160_l',
#   variable_card = 2,
#   values = [[.09, .08, .07, .05, .02, .06, .03, .05, .07, .08, .07, .03, .06, .05, .08, .02],
#   [.91, .92, .93, .95, .98, .94, .97, .95, .93, .92, .93, .97, .94, .95, .92, .98]],
#   evidence = ['mcq365_a', 'mcq365_b', 'mcq365_c', 'mcq365_d'],
#   evidence_card = [2, 2, 2, 2])

# bn_model.add_cpds(cpd_drlosewt, cpd_drexercise, cpd_drreducesalt, cpd_drreducefat, cpd_livercond)
# bn_model.check_model()