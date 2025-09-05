import os
import seaborn as sns
import numpy as np
import pandas as pd
import matplotlib
from cmdstanpy import CmdStanModel
import arviz as az
import matplotlib.pyplot as plt
import plotnine as pn
import pickle
from pyhere import here

os.environ['QT_API'] = 'PyQt6'

pd.set_option('display.max_columns', None)
pd.options.mode.copy_on_write = True
matplotlib.rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off
matplotlib.use('TkAgg')

# data created
np.random.seed(81425)
probs = np.random.rand(250, 9)

# Generate outcomes for each item using those probabilities
np.random.seed(81425)
outcomes = np.random.binomial(n = 1, p = probs)

# Convert to DataFrame
df = pd.DataFrame(outcomes, columns=[f"item_{i+1}" for i in range(9)])

# q matrix
q = pd.DataFrame({
  'att1': [1, 1, 1, 1, 1, 1, 1, 1, 1],
  'att2': [0, 0, 0, 1, 1, 1, 0, 0 ,0],
  'att3': [0, 0, 0, 0, 0, 0, 1, 1, 1]})

# attribute mastery matrix
alpha = pd.DataFrame([(x, y, z) for x in range(2) for y in range(2) for z in range(2)])
alpha = alpha.rename(columns = {0: 'att1', 1: 'att2', 2: 'att3'})


# stan dictionary data
stan_dict = {
  'J': df.shape[0],
  'I': df.shape[1],
  'K': q.shape[1],
  'C': alpha.shape[0],
  'Y': np.array(df),
  'Q': np.array(q), 
  'alpha': np.array(alpha)
}

with open(here('projects/dcm/lcdm_py/lcdm250_3att_linear.pkl'), "rb") as f:
    data_list = pickle.load(f)
  
stan_model = data_list[0]
fit = data_list[1]

# model diagnostics

fit.summary()
fit.diagnose()

(
  fit.summary()['R_hat']
  .sort_values(ascending = False)
)

# draws as array
fit.draws()[[0]]

# draws dataframe
post_df = fit.draws_pd()

post_df.columns.tolist()


# inference dataset
# inference = az.from_cmdstanpy(
#     posterior = fit,
#     posterior_predictive = {'Y_rep': fit.stan_variable('Y_rep')},
#     observed_data = {'Y': stan_dict['Y']},
#     log_likelihood = fit.stan_variable('log_item'),
#     coords = {"student": np.arange(stan_dict['J'])}
# )


# fit.draws_xr()
# fit.stan_variables().keys()

ex = az.load_arviz_data("regression1d")

idata = az.from_cmdstanpy(
    posterior = fit,
    posterior_predictive = {'Y_rep': ['Y_rep']},
    observed_data = {'Y': stan_dict['Y']}
)

name_mapping = {'Y_rep': 'Y'}
idata = idata.rename(name_dict = name_mapping, groups = ["posterior_predictive"])


print(idata.posterior)
print(idata.posterior_predictive)
print(idata.sample_stats)
print(idata.observed_data)

ex.posterior_predictive
ex.observed_data

# model visuals
plt.clf()

az.plot_bpv(idata, kind = 't_stat', t_stat = .5)
plt.show()
plt.clf()

az.plot_ppc(idata, data_pairs = {'Y': 'Y'}, alpha = .05, textsize = 14)
plt.show()
plt.clf()

az.plot_ppc(idata, data_pairs = {'Y': 'Y'}, alpha = .05, kind = 'cumulative', textsize = 14)
plt.show()
plt.clf()



# probability of student in each class
# az.plot_forest(inference, var_names = 'prob_resp_class', colors = 'seagreen')
# plt.show()
# plt.clf()

# # probability of student mastery of each attribute
# az.plot_forest(fit, var_names = 'prob_resp_attr', colors = 'seagreen')
# plt.show()
# plt.clf()

stu_att_mastery = pd.DataFrame({
  'parameters': post_df.filter(regex = '^prob_resp_attr').columns,
  'mean': post_df.filter(regex = '^prob_resp_attr').mean().reset_index(drop = True)
})

stu_att_mastery[['drop', 'other']] = stu_att_mastery['parameters'].str.split(pat = '[', expand = True)
stu_att_mastery[['id', 'att']] = stu_att_mastery['other'].str.split(pat = ',', expand = True)
stu_att_mastery['att'] = stu_att_mastery['att'].str.replace(']', '')

stu_att_mastery = stu_att_mastery.drop(columns = ['parameters', 'drop', 'other'])

stu_att_mastery['id'] = stu_att_mastery['id'].astype(int)


stu_att_mastery = (
  stu_att_mastery
  .pivot(index = 'id', columns = 'att', values = 'mean')
  .reset_index()
  .sort_values(by = 'id')
)

stu_att_mastery['att1_bi'] = pd.Series(np.where(stu_att_mastery['1'] >= .7, 1, 0))
stu_att_mastery['att2_bi'] = pd.Series(np.where(stu_att_mastery['2'] >= .7, 1, 0))
stu_att_mastery['att3_bi'] = pd.Series(np.where(stu_att_mastery['3'] >= .7, 1, 0))

stu_att_mastery['profile'] = (
  stu_att_mastery['att1_bi'].astype(str)
  + stu_att_mastery['att2_bi'].astype(str)
  + stu_att_mastery['att3_bi'].astype(str)  
)

stu_att_mastery.head()

stu_att_mastery = stu_att_mastery.rename(columns = {'1': 'att1', '2': 'att2', '3': 'att3'})

for i in ['att1_bi', 'att2_bi', 'att3_bi']:
  print(stu_att_mastery[i].value_counts(normalize = True))

# overall
lower_stu_att_mastery = []
upper_stu_att_mastery = []

for i in ['att1', 'att2', 'att3']:
  lower = np.percentile(stu_att_mastery[i], 2.5)
  upper = np.percentile(stu_att_mastery[i], 97.5)
  
  lower_stu_att_mastery.append(lower)
  upper_stu_att_mastery.append(upper)


# attribute reliability per skill
att_rel = (
  stu_att_mastery[['att1',
                   'att2',
                   'att3']]
  .mean()
  .reset_index()
)

att_rel['lower_ci'] = lower_stu_att_mastery
att_rel['upper_ci'] = upper_stu_att_mastery

att_rel = att_rel.rename(columns = {0: 'avg_prob'})

att_rel

plt.bar(att_rel['att'], att_rel['avg_prob'], color = 'green', edgecolor = 'black')
plt.ylim(0, 1)
plt.yticks(np.arange(0, 1.05, .1))
plt.show()

plt.clf()

# class reliability
stu_class = pd.DataFrame({
  'parameters': post_df.filter(regex = '^prob_resp_class').columns,
  'avg_prob': post_df.filter(regex = '^prob_resp_class').mean().reset_index(drop = True)
})

stu_class[['drop', 'other']] = stu_class['parameters'].str.split(pat = '[', expand = True)
stu_class[['id', 'lat_class']] = stu_class['other'].str.split(',', expand = True)
stu_class['lat_class'] = stu_class['lat_class'].str.replace(']', '')

stu_class.head()

stu_class = stu_class[['id', 'lat_class', 'avg_prob']]
stu_class['id'] = stu_class['id'].astype(int)

max_prob = stu_class.groupby('id')['avg_prob'].max().reset_index()
max_prob = max_prob.rename(columns = {'avg_prob': 'max_prob'})

print(stu_class.head())

class_prob_join = stu_class.merge(max_prob)

class_prob_join.head()
class_prob_join.shape

print(
  class_prob_join
  .loc[class_prob_join['avg_prob'] == class_prob_join['max_prob']]
  .sort_values('id')
  .round(2)
  .value_counts('lat_class')
)

print(
  class_prob_join
  .loc[(class_prob_join['avg_prob'] == class_prob_join['max_prob'])]
  .loc[class_prob_join['max_prob'] < .5]
  .sort_values('max_prob')
)

stu_class_member = []

for i in np.arange(1, np.unique(class_prob_join['id']).max(), 1):
  member = class_prob_join.loc[(class_prob_join['id'] == i)]
  stu_class_member.append(member)

stu_class_member[0]

patt_rel = (
  class_prob_join
  .loc[class_prob_join['avg_prob'] == class_prob_join['max_prob']]
  .sort_values('id')
  .drop(columns = 'avg_prob')
)

patt_rel['id'] = patt_rel['id'].astype(int)
patt_rel['lat_class'] = patt_rel['lat_class'].astype(int)

# Overall pattern reliability
patt_rel['max_prob'].mean()


# Pattern reliability by MAP class
(
  patt_rel
  .groupby('lat_class')['max_prob']
  .mean()
  .reset_index()
  .rename(columns = {'max_prob': 'avg_prob'})
  .merge((patt_rel
          .groupby('lat_class')['max_prob']
          .quantile([.025, .975])
          .reset_index()
          .rename(columns = {'level_1': 'uncertainity',
                     'max_prob': 'prob'})))
)



# simulated data to see accuracy
y = pd.DataFrame(stan_dict['Y'])

y.columns = [f'item_{i+1}' for i in range(9)]

y_rep = post_df.filter(regex = '^Y_rep')

print(y.head())
print(y_rep.head())

# y_count = []

# for i in range(y_rep.shape[1]):
#   counts = y_rep.iloc[i].value_counts(normalize = True)
#   y_count.append(counts)

# print(y_count)

yrep_prob = pd.DataFrame({
  'mean': y_rep.mean(),
  'std': y_rep.std(),
  'median': y_rep.median()
}).reset_index()

yrep_prob
y_describe = y.agg(['mean', 'std', 'median']).reset_index()

yrep_prob[['drop', 'other']] = yrep_prob['index'].str.split(pat = '[', expand = True)
yrep_prob[['id', 'item']] = yrep_prob['other'].str.split(pat = ',', expand = True)
yrep_prob['item'] = yrep_prob['item'].str.replace(']', '')

yrep_prob = yrep_prob.drop(columns = ['drop', 'other', 'index'])

yrep_prob['id'] = yrep_prob['id'].astype(int)
yrep_prob['item'] = yrep_prob['item'].astype(int)


# posterior predictive p-value (PPP)

yrep_prob.head()
y_describe.head()

def ppp_func(item_num, stat):
  print(
    pd
    .Series(np
            .where(
              yrep_prob
              .loc[yrep_prob['item'] == item_num, f'{stat}'] > np
              .array(y_describe
              .loc[y_describe['index'] == f'{stat}', f'item_{item_num}'])[0],
              1,
              0))
    .mean()
    )

for i in np.arange(1, 10):
  ppp_func(item_num = i, stat = 'mean')

for i in np.arange(1, 10):
  ppp_func(item_num = i, stat = 'std')
  
for i in np.arange(1, 10):
  ppp_func(item_num = i, stat = 'median')



pn.ggplot.show(
  pn.ggplot(
    yrep_prob.loc[yrep_prob['item'] == 9],
    pn.aes('mean')
  )
  + pn.geom_histogram()
  + pn.geom_vline(xintercept = y_describe.loc[y_describe['index'] == 'mean','item_9'])
)

# yrep_prob['correct'] = np.where(yrep_prob['mean'] >= .5, 1, 0)


# yrep_wide = (
#   yrep_prob
#   .pivot(
#     index = 'id',
#     columns = 'item',
#     values = 'correct')
#   .reset_index(drop = True)
# )

# yrep_wide.columns = [f'item_{i+1}' for i in range(9)]

# yrep_wide = yrep_wide.reset_index()

# yrep_wide.head()

