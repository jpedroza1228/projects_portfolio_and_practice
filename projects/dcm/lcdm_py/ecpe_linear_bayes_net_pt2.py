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
import joblib
from pyhere import here
from janitor import clean_names

os.environ['QT_API'] = 'PyQt6'

pd.set_option('display.max_columns', None)
pd.options.mode.copy_on_write = True
matplotlib.rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

# ecpe dataset
y = pd.read_csv(here('projects/dcm/lcdm_py/ecpe_data.csv')).clean_names(case_type = 'snake')

alpha = pd.DataFrame([(x, y, z) for x in np.arange(2) for y in np.arange(2) for z in np.arange(2)])
alpha = alpha.rename(columns = {0: 'trait1', 1: 'trait2', 2: 'trait3'})

# with open(here('projects/dcm/lcdm_py/ecpe_lcdm_linear.pkl'), "rb") as f:
#     data_list = pickle.load(f)

# stan_model = data_list[0]
# fit = data_list[1]

# CHOOSE A MODEL
# stan_list = joblib.load('projects/dcm/lcdm_py/ecpe_lcdm_linear_jl_compress.joblib')
# stan_list = joblib.load('projects/dcm/lcdm_py/ecpe_lcdm_convergent_jl_compress.joblib')
stan_list = joblib.load('projects/dcm/lcdm_py/ecpe_lcdm_divergent_jl_compress.joblib')

stan_model = stan_list[0]
fit = stan_list[1]

# model diagnostics

# fit.summary()['R_hat'].sort_values(ascending = False)
# fit.diagnose()

# draws as array
# fit.draws()[[0]]

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

idata = az.from_cmdstanpy(
    posterior = fit,
    posterior_predictive = ['Y_rep'],
    observed_data = {'Y': y},
    log_likelihood = ['log_item'])

name_mapping = {'Y_rep': 'Y'}
idata = idata.rename(name_dict = name_mapping, groups = ["posterior_predictive"])

# print(idata.posterior)
# print(idata.posterior_predictive)
# print(idata.sample_stats)
# print(idata.observed_data)

# model visuals
# az.plot_bpv(idata, kind = 't_stat', t_stat = .5)
# plt.show()
# plt.clf()

# az.plot_ppc(idata, data_pairs = {'Y': 'Y'}, num_pp_samples = 10, alpha = .05, textsize = 14)
# plt.show()
# plt.clf()

# az.plot_ppc(idata, data_pairs = {'Y': 'Y'}, alpha = .05, num_pp_samples = 10, kind = 'cumulative', textsize = 14)
# plt.show()
# plt.clf()


# calculate m2 value




# Posterior Predictive Model Checks (PPMC)

# Count proportion of students that got item correct
# compare to replicated dataframe
y.shape

df_resp_correct = y[y.columns].sum(axis = 1).reset_index()
df_resp_correct = df_resp_correct.rename(columns = {'index': 'student', 0: 'correct'})
df_resp_correct['student'] = df_resp_correct['student'] + 1
df_resp_correct_count = df_resp_correct['correct'].value_counts().reset_index().rename(columns = {'count': 'real_count'})

y_rep = post_df.filter(regex = '^Y_rep')
# y_rep.shape[1]

# 1 sample (to make sure everything works for function & loop)
# y_rep_sample1 = y_rep.iloc[0, :].reset_index()
# y_rep_sample1.head()

# y_rep_sample1 = y_rep_sample1.rename(columns = {0: 'value'})
# y_rep_sample1[['drop', 'other']] = y_rep_sample1['index'].str.split(pat = '[', expand = True)
# y_rep_sample1[['student', 'item']] = y_rep_sample1['other'].str.split(pat = ',', expand = True)
# y_rep_sample1['item'] = y_rep_sample1['item'].str.replace(']', '')
# y_rep_sample1 = y_rep_sample1[['student', 'item', 'value']]
# y_rep_sample1[['student', 'item']] = y_rep_sample1[['student', 'item']].astype(int)
# y_rep_sample1 = y_rep_sample1.pivot(index = 'student', columns = 'item', values = 'value').reset_index(drop = True).sum(axis = 1).reset_index()
# y_rep_sample1 = y_rep_sample1.rename(columns = {'index': 'student', 0: 'correct'})
# y_rep_sample1['student'] = y_rep_sample1['student'] + 1
# y_rep_sample1_count = y_rep_sample1['correct'].value_counts().reset_index()

# resp_correct_compare_sample1 = (
#   df_resp_correct_count
#   .merge(y_rep_sample1_count, how = 'outer')
#   .melt(id_vars = 'correct', value_vars = ['count', 'real_count'])
# )

# pn.ggplot.show(
#   pn.ggplot(resp_correct_compare_sample1, pn.aes('factor(correct)',
#                                                  'value',
#                                                  color = 'factor(variable)',
#                                                  group = 'factor(variable)'))
#   + pn.geom_point()
#   + pn.geom_line()
#   + pn.theme_light()
# )

# loop for all the samples correct responses
def sample_maker(sample, row):
    sample = sample.rename(columns = {row: 'value'})
    sample[['drop', 'other']] = sample['index'].str.split(pat = '[', expand = True)
    sample[['student', 'item']] = sample['other'].str.split(pat = ',', expand = True)
    sample['item'] = sample['item'].str.replace(']', '')
    sample = sample[['student', 'item', 'value']]
    sample[['student', 'item']] = sample[['student', 'item']].astype(int)
    sample = sample.pivot(index = 'student', columns = 'item', values = 'value').reset_index(drop = True).sum(axis = 1).reset_index()
    sample = sample.rename(columns = {'index': 'student', 0: 'correct'})
    sample['student'] = sample['student'] + 1
    # sample_count = sample['correct'].value_counts().reset_index()
    
    return sample

sample_maker(sample = y_rep.iloc[2736, :].reset_index(), row = 2736)

sample_correct_rep = []

# don't run this, run the list comprehension below
# for i in np.arange(y_rep.shape[0]):
#   sample_num = y_rep.iloc[i, :].reset_index()
#   sample_df = sample_maker(sample = sample_num, row = i)
#   sample_df['sample'] = i  # add sample number
#   sample_correct_rep.append(sample_df)

# sample_correct_rep

# run this instead
# this will take a long time to run
sample_correct_rep = [
    sample_maker(y_rep.iloc[i, :].reset_index(), i).assign(sample = i)
    for i in range(y_rep.shape[0])
]

(
  joblib.dump(sample_correct_rep,
              'projects/dcm/lcdm_py/ecpe_lcdm_divergent_correct_per_student_all_draws.joblib',
              compress = 3)
)

# sample_correct_rep = joblib.load('projects/dcm/lcdm_py/ecpe_lcdm_linear_correct_per_student_all_draws.joblib')
# sample_correct_rep = joblib.load('projects/dcm/lcdm_py/ecpe_lcdm_convergent_correct_per_student_all_draws.joblib')
sample_correct_rep = joblib.load('projects/dcm/lcdm_py/ecpe_lcdm_divergent_correct_per_student_all_draws.joblib')

# # Then combine all into a single dataframe
sample_correct_combined = pd.concat(sample_correct_rep, ignore_index = True)
sample_correct_combined = sample_correct_combined.groupby('sample')['correct'].value_counts().reset_index()
sample_correct_combined['sample'] = sample_correct_combined['sample'] + 1
sample_correct_combined.head()

correct_compare = (
  sample_correct_combined
  .merge(df_resp_correct_count, how = 'outer')
  .melt(id_vars = ['correct', 'sample'], value_vars = ['count', 'real_count'])
)

correct_compare_avg = correct_compare.loc[correct_compare['variable'] == 'count'].groupby('correct')['value'].mean().reset_index()

pn.ggplot.show(
  pn.ggplot(correct_compare, pn.aes('factor(correct)',
                                    'value'))
  + pn.geom_point(pn.aes(color = 'variable'))
  + pn.geom_line(correct_compare_avg, pn.aes('factor(correct)',
                                             'value',
                                             group = 1),
                 color = 'seagreen', size = 1.25, linetype = 'dashed')
  + pn.scale_color_manual(values = {'count': 'seagreen', 'real_count': 'darkred'})
  + pn.theme_light()
)

# calculate chi-square like statistic comparing number of
# respondents at each score point in each iteration to the expectation
# merged_counts = sample_correct_combined.merge(df_resp_correct_count, on = 'correct', how = 'outer')

# merged_counts[['count', 'real_count']] = merged_counts[['count', 'real_count']].fillna(0)

# merged_counts['chi2'] = ((merged_counts['count'] - merged_counts['real_count'])**2) / (merged_counts['real_count'] + 1e-9)

# chi2_by_sample = (
#     merged_counts
#     .groupby('sample')['chi2']
#     .sum()
#     .reset_index()
#     .rename(columns = {'chi2': 'chi2_stat'})
# )

# merged_counts['chi2'].sort_values(ascending = True)

# pn.ggplot.show(
#   pn.ggplot(df_resp_correct_count, pn.aes('correct', 'real_count'))
#   + pn.geom_col(position = 'dodge', fill = 'seagreen', color = 'black')
#   + pn.geom_vline(chi2_by_sample, pn.aes(xintercept = 'chi2_stat'),
#                   alpha = .3,
#                   linetype = 'dashed')
#   + pn.theme_light()
# )


# simulated data to see accuracy
# y = pd.DataFrame(stan_dict['Y'])
# y.columns = [f'item_{i+1}' for i in range(9)]

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

yrep_prob.iloc[0:5, :].head()
y_describe = y.agg(['mean', 'std', 'median']).reset_index()

yrep_prob[['drop', 'other']] = yrep_prob['index'].str.split(pat = '[', expand = True)
yrep_prob[['id', 'item']] = yrep_prob['other'].str.split(pat = ',', expand = True)
yrep_prob['item'] = yrep_prob['item'].str.replace(']', '')

yrep_prob = yrep_prob.drop(columns = ['drop', 'other', 'index'])

yrep_prob[['id', 'item']] = yrep_prob[['id', 'item']].astype(int)

yrep_prob.iloc[0:5, :].head()

yrep_prob['correct'] = np.where(yrep_prob['mean'] >= .5, 1, 0)

yrep_wide = (
  yrep_prob
  .pivot(
    index = 'id',
    columns = 'item',
    values = 'correct')
  .reset_index(drop = True)
)

yrep_wide.columns = [f'item{i+1}' for i in np.arange(yrep_wide.shape[1])]

yrep_wide = yrep_wide.reset_index()

yrep_wide = yrep_wide.rename(columns = {'index': 'student'})

yrep_wide.head()

# posterior predictive p-value (PPP)
yrep_prob.head()
yrep_prob.info()
y_describe.head()

y_describe.columns = y_describe.columns.str.replace('item0', 'item')

yrep_prob.loc[yrep_prob['item'] == 1, 'mean']
y_describe.loc[y_describe['index'] == 'mean', 'item1']

def ppp_func(df, item_num, stat):
    thresh = np.array(y_describe.loc[y_describe['index'] == stat, f'item{item_num}'])[0]
    cond = df.loc[df['item'] == item_num, stat] > thresh
    ppp_val = np.where(cond, 1, 0).mean()
    return ppp_val


means = [ppp_func(df = yrep_prob, item_num = i, stat = 'mean') for i in np.arange(1, (y_describe.shape[1]))]
stds = [ppp_func(df = yrep_prob, item_num = i, stat = 'std') for i in np.arange(1, (y_describe.shape[1]))]
medians = [ppp_func(df = yrep_prob, item_num = i, stat = 'median') for i in np.arange(1, (y_describe.shape[1]))]

ppp_df = pd.DataFrame({'means': pd.Series(means),
                       'stds': pd.Series(stds),
                       'medians': pd.Series(medians)})

ppp_df = ppp_df.reset_index()
ppp_df = ppp_df.rename(columns = {'index': 'item'})
ppp_df['item'] = ppp_df['item'] + 1

ppp_df.head()
y_describe


pn.ggplot.show(
  pn.ggplot(ppp_df, pn.aes('item', 'means'))
  + pn.geom_point(color = 'seagreen',
                  size = 2)
  + pn.geom_hline(yintercept = .5, linetype = 'dashed')
  + pn.geom_hline(yintercept = .025, linetype = 'dotted')
  + pn.geom_hline(yintercept = .975, linetype = 'dotted')
  + pn.scale_x_continuous(limits = [1, ppp_df['item'].max() + 1],
                          breaks = np.arange(1, ppp_df['item'].max() + 1))
  + pn.scale_y_continuous(limits = [0, 1.01],
                          breaks = np.arange(0, 1.01, .1))
  + pn.theme_light()
)

pn.ggplot.show(
  pn.ggplot(ppp_df, pn.aes('item', 'stds'))
  + pn.geom_point(color = 'seagreen',
                  size = 2)
  + pn.geom_hline(yintercept = .5, linetype = 'dashed')
  + pn.geom_hline(yintercept = .025, linetype = 'dotted')
  + pn.geom_hline(yintercept = .975, linetype = 'dotted')
  + pn.scale_x_continuous(limits = [1, ppp_df['item'].max() + 1],
                          breaks = np.arange(1, ppp_df['item'].max() + 1))
  + pn.scale_y_continuous(limits = [0, 1.01],
                          breaks = np.arange(0, 1.01, .1))
  + pn.theme_light()
)


# item level fit
# conditional probabilities using pi matrix compared to averages of y_describe (data)
pi_df = post_df.filter(regex = '^pi')

pi_df_avg = pi_df.mean().reset_index()
pi_df_avg[['drop', 'other']] = pi_df_avg['index'].str.split(pat = '[', expand = True)
pi_df_avg[['item', 'lat_class']] = pi_df_avg['other'].str.split(pat = ',', expand = True)
pi_df_avg['lat_class'] = pi_df_avg['lat_class'].str.replace(']', '')
pi_df_avg = pi_df_avg.rename(columns = {0: 'avg_prob'})
pi_df_avg = pi_df_avg[['lat_class', 'item', 'avg_prob']]

pi_df_avg.head()

pi_cred_int = (
  pi_df
  .quantile([.025, .975])
  .reset_index()
)

pi_cred_int = pi_cred_int.melt(id_vars = 'index', value_vars = pi_cred_int.drop(columns = 'index').columns)

pi_cred_int[['drop', 'other']] = pi_cred_int['variable'].str.split(pat = '[', expand = True)
pi_cred_int[['item', 'lat_class']] = pi_cred_int['other'].str.split(pat = ',', expand = True)
pi_cred_int['lat_class'] = pi_cred_int['lat_class'].str.replace(']', '')
pi_cred_int = pi_cred_int.rename(columns = {'index': 'cred_int', 'value': 'cred_int_value'})
pi_cred_int = pi_cred_int[['lat_class', 'item', 'cred_int', 'cred_int_value']]

pi_cred_int = pi_cred_int.pivot(index = ['lat_class', 'item'], columns = 'cred_int', values = 'cred_int_value').reset_index()
pi_cred_int = pi_cred_int.rename(columns = {0.025: 'lower_ci', 0.975: 'upper_ci'})

pi_df_uncertain = pi_df_avg.merge(pi_cred_int, how = 'left', on = ['lat_class', 'item'])

pi_df_uncertain[['lat_class', 'item']] = pi_df_uncertain[['lat_class', 'item']].astype(int) 

pi_cond = (
  pi_df_uncertain['lat_class'] == 1,
  pi_df_uncertain['lat_class'] == 2,
  pi_df_uncertain['lat_class'] == 3,
  pi_df_uncertain['lat_class'] == 4,
  pi_df_uncertain['lat_class'] == 5,
  pi_df_uncertain['lat_class'] == 6,
  pi_df_uncertain['lat_class'] == 7,
  pi_df_uncertain['lat_class'] == 8
)

alpha['profile'] = (
  alpha['trait1'].astype(str)
  + alpha['trait2'].astype(str)
  + alpha['trait3'].astype(str)
)

alpha['profile'] = alpha['profile'].astype(object)

alpha

pi_choice = (
  alpha['profile'][0],
  alpha['profile'][1],
  alpha['profile'][2],
  alpha['profile'][3],
  alpha['profile'][4],
  alpha['profile'][5],
  alpha['profile'][6],
  alpha['profile'][7]
)

pi_df_uncertain['profile'] = np.select(pi_cond, pi_choice, default = 'Other')

pi_df_uncertain.head()



pi_df_long = pi_df.melt(value_vars = pi_df.columns)

pi_df_long[['drop', 'other']] = pi_df_long['variable'].str.split(pat = '[', expand = True)
pi_df_long[['item', 'lat_class']] = pi_df_long['other'].str.split(pat = ',', expand = True)
pi_df_long['lat_class'] = pi_df_long['lat_class'].str.replace(']', '')

pi_df_long = pi_df_long[['lat_class', 'item', 'value']]
pi_df_long = pi_df_long.rename(columns = {'value': 'prob'})

pi_df_long[['lat_class', 'item']] = pi_df_long[['lat_class', 'item']].astype(int)

pi_long_cond = (
  pi_df_long['lat_class'] == 1,
  pi_df_long['lat_class'] == 2,
  pi_df_long['lat_class'] == 3,
  pi_df_long['lat_class'] == 4,
  pi_df_long['lat_class'] == 5,
  pi_df_long['lat_class'] == 6,
  pi_df_long['lat_class'] == 7,
  pi_df_long['lat_class'] == 8
)

pi_df_long['profile'] = np.select(pi_long_cond, pi_choice, default = 'Other')

pi_df_long.shape
pi_df_long.head()

def ppp_prob_func(df, item_num, lat_class):
    thresh = np.array(y_describe.loc[y_describe['index'] == 'mean', f'item{item_num}'])[0]
    cond = df.loc[(df['item'] == item_num) & (df['lat_class'] == lat_class), 'prob'] > thresh
    ppp_val = np.where(cond, 1, 0).mean()
    return ppp_val

ppp_prob_func(pi_df_long, 1, 4)

probs = [
    {
        'item': i,
        'lat_class': j,
        'ppp_value': ppp_prob_func(df = pi_df_long, item_num = i, lat_class = j)
    }
    for i in range(1, pi_df_long['item'].max() + 1)
    for j in range(1, pi_df_long['lat_class'].max() + 1)
]

probs_df = pd.DataFrame(probs)

pi_df_uncertain.info()
probs_df.info()

pi_df_ppp = pi_df_uncertain.merge(probs_df, 'left', on = ['lat_class', 'item'])
pi_df_ppp = pi_df_ppp.sort_values('item')

pi_df_ppp.loc[pi_df_ppp['item'] == 1]

# fit statistics
# absolute difference is greater than 2.5 * SE of the difference
az.waic(idata)
az.loo(idata)

def acceptable_fit_stat(inference_data, func_name = ['waic', 'loo']):
  if func_name == 'waic':
    est = np.abs(az.waic(idata).iloc[0])
    se = az.waic(idata).iloc[1]
    
    if est > se * 2.5:
      print('Absolute difference is greater than 2.5 x the standard error of the difference. Model is acceptable.')
      
    else:
      print('Absolute difference is not greater than 2.5 x the standard error of the difference. Model is not acceptable.')
  elif func_name == 'loo':
    est = np.abs(az.loo(idata).iloc[0])
    se = az.loo(idata).iloc[1]
    
    if est > se * 2.5:
      print('Absolute difference is greater than 2.5 x the standard error of the difference. Model is acceptable.')
      
    else:
      print('Absolute difference is not greater than 2.5 x the standard error of the difference. Model is not acceptable.')

acceptable_fit_stat(inference_data = idata, func_name = 'waic')
acceptable_fit_stat(inference_data = idata, func_name = 'loo')

# diagnostics
# az.rhat(idata) # estimate of rank normalized splitR-hat for set of traces
# az.bfmi(idata) # estimated bayesian fraction of missing information
# az.ess(idata) # estimate of effective sample size
# az.mcse(idata) # markov chain standard error statistic
# az.psens(idata) # power-scaling sensitivity diagnostic

# reliability (amount of students that mastered each latent attribute)

# # probability of student mastery of each attribute
az.plot_forest(idata, var_names = 'prob_resp_attr', colors = 'seagreen')
plt.show()
plt.clf()

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

# can choose what you consider the threshold for mastery
stu_att_mastery['att1_bi'] = pd.Series(np.where(stu_att_mastery['1'] >= .7, 1, 0))
stu_att_mastery['att2_bi'] = pd.Series(np.where(stu_att_mastery['2'] >= .7, 1, 0))
stu_att_mastery['att3_bi'] = pd.Series(np.where(stu_att_mastery['3'] >= .7, 1, 0))

stu_att_mastery['profile'] = (
  stu_att_mastery['att1_bi'].astype(str)
  + stu_att_mastery['att2_bi'].astype(str)
  + stu_att_mastery['att3_bi'].astype(str)  
)

stu_att_mastery = stu_att_mastery.rename(columns = {'1': 'att1', '2': 'att2', '3': 'att3'})

# attribute level probabilities (att\\d) & classifications (bi)
stu_att_mastery.head()

for i in ['att1_bi', 'att2_bi', 'att3_bi']:
  print(stu_att_mastery[i].value_counts(normalize = True))

stu_att_mastery['profile'].value_counts(normalize = True)

# attribute reliability per skill
att_rel = (
  stu_att_mastery[['att1',
                   'att2',
                   'att3']]
  .mean()
  .reset_index()
)

att_rel = att_rel.rename(columns = {0: 'avg_prob'})

att_uncertain_rel = (
  stu_att_mastery[['att1',
                   'att2',
                   'att3']]
  .quantile([.025, .975])
  .reset_index()
  .melt(id_vars = 'index', value_vars = ['att1', 'att2', 'att3'])
  .pivot(index = 'att', columns = 'index', values = 'value')
  .rename(columns = {0.025: 'lower_ci95', 0.975: 'upper_ci95'})
  .reset_index()
)

att_uncertain_rel = att_uncertain_rel.merge(att_rel, how = 'left', on = 'att')

# classification reliability
att_uncertain_rel

pn.ggplot.show(
  pn.ggplot(att_uncertain_rel, pn.aes('att', 'avg_prob'))
  + pn.geom_errorbar(pn.aes(ymin = 'lower_ci95', ymax = 'upper_ci95'), alpha = .3)
  + pn.geom_point(pn.aes(color = 'avg_prob'), size = 4)
  + pn.theme_light()
  + pn.theme(legend_position = None)
)

# class reliability

# probability of student in each class
az.plot_forest(idata, var_names = 'prob_resp_class', colors = 'seagreen')
plt.show()
plt.clf()

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

# this is profile level probabilities
stu_class.sort_values(by = ['avg_prob'], ascending = False)

max_prob = stu_class.groupby('id')['avg_prob'].max().reset_index()
max_prob = max_prob.rename(columns = {'avg_prob': 'max_prob'})

print(stu_class.head())

class_prob_join = stu_class.merge(max_prob)

class_prob_join.head()
class_prob_join.shape

# this is profile level classificaiton
profile_level_class = (
  class_prob_join
  .loc[class_prob_join['avg_prob'] == class_prob_join['max_prob']]
  .drop(columns = 'avg_prob')
  .sort_values(by = 'id')
)

# this is profile reliability
profile_level_class['max_prob'].mean()


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

patt_rel[['id', 'lat_class']] = patt_rel[['id', 'lat_class']].astype(int)

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
