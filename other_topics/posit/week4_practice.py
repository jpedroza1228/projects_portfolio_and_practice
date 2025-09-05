import pandas as pd
import numpy as np
import plotnine as pn
import plotly.express as px
from matplotlib import rcParams
from pyhere import here

# Set some options
pd.set_option('mode.copy_on_write', True)
pd.set_option('display.max_columns', None)
rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

covid = pd.read_csv(here('other_topics/posit/covid.csv'))

covid['month'] = pd.to_datetime(covid['date']).dt.month

month_cond = [
  (covid['month'] == 1),
  (covid['month'] == 2),
  (covid['month'] == 3),
  (covid['month'] == 4),
  (covid['month'] == 5),
  (covid['month'] == 6),
  (covid['month'] == 7),
  (covid['month'] == 8),
  (covid['month'] == 9),
  (covid['month'] == 10),
  (covid['month'] == 11),
  (covid['month'] == 12)
]

month_choice = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

covid['month_extra'] = np.select(month_cond, month_choice, default = 'other')
covid.head()
covid.tail()

covid[['hospitalizations', 'cases', 'deaths', 'tests']].describe().transpose()

covid[['hosp', 'cases', 'deaths', 'tests']] = (pd
  .DataFrame([np.where(covid[i] < 0, 0, covid[i]) for i in covid[['hospitalizations', 'cases', 'deaths', 'tests']]])
  .transpose()
)

# for i in ['hospitalizations', 'cases', 'deaths', 'tests']:
#   covid[i] = np.where(covid[i] < 0, 0, covid[i])


# one option
case_cond = [covid['cases'].le(covid['cases'].quantile(.7)),
             covid['cases'].between(covid['cases'].quantile(.7), covid['cases'].quantile(.9), inclusive = 'both'),
             covid['cases'].gt(covid['cases'].quantile(.9))]

case_choice = ['low', 'med', 'high']

np.select(case_cond, case_choice, default = 'other')

# second option 
def bin_func(x, lower, upper):
  if lower > upper:
    raise ValueError("Lower value must be less than the upper value.")

  if x > upper:
    return 'high'
  elif x < lower:
    return 'low'
  else:
    return 'med'

bin_func_vec = np.vectorize(bin_func)

covid['cases'].quantile([.25, .75]).reset_index()

def quantile(x, value):
  return x.quantile(value)

quantile(covid['cases'], .25)

# will raise error
bin_func_vec(covid['cases'], quantile(covid['cases'], .9), quantile(covid['cases'], .7))

bin_func_vec(covid['cases'], quantile(covid['cases'], .7), quantile(covid['cases'], .9))

covid[['hosp_cat', 'cases_cat', 'deaths_cat', 'tests_cat']] = (pd
  .DataFrame([bin_func_vec(covid[i], quantile(covid[i], .7), quantile(covid[i], .9)) for i in covid[['hosp', 'cases', 'deaths', 'tests']]])
  .transpose()
)

covid.info()

covid.head()

covid_count = (
  covid[['cases_cat', 'tests_cat']]
  .value_counts(normalize = True)
  .reset_index()
)
covid_count
covid_count.info()

covid_count['cases_cat'] = pd.Categorical(covid_count['cases_cat'], categories = ['low', 'med', 'high'], ordered = True)
covid_count['tests_cat'] = pd.Categorical(covid_count['tests_cat'], categories = ['low', 'med', 'high'], ordered = True)

pn.ggplot.show(
  pn.ggplot(covid_count, pn.aes('cases_cat', 'tests_cat'))
  + pn.geom_tile(pn.aes(fill = 'proportion'), color = 'black')
  + pn.geom_text(pn.aes(label = 'round(proportion, 2)'),
  size = 12,
  color = 'red',
  nudge_x = -.3,
  nudge_y = -.3)
  + pn.theme_classic()
  + pn.theme(legend_position = 'none') # argument None does not work here
  )

import seaborn as sns
import matplotlib.pyplot as plt

covid_count_wide = covid_count.pivot(index = 'cases_cat', columns = 'tests_cat', values = 'proportion')

covid_count_wide = covid_count_wide.round(2)

plt.clf()

sns.heatmap(covid_count_wide, annot = True, cbar = False)
plt.show()


# learner extension
def clean_data(df):
    # Drop rows with negative values for hospitalizations, cases, and deaths
    df = df[(df['hospitalizations'] >= 0) &
             (df['cases'] >= 0) &
             (df['deaths'] >= 0)]
    return df

def bin_data(df):
    df['hospitalization_bin'] = pd.cut(df['hospitalizations'],
                                        bins=[0, 100, 1000, float('inf')],
                                        labels=['low', 'medium', 'high'],
                                        right=False)
    df['death_bin'] = pd.cut(df['deaths'],
                             bins=[0, 100, 1000, float('inf')],
                             labels=['low', 'medium', 'high'],
                             right=False)
    df['case_bin'] = pd.cut(df['cases'],
                            bins=[0, 100, 1000, float('inf')],
                            labels=['low', 'medium', 'high'],
                            right=False) 
    return df

covid = bin_data(clean_data(covid))
covid.head()

high_hospitalization_states = covid[covid['hospitalization_bin'] == 'high']
high_death_states = covid[covid['death_bin'] == 'high']
high_case_states = covid[covid['case_bin'] == 'high']

high_hospitalization_and_death_states = set(high_hospitalization_states['state']).intersection(set(high_death_states['state']))
print(f"\nNumber of states with both high hospitalization and high death counts: {len(high_hospitalization_and_death_states)}")
print("States with both high hospitalization and high death counts:")
print(high_hospitalization_and_death_states)

high_hospitalization_and_case_states = set(high_hospitalization_states['state']).intersection(set(high_case_states['state']))
print(f"\nNumber of states with both high hospitalization and high case counts: {len(high_hospitalization_and_case_states)}")
print("States with both high hospitalization and high case counts:")
print(high_hospitalization_and_case_states)

high_death_and_case_states = set(high_death_states['state']).intersection(set(high_case_states['state']))
print(f"\nNumber of states with both high death and high case counts: {len(high_death_and_case_states)}")
print("States with both high death and high case counts:")
print(high_death_and_case_states)

high_all_states = set(high_hospitalization_states['state']).intersection(set(high_death_states['state'])).intersection(set(high_case_states['state']))
print(f"\nNumber of states with high hospitalization, high death, and high case counts: {len(high_all_states)}")
print("States with high hospitalization, high death, and high case counts:")
print(high_all_states)