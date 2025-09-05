import pandas as pd
import numpy as np
import plotnine as pn
from pyhere import here

pd.set_option('display.max_columns', None)
pd.set_option('mode.copy_on_write', True)
RcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

covid = pd.read_csv(here('other_topics/posit/covid.csv'))

covid.head()
covid.info()

covid['date2'] = pd.to_datetime(covid['date'])
covid[['day', 'month', 'year']] = covid['date'].str.split(pat = '-', expand = True)
covid.info()

covid[['day', 'month', 'year']] = covid[['day', 'month', 'year']].astype(int)

covid.info()

covid['state2'] = pd.Categorical(covid['state'])
covid['month2'] = pd.Categorical(covid['month'], ordered = True)

covid_sub = covid.loc[(covid['state'].isin(['CA', 'OR', 'WA']))]

covid_sub.head()

covid_sub['state2'] = covid_sub['state2'].cat.remove_unused_categories()

pn.ggplot.show(
  pn.ggplot(covid_sub, pn.aes('month', 'cases'))
  + pn.geom_point(alpha = .3)
  + pn.geom_smooth(pn.aes(color = 'factor(state)'),
                   se = False)
  + pn.scale_x_continuous(limits = [1, 12], breaks = np.arange(1, covid_sub['month'].max() + 1, 1))
  + pn.theme_light()
)

pn.ggplot.show(
  pn.ggplot(covid_sub, pn.aes('month', 'cases'))
  + pn.geom_point(alpha = .3)
  + pn.geom_smooth(pn.aes(color = 'state2'),
                   se = False)
  + pn.scale_x_continuous(limits = [1, 12], breaks = np.arange(1, covid_sub['month'].max() + 1, 1))
  + pn.theme_light()
)

# klaus LLM code from learner

# Example dictionary with two value columns
df_dict = {
    'A': pd.DataFrame({'date': ['2025-01-01', '2025-01-02'], 'value1': [1, 2], 'value2': [10, 20]}),
    'B': pd.DataFrame({'date': ['2025-01-01', '2025-01-02'], 'value1': [4, 5], 'value2': [40, 50]}),
    'C': pd.DataFrame({'date': ['2025-01-01', '2025-01-02'], 'value1': [6, 1], 'value2': [60, 10]}),
    'D': pd.DataFrame({'date': ['2025-01-01', '2025-01-02'], 'value1': [7, 3], 'value2': [70, 30]})
}
# Convert 'date' to datetime
for df in df_dict.values():
    df['date'] = pd.to_datetime(df['date'])
# Define subset
subset_keys = ['A', 'C']
# Add group column
for key, df in df_dict.items():
    df['group'] = 'subset' if key in subset_keys else 'non-subset'
# Combine all DataFrames
combined_df = pd.concat(df_dict.values(), ignore_index=True)
# Group by date and group, summing both value columns
summary_df = combined_df.groupby(['date', 'group'], as_index=False)[['value1', 'value2']].sum()
print(summary_df)


df_all = pd.DataFrame({
    'date': ['2025-01-01', '2025-01-02', '2025-01-01', '2025-01-02', '2025-01-01', '2025-01-02', '2025-01-01', '2025-01-02'],
    'value1': [1, 2, 4, 5, 6, 1, 7, 3],
    'value2': [10, 20, 40, 50, 60, 10, 70, 30],
    'source': ['A', 'A', 'B', 'B', 'C', 'C', 'D', 'D']
})
df_all['date'] = pd.to_datetime(df_all['date'])
subset_keys = ['A', 'C']
df_all['group'] = 'non-subset'
df_all.loc[df_all['source'].isin(['A', 'C']), 'group'] = 'subset' 
result = df_all.groupby(['date', 'group']).sum()
result