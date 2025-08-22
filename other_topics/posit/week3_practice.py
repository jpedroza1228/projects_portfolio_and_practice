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