import pandas as pd
import numpy as np
import seaborn as sns
import plotnine as pn
from matplotlib import RcParams, pyplot as plt
import plotly.express as px
from pyhere import here

pd.set_option('display.max_columns', None)
pd.set_option('mode.copy_on_write', True)
RcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

covid = pd.read_csv(here('other_topics/posit/covid.csv'))

covid.head()
covid.info()

(
  covid
  .loc[
    covid['state']
    .isin(['NY', 'CA', 'TX', 'WA', 'OR'])]['state']
  .unique()
)

(
  covid
  .loc[
    ~covid['state']
    .isin(['NY', 'CA', 'TX', 'WA', 'OR'])]['state']
  .unique()
)



covid['date2'] = pd.to_datetime(covid['date'])
covid_ny = covid.loc[covid['state'] == 'NY']

# plotly
plotly_plot = (
  px
  .line(covid_ny,
        x = 'date2',
        y = ['cases', 'deaths'],
        color_discrete_sequence = ['blue', 'red'])
)
plotly_plot.show()

# plotnine
pn.ggplot.show(
  pn.ggplot(covid_ny)
  + pn.geom_line(pn.aes(x = 'date2', y = 'cases', group = 1))
  + pn.geom_line(pn.aes(x = 'date2', y = 'deaths', group = 1))
  + pn.theme_light()
)

# seaborn
sns.lineplot(covid_ny, x = 'date2', y = 'cases')
sns.lineplot(covid_ny, x = 'date2', y = 'deaths')
plt.show()

plt.clf()

# matplotlib

plt.plot(covid_ny['date2'], covid_ny['cases'])
plt.plot(covid_ny['date2'], covid_ny['deaths'], linestyle = 'dashed')