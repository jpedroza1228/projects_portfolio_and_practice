import os
import numpy as np
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt
import plotnine as pn

mycolor = 'seagreen'

os.environ['QT_API'] = 'PyQt6'

pd.set_option('display.max_columns', None)
pd.options.mode.copy_on_write = True
matplotlib.rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

rate_aug = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
rate_sept = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')

rate_aug.columns
rate_aug.head()
rate_aug.info()

# contain_mf = (
#   rate_aug
#   .groupby('title')['sex']
#   .transform(lambda x: {'M', 'F'}.issubset(x.unique()))
#   .fillna(False)
# )
# contain_mf
# aug_filter = rate_aug.loc[contain_mf]
 
# aug_filter['games'].value_counts().reset_index()
# aug_filter['games_bi'] = np.where(aug_filter['games'] == 0, 0, 1)
# aug_filter

aug_filter = rate_aug.loc[rate_aug['games'] == 0]
sept_filter = rate_sept.loc[rate_sept['games'] > 0]

aug_lowest = aug_filter[['id', 'games', 'rating']].sort_values('rating').head(5000)
aug_lowest

# aug = aug_lowest.merge(aug_filter, 'left')

combo = aug_lowest.merge(sept_filter, 'inner', 'id')

combo.head()
combo['diff'] = combo['rating_y'] - combo['rating_x']

pn.ggplot.show(
  pn.ggplot(combo, pn.aes('diff', 'games_y'))
  + pn.geom_point(alpha = .7, color = mycolor)
  + pn.geom_smooth(method = 'lm')
  + pn.labs(title = 'Relationship Between Rating Difference and Number of Games Played',
            subtitle = 'For the Lowest 5000 Rated Chess Players That Played 0 Games in August',
            x = 'Rating Difference',
            y = 'Number of Games Played in September')
  + pn.theme_light()
)

pn.ggplot.show(
  pn.ggplot(combo, pn.aes('diff', 'games_y'))
  + pn.geom_point(alpha = .7, color = mycolor)
  # + pn.geom_smooth(method = 'lm')
  + pn.geom_smooth(formula = 'y ~ x + x**2', method = 'lm')
  + pn.labs(title = 'Relationship Between Rating Difference and Number of Games Played',
            subtitle = 'For the Lowest 5000 Rated Chess Players That Played 0 Games in August',
            x = 'Rating Difference',
            y = 'Number of Games Played in September')
  + pn.theme_light()
)

combo_long = combo.melt(id_vars = ['id', 'fed', 'sex', 'games_x', 'games_y', 'bday'],
           value_vars = ['rating_x', 'rating_y'])

combo_long.head()
combo_long['variable'] = np.where(combo_long['variable'] == 'rating_x', 1, 2)



import statsmodels.api as sm
y = combo_long['value']
x = combo_long['variable']
x = sm.add_constant(x)

results = sm.OLS(y, x).fit()
results.summary()

x2 = combo_long[['variable', 'games_y']]
# x['variable'] = x['variable'] - x['variable'].mean()
x2['games_y'] = x2['games_y'] - x2['games_y'].mean() 
x2['interaction'] = x2['variable']*x2['games_y']
x2 = sm.add_constant(x2)

results2 = sm.OLS(y, x2).fit()
results2.summary()

pn.ggplot.show(
  pn.ggplot(combo_long, pn.aes('games_y', 'value'))
  + pn.geom_point(pn.aes(color = 'factor(variable)'), alpha = .3)
  + pn.geom_smooth(pn.aes(color = 'factor(variable)'), method = 'lm', se = False)
  + pn.theme_light()
)

import plotly.express as px
fig = px.line(combo_long,
              x = 'variable',
              y = 'value',
              color = 'id',
              text = 'games_y',
              hover_data = {
                'games_y': True,
                'variable': False,
                'value': True,
                'id': True
              })
fig.show()


import polars as pl
import os
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import plotnine as pn

mycolor = 'seagreen'

os.environ['QT_API'] = 'PyQt6'

matplotlib.rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

rate_aug = pl.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_august.csv')
rate_sept = pl.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-23/fide_ratings_september.csv')

aug_filter = (
  rate_aug
  .filter(pl.col('games') == 0)
)

sept_filter = (
  rate_sept
  .filter(pl.col('games') > 0)
)

aug_lowest = (
  aug_filter
  .select(pl.col('id'),
          pl.col('games'),
          pl.col('rating'))
  .sort(pl.col('rating'))
  .head(5000)
)

combo = aug_lowest.join(sept_filter, 'id', 'inner')
combo = combo.with_columns((pl.col('rating_right') - pl.col('rating')).alias('diff'))

combo = combo.with_columns((2025 - pl.col('bday')).alias('age'))

combo = (
  combo
  .with_columns(pl.when(pl.col('age') <= 10)
                .then(pl.lit('10 or younger'))
                .when(pl.col('age') <= 20)
                .then(pl.lit('11-20'))
                .when(pl.col('age') <= 30)
                .then(pl.lit('21-30'))
                .when(pl.col('age') <= 50)
                .then(pl.lit('31-50'))
                .otherwise(pl.lit('51+'))
                .alias('age_cat'))
)

# combo = combo.with_columns(pl.col('age_cat').cast(pl.Categorical))

combo.head()

pn.ggplot.show(
  pn.ggplot(combo, pn.aes('age'))
  + pn.geom_histogram(color = mycolor)
  + pn.theme_light()
)

import pyarrow as pa
combo_panda = pa.table(combo).to_pandas()
# plotnine version = 0.15.0
# polars version = 1.33.1
# pyarrow version = 21.0.0

pn.ggplot.show(
  pn.ggplot(combo_panda, pn.aes('age'))
  + pn.geom_histogram(color = mycolor)
  + pn.theme_light()
)

pn.ggplot.show(
  pn.ggplot(combo_panda, pn.aes('diff', 'games_right'))
  + pn.geom_point(pn.aes(color = 'age_cat'), alpha = .7, size = 2)
  + pn.geom_smooth(method = 'lm')
  + pn.labs(title = 'Relationship Between Rating Difference and Number of Games Played',
            subtitle = 'For the Lowest 5,000 Rated Chess Players That Played 0 Games in August',
            x = 'Rating Difference',
            y = 'Number of Games Played in September')
  + pn.theme_light()
)
