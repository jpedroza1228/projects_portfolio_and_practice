import os
import pandas as pd
import numpy as np
import plotnine as pn
import matplotlib
import matplotlib.pyplot as plt
import joblib
from pyhere import here
from janitor import clean_names
# import pydantic 
# import pytest
# import fastapi

os.environ['QT_API'] = 'PyQt6'

pd.set_option('display.max_columns', None)
pd.options.mode.copy_on_write = True
matplotlib.rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

df = pd.read_csv(here('projects/pipeline/stocks.csv')).clean_names(case_type = 'snake')


# train/test split
from sktime.split import temporal_train_test_split
df_sep, df_test = temporal_train_test_split(df, test_size = .2)
df_train, df_val = temporal_train_test_split(df_sep, test_size = .2)


# data wrangling
def wrangle_df(df, date_col, y_col, freq):
  df['ds'] = df[date_col].str.replace(r'T.*', '', regex = True)
  
  df['ds'] = pd.to_datetime(df['ds'])
  
  df = df[['ds', y_col]]
  df = df.set_index('ds')
  df = df.asfreq(freq)
  
  return df
  
def mm_yy_impute_df(df, y_col, date_col, Series = True):
  if Series is True:
    df = df.reset_index()
    df['ds_date'] = df[date_col].astype(str)
    df[['year', 'month', 'day']] = df['ds_date'].astype(object).str.split(pat = '-', expand = True)
  
  else:
    df['ds_date'] = df[date_col].astype(str)
    df[['year', 'month', 'day']] = df['ds_date'].astype(object).str.split(pat = '-', expand = True)
      
  df_join = df.groupby(['year', 'month'])[y_col].mean().reset_index()
  df_join = df_join.rename(columns = {y_col: f'{y_col}_imp'})
  
  df_merged = df.merge(df_join, 'left', on = ['year', 'month'])
  df_merged[y_col] = df_merged[y_col].fillna(df_merged[f'{y_col}_imp'])
  
  df_merged = df_merged[['ds', y_col]]
  df_merged = df_merged.set_index('ds')
  
  return df_merged

def anomaly_include(df):
  from adtk.detector import OutlierDetector
  from sklearn.neighbors import LocalOutlierFactor
  
  outlier_detector = OutlierDetector(LocalOutlierFactor(contamination = 0.05))
  df['outlier'] = outlier_detector.fit_detect(df).astype(int)
  
  return df

def model_forecast(y, X = None, y_col = 'close'):
  import joblib
  from sktime.forecasting.fbprophet import Prophet
    
  if X is None:
    forecaster = Prophet()
    forecaster.fit(y[y_col])
    
  else:
    forecaster = Prophet()
    forecaster.fit(y = y[y_col], X = X)
    
  joblib.dump(forecaster, here('projects/pipeline/prophet_model.joblib'))
  
  # if __name__ == "__main__":
  #   model_forecast(y, num)

def prophet_predict(y, num, freq = 'b', date_index = True):
  from sktime.forecasting.base import ForecastingHorizon
  
  if date_index is True:
    last_date = y.index[-1]
    fh_dates = pd.date_range(start = last_date + pd.Timedelta(days = 1), periods = num, freq = freq)
    fh = ForecastingHorizon(fh_dates, is_relative = False)
  
  else:
    fh = ForecastingHorizon(np.arange(1, num))
    
  if X is None:
    y_pred = forecaster.predict(fh = fh)
  
  else:
    y_pred = forecaster.predict(X = X, fh = fh)
    
  return pd.DataFrame(y_pred)


def metric_df(test, num, y_col = 'close', val = None):
  from sktime.performance_metrics.forecasting import MeanSquaredError, MeanAbsoluteError
  
  mae = MeanAbsoluteError()
  mse = MeanSquaredError()
  rmse = MeanSquaredError(square_root = True)

  print("Make sure num == num from the `model_forecast()` function.")
  if val is None:
    df_metric = (
      pd.DataFrame({'term': ['MAE', 'MSE', 'RMSE'],
              'test_score': [mae(test[y_col].iloc[0:num], y_pred),
                        mse(test[y_col].iloc[0:num], y_pred),
                        rmse(test[y_col].iloc[0:num], y_pred)]}).round(3)
    )
  else:
    df_metric = (
      pd.DataFrame({'term': ['MAE', 'MSE', 'RMSE'],
              'val_score': [mae(val[y_col].iloc[0:num], y_pred),
                        mse(val[y_col].iloc[0:num], y_pred),
                        rmse(val[y_col].iloc[0:num], y_pred)],
              'test_score': [mae(test[y_col].iloc[0:num], y_pred),
                        mse(test[y_col].iloc[0:num], y_pred),
                        rmse(test[y_col].iloc[0:num], y_pred)]}).round(3)
    )
    
  return df_metric 

# from sktime.transformations.series.date import DateTimeFeatures
# transformer = DateTimeFeatures(manual_selection = ['is_weekend'])
# transformer.fit_transform(df_train['close']).reset_index()[0].value_counts()

# from adtk.data import validate_series
# dft = validate_series(dft)

train = wrangle_df(df_train, 'date', 'close', 'b')
test = wrangle_df(df_val, 'date', 'close', 'b')

# train = wrangle_df(df_sep, 'date', 'close', 'b')
# test = wrangle_df(df_test, 'date', 'close', 'b')

train = mm_yy_impute_df(train, 'close', 'ds')
train = anomaly_include(train)
# x = train['outlier']
y = train.drop(columns = 'outlier')


test = mm_yy_impute_df(test, 'close', 'ds')
test = anomaly_include(test)
# x = test['outlier']
y_test = test.drop(columns = 'outlier')


# forecasting model
model_forecast(y = y, y_col = 'close')

model_predict(y = y, num = y_test.shape[0] + 1)
  
from sktime.utils.plotting import plot_series
# plot_series(y, y_pred, labels = ['y', 'y_pred'])
plot_series(y_test, y_pred, labels = ['y', 'y_pred'])
plt.show()
# plt.clf()

from datetime import date
today = date.today()

metrics = metric_df(test = y_test, num = y_test.shape[0])
metrics

metrics.to_csv(here(f'projects/pipeline/fit_metrics_{today.strftime('%Y-%m-%d')}.csv'))







# ADDITIONAL STUFF FROM CHAT GPT

import os
import joblib
import pandas as pd
from pyhere import here
from sktime.forecasting.fbprophet import Prophet
from sktime.forecasting.base import ForecastingHorizon
from datetime import timedelta

MODEL_PATH = here("projects/pipeline/prophet_model.joblib")
FORECAST_HORIZON_DAYS = 30

def train_prophet_model(y, y_col='close', X=None):
    """
    Train a Prophet model and save it.
    
    :param y: DataFrame with a DatetimeIndex and a y_col column.
    :param y_col: column name for forecasting.
    :param X: Optional exogenous variables.
    :return: trained model.
    """
    # Reset index and rename for Prophet if needed
    y_df = y.reset_index().rename(columns={'ds': 'ds', y_col: 'y'})
    
    forecaster = Prophet()
    if X is None:
        forecaster.fit(y=y_df)
    else:
        forecaster.fit(y=y_df, X=X)
    
    # Save the model to disk
    joblib.dump(forecaster, MODEL_PATH)
    return forecaster

def load_prophet_model():
    """
    Load the previously saved Prophet model.
    """
    if os.path.exists(MODEL_PATH):
        return joblib.load(MODEL_PATH)
    else:
        raise FileNotFoundError("No saved model found at: " + MODEL_PATH)

def make_prophet_prediction(y, num_days=FORECAST_HORIZON_DAYS, X=None, freq='D'):
    """
    Make forecasts using a saved Prophet model.
    :param y: DataFrame with a DatetimeIndex.
    :param num_days: number of days to forecast.
    :param X: Optional exogenous variables.
    :param freq: frequency string, e.g. 'D' for daily.
    :return: DataFrame of forecasts.
    """
    from sktime.forecasting.base import ForecastingHorizon

    forecaster = load_prophet_model()
    
    # Forecast dates starting the day after the last observed date 
    last_date = y.index[-1]
    fh_dates = pd.date_range(start=last_date + timedelta(days=1), periods=num_days, freq=freq)
    fh = ForecastingHorizon(fh_dates, is_relative=False)
    
    if X is None:
        y_pred = forecaster.predict(fh=fh)
    else:
        y_pred = forecaster.predict(X=X, fh=fh)
    
    # Reset index if needed (so the forecast timeline is accessible)
    return pd.DataFrame(y_pred, index=fh_dates)

if __name__ == '__main__':
    # Example usage in production: read data (e.g. from a database or CSV), train or update the model,
    # generate forecasts, and save them.
    
    # Read training data; use your wrangling and imputation functions to get a cleaned DataFrame 'y'
    from practice import wrangle_df, mm_yy_impute_df, anomaly_include
    from pyhere import here
    
    # Assume df_train is read from your data source.
    # For example, using your data wrangling:
    df_train = pd.read_csv(here('projects/pipeline/stocks.csv')).clean_names(case_type='snake')
    y = wrangle_df(df_train, 'date', 'close', 'D')
    y = mm_yy_impute_df(y, 'close', 'ds')
    y = anomaly_include(y)
    y = y.drop(columns='outlier')
    
    # Train or update the model (you might also check if retraining is necessary)
    model = train_prophet_model(y, y_col='close')
    
    # Make predictions for the next 30 days
    forecast = make_prophet_prediction(y, num_days=30, freq='D')
    print("Forecast:\n", forecast)
    
    # Optionally: export forecast to CSV, or save into a database.
    forecast.to_csv(here('projects/pipeline/forecast.csv'), index=True)