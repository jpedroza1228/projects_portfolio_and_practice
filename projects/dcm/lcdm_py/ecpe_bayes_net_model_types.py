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
ecpe = pd.read_csv(here('projects/dcm/lcdm_py/ecpe_data.csv')).clean_names(case_type = 'snake')
q = pd.read_csv(here('projects/dcm/lcdm_py/ecpe_qmatrix.csv')).clean_names(case_type = 'snake')

ecpe.columns.tolist()
q.columns.tolist()

ecpe.columns = ecpe.columns.str.replace('0', '')

# attribute mastery matrix
alpha = pd.DataFrame([(x, y, z) for x in np.arange(2) for y in np.arange(2) for z in np.arange(2)])
alpha = alpha.rename(columns = {0: 'trait1', 1: 'trait2', 2: 'trait3'})

# stan dictionary data
stan_dict = {
  'J': ecpe.shape[0],
  'I': ecpe.shape[1],
  'K': q.shape[1],
  'C': alpha.shape[0],
  'Y': np.array(ecpe),
  'Q': np.array(q), 
  'alpha': np.array(alpha)
}


# ordinary LCDM
stanfile = os.path.join(here('projects/dcm/lcdm_py/ecpe_lcdm.stan'))
stan_model = CmdStanModel(stan_file = stanfile, cpp_options={'STAN_THREADS': 'TRUE'})

np.random.seed(81425)
fit = stan_model.sample(data = stan_dict, show_console = True, chains = 4)

with open(here('projects/dcm/lcdm_py/ecpe_lcdm.pkl'), 'wb') as f:
  pickle.dump([stan_model, fit], f, protocol = -1)
  
(
  joblib.dump([stan_model, fit],
              'projects/dcm/lcdm_py/ecpe_lcdm_jl_compress.joblib',
              compress = 3)
)

(
  joblib.dump([stan_model, fit],
              'projects/dcm/lcdm_py/ecpe_lcdm_jl.joblib')
)


# linear model
# stanfile = os.path.join(here('projects/dcm/lcdm_py/ecpe_lcdm_bayes_net.stan'))
# stan_model = CmdStanModel(stan_file = stanfile, cpp_options={'STAN_THREADS': 'TRUE'})

# np.random.seed(81425)
# fit = stan_model.sample(data = stan_dict, show_console = True, chains = 4)

# with open(here('projects/dcm/lcdm_py/ecpe_lcdm_linear.pkl'), 'wb') as f:
#   pickle.dump([stan_model, fit], f, protocol = -1)
  
# (
#   joblib.dump([stan_model, fit],
#               'projects/dcm/lcdm_py/ecpe_lcdm_linear_jl_compress.joblib',
#               compress = 3)
# )

# (
#   joblib.dump([stan_model, fit],
#               'projects/dcm/lcdm_py/ecpe_lcdm_linear_jl.joblib')
# )




# convergent model
# A --> C; B --> C
# stanfile = os.path.join(here('projects/dcm/lcdm_py/ecpe_lcdm_bayes_net_convergent.stan'))
# stan_model = CmdStanModel(stan_file = stanfile, cpp_options={'STAN_THREADS': 'TRUE'})

# np.random.seed(81425)
# fit = stan_model.sample(data = stan_dict, show_console = True, chains = 4)

# with open(here('projects/dcm/lcdm_py/ecpe_lcdm_convergent.pkl'), 'wb') as f:
#   pickle.dump([stan_model, fit], f, protocol = -1)
  
# (
#   joblib.dump([stan_model, fit],
#               'projects/dcm/lcdm_py/ecpe_lcdm_convergent_jl_compress.joblib',
#               compress = 3)
# )

# (
#   joblib.dump([stan_model, fit],
#               'projects/dcm/lcdm_py/ecpe_lcdm_convergent_jl.joblib')
# )




# divergent model
# A --> B; A --> C
# stanfile = os.path.join(here('projects/dcm/lcdm_py/ecpe_lcdm_bayes_net_divergent.stan'))
# stan_model = CmdStanModel(stan_file = stanfile, cpp_options={'STAN_THREADS': 'TRUE'})

# np.random.seed(81425)
# fit = stan_model.sample(data = stan_dict, show_console = True, chains = 4)

# with open(here('projects/dcm/lcdm_py/ecpe_lcdm_divergent.pkl'), 'wb') as f:
#   pickle.dump([stan_model, fit], f, protocol = -1)
  
# (
#   joblib.dump([stan_model, fit],
#               'projects/dcm/lcdm_py/ecpe_lcdm_divergent_jl_compress.joblib',
#               compress = 3)
# )

# (
#   joblib.dump([stan_model, fit],
#               'projects/dcm/lcdm_py/ecpe_lcdm_divergent_jl.joblib')
# )