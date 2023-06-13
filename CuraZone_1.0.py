# %% [markdown]
# # Import libraries

# %%
import pandas as pd
import numpy as np
import warnings
warnings.filterwarnings("ignore")

from sklearn.metrics import make_scorer
from sklearn.model_selection import RepeatedKFold
from sklearn.preprocessing import StandardScaler
from sklearn.pipeline import Pipeline
from sklearn.linear_model import ElasticNet
from skopt import BayesSearchCV

# %% [markdown]
# # Import data

# %%
data = pd.read_csv("covid_data_196.csv")

# %%
# Set variables
xvar = ["Employed", "Males", "Vulnerable_pop", "Health_Insurance", 
        "Secondary_Education", "Life_Expectancy", "prev_cronic_prov", 
        "Indice_Pobreza_Compuesto", "White_1000_Inhab", "Assian_1000_Inhab", 
        "Black_1000_Inhab", "prev_diferencial_prov_endes", "prev_hipertension_prov_endes", 
        "prev_diabetes_prov_endes", "prev_obesidad_prov_endes", "Days_Till_Attended", 
        "SD_Days_Till_Attended",  "Travel_Time_toHFacility_Hours", "SD_TTtHFH",
        "Waiting_Time_4Attention_Hours", "SD_WT4AH", "logPD_1000", "Overcrowding",
        "Natural_Region1", "Natural_Region2", "Natural_Region3"]

datac = data.dropna()

x = datac[xvar]
Y = datac["logmuertes1000"]

# %%
# Display dataset
datac

# %% [markdown]
# # Setting parameters

# %%
# Set the cross-validation

# Define cross-validation method
cv = RepeatedKFold(n_splits=4, n_repeats=5,random_state=0)

# %%
# Set the hyperparameters grid

# Define grid
param_grid = {
    'enr__alpha': (1e-2, 100.0, 'log-uniform'),
    'enr__l1_ratio': (0.1, 1.0, 'uniform')
}

# %% [markdown]
# # Model training

# %%
# Define evaluation and post-processing criteria
from sklearn.metrics import mean_squared_error

scoring = make_scorer(mean_squared_error, greater_is_better=False)

# Define the model
pipe = Pipeline([('scaler', StandardScaler()), ('enr', ElasticNet())])


# Define search
search_ddnn = BayesSearchCV(estimator = pipe, search_spaces=param_grid, scoring=scoring,
                           cv=cv, n_jobs=-1, verbose = 4, n_iter=50, n_points=5)

# Perform the search
results_ENLR = search_ddnn.fit(x, Y)

# %%
# Display search results
pd.DataFrame(results_ENLR.cv_results_)

# %% [markdown]
# # Results Visualization

# %%
# Print MSE and Settings for optimal model
print('MSE: %.3f' % results_ENLR.best_score_)
print('Config: %s' % results_ENLR.best_params_)

# %%
results_ENLR.best_estimator_

# %%
Results_LR = pd.DataFrame(list(zip(x.columns.values,results_ENLR.best_estimator_['enr'].coef_)), columns =["Variable", "Coefficient"])
Results_LR

# %%
# Visualization of most important features

Results_LR.loc[Results_LR.Coefficient != 0]

# %% [markdown]
# # Predict a real case

# %%
# Enter data for any province:

x.iloc[0:1,:] # The first province

# %%
# Print the prediction
n = 98
pred = results_ENLR.best_estimator_.predict(x.iloc[n:(n+1),:]) # Row n point prediction

print("This province would have:", np.round(np.exp(pred),1).item(), "Deaths per 1000 inhabitants")

# %%
# Input data in matrix format
x_test = [[4.2101755e+02, 4.9185672e+02, 1.9342053e+02, 8.3711670e+02,
        1.4840000e+04, 6.3489361e+01, 4.8455893e+02, 4.7172291e+01,
        4.1815300e+01, 9.0080351e-02, 2.4213598e+01, 4.1135788e+02,
        1.0121213e+02, 2.2985615e+01, 1.4377316e+02, 3.3102899e+00,
        9.4108963e+00, 4.0912408e-01, 8.2797784e-01, 1.2161849e-01,
        4.9298859e-01, 6.1477417e-01, 1.1253831e+01, 0.0000000e+00,
        1.0000000e+00, 0.0000000e+00]]

# %%
pred = results_ENLR.best_estimator_.predict(x_test) # Row n point prediction

print("This province would have:", np.round(np.exp(pred),1).item(), "Deaths per 1000 inhabitants")


