# REQUIREMENTS & OBJECTIVE

# DATA 
# - Macroeconomic indicators
# - time series by construct

# TIME SERIES ANALYSIS
# - Forecasting univariate : ARIMA, ETS, GARCH, PROPHET, ARIMAX,
#                            NNETAR, OLS, state space model
# - Multivariate: VAR, SVAR, State Space model

# OUTPUT
# - Rmarkdown report, rendered to pdf or html files

# FILE STRUCTURE 
# - data_fetch.R -> various functions to fetch series
# - utils.R -> Misc/Helper functions
# - univariate_analysis : - arima.R
#                         - ols.R
#                         - machine_learning.R
#                         - state_space.R
#                         - compare_methods.R -> select top 3 best models
# - multivariate_analysis : - var.R
#                           - svar.R
#                           - maybe more models
#                           - compare_methods.R -> select top 3 models
# - main.rmd 

# Data fetch section
# - BPS API 
# - BI API
# - fredr
# - WDI
# - world bank
# - quandl

library(tidyverse)
library(data.table)








