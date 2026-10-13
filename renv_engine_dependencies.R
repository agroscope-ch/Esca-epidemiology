# Packages that are only used indirectly (as parsnip/tidymodels engines, referenced
# by name in esca_ml_config.R). renv cannot detect these from the code, so they are
# declared here to make sure they end up in renv.lock. This file is never run.
library(ranger)   # engine = "ranger"  (Random Forest)
library(glmnet)   # engine = "glmnet"  (LASSO)
