# ========================================
# Bootstrapping datasets and amp modelling 
# ========================================

# Author: Roxanne Beauclair

# Description: This script takes the imputed datasets and 
# bootstraps more datasets, then runs the amp models on them

# ====================
# Loading dependencies
# ====================
source(fxn)


InstallLoad("magrittr", "MASS", "tidyverse", 
            "mice","nlme", "lme4", "modelr",
            "mgcv", "broom", "splines")

# =============
# Load datasets
# =============
load(imputedata)

# =========================
# Run bootstrap then models
# =========================

# 1. Create a dataset that nests all of the datasets according to
# gender, imputation, and hiv status.
# 2. Bootstrap each of the 50 datasets for each subgroup
# 3. Run regression models on all datasets
# 4. Extract model components
# 5. Output tidy df

set.seed(4387)
tidysumamp <- dfimp %>%
  filter(.imp != 0)%>%
  select(.imp, .id, id, sex, hiv, agep, age0) %>%
  mutate(id = as.factor(id)) %>%
  group_by(.imp, sex, hiv) %>%
  nest() %>%
  mutate(boot = map(data, ~bootstrap_clus(., 10))) %>%
  unnest(boot, .drop = TRUE) %>%
  mutate(model = pmap(list(strap, as.character(sex), as.character(hiv)), # pmap changes sex and hiv to numeric, so must force character
                      ampmodel), #ampmodel is function for nlme amp model
         modelsum = map(model, ~tidy(.x, effects = "fixed")), # Obtaining all the b's and intercepts from the models
         bvar = map(model, bvar), # Obtaining between subject variance using function bvar
         wvar = map(model, wvar)) %>% # Obtaining within subject variance using function wvar
  select(-strap)




# ===================
# Save tidy dataframe
# ===================
save(tidysumamp, file = )

