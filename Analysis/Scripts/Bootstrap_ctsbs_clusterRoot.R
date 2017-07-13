# ========================================
# Bootstrapping datasets and amp modelling 
# ========================================

# Author: Roxanne Beauclair

# Description: This script takes the imputed datasets and 
# bootstraps more datasets, then runs the amp models on them. 
# It is set up to be run from W.Delva's VSC supercomputer
# account.

# ====================
# Loading dependencies
# ====================
source("/user/data/gent/vsc400/vsc40070/CTSBS/00-Functions.R")


library(magrittr)
library(tibble)
library(tidyr)
library(dplyr)
library(purrr)
library(modelr)
library(broom)
library(forcats)
library(nlme)
library(lme4)

# =============
# Load datasets
# =============
load("/user/data/gent/vsc400/vsc40070/CTSBS/ctsbs_impute_data.rda")

# =========================
# Run bootstrap then models
# =========================

# 1. Create a dataset that nests all of the datasets according to
# gender, imputation, and hiv status.
# 2. Bootstrap each of the 50 datasets for each subgroup
# 3. Run regression models on all datasets (adjusting for race)
# 4. Extract model components
# 5. Output tidy df

set.seed(4387)
tidysumamp <- dfimp %>%
  filter(.imp == 1) %>%
  select(.imp, .id, id, sex, hiv, agep, age0, race) %>%
  mutate(id = as.factor(id)) %>% # Needed for bootstrapping
  group_by(.imp, sex, hiv) %>%
  nest() %>%
  mutate(boot = map(data, ~bootstrap_clus(., 2000))) %>%
  unnest(boot, .drop = TRUE) %>%
  mutate(model = map(strap, ampmodel), # ampmodel is function for lmer amp model
         modelsum = map(model, ~tidy(.x, effects = "fixed")), # Obtaining all the b's and intercepts from the models
         bvar = map(model, bvar), # Obtaining between subject variance using function bvar
         wvar = map(model, wvar)) %>% # Obtaining within subject variance using function wvar
  select(-strap, -model) # These objects contain full datasets, which make the file too big to download


# ===================
# Save tidy dataframe
# ===================
save(tidysumamp, file = "/user/data/gent/vsc400/vsc40070/CTSBS/ctsbs_boot_amp_data_1.rda")

