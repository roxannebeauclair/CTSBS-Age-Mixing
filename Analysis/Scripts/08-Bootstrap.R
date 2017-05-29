# ========================================
# Bootstrapping datasets and amp modelling 
# ========================================

# Author: Roxanne Beauclair

# Description: This script takes the imputed datasets and 
# bootstraps more datasets, then runs the amp models on them

wd <- "/Users/roxannebeauclair/Documents/Analytical Projects/PhD/CTSbS Age Mixing/Analysis"
cdata <- paste0(wd, "/Data/Cleaned")

imputedata <- paste0(cdata, "/ctsbs_impute_data.rda")
bootdata <- paste0(cdata, "/ctsbs_boot_amp_data.rda")

fxn <- paste0(wd, "/Scripts/00-Functions.R")

# ====================
# Loading dependencies
# ====================
source(fxn)


InstallLoad("magrittr", "tidyverse", 
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

start.time <- Sys.time()

tidysumamp <- dfimp %>%
  filter(.imp != 0)%>%
  select(.imp, .id, id, sex, hiv, agep, age0) %>%
  group_by(.imp, sex, hiv) %>%
  nest() %>%
  mutate(boot = map(data, ~modelr::bootstrap(., 10))) %>%
  unnest(boot, .drop = TRUE) %>%
  mutate(model = pmap(list(strap, as.character(sex), as.character(hiv)), # pmap changes sex and hiv to numeric, so must force character
                      ampmodel), #ampmodel is function for nlme amp model
         modelsum = map(model, ~tidy(.x, effects = "fixed")), # Obtaining all the b's and intercepts from the models
         bvar = map(model, bvar), # Obtaining between subject variance using function bvar
         wvar = map(model, wvar), # Obtaining within subject variance using function wvar
         power = map(model, power))

stop.time <- Sys.time()
time <- stop.time - start.time
time

# ===================
# Save tidy dataframe
# ===================
save(tidysumamp, file = bootdata)

# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
Vectorize(detach)(name = paste0("package:", c("tidyverse", "mice",
                                              "mgcv", "nlme", 
                                              "lme4", "modelr",
                                              "broom", "splines")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())
