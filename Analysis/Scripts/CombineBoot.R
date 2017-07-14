# ========================================
# Combining bootstrapped datasets 
# ========================================

# Author: Roxanne Beauclair

# Description: This script takes all of the datasets
# produced by the bootstrapping and models done in
# the cluster, and combines them
library(magrittr)
library(tibble)
library(tidyr)
library(dplyr)

# ==========
# File paths
# ==========
wd <- "/user/data/gent/vsc400/vsc40070/CTSBS/"
bdata <- paste0(wd, "data/2000reps/")

bootdata <- paste0(bdata, "ctsbs_boot_rbind_data.rda")

# ==================================
# Load datasets and combine datasets
# ==================================

##initiate the big fat master dataset
ampdf <- tibble(.imp = factor(),
                            sex = factor(),
                            hiv = factor(),
                            .id = integer(),
                            modelsum = list(),
                            bvar = list(),
                            wvar = list()) 

for(i in 1:100){
  path <- bdata
  data.name <- paste0(path, "ctsbs_boot_amp_data_", i, ".rda")
  
  if(file.exists(data.name)) {
    
    load(data.name) # loading the tidysumamp object into the environment
    
    ampdf <- bind_rows(ampdf, tidysumamp)
    
  }
}

# ===========
# Save tibble
# ===========

save(ampdf, file = bootdata)
