# ========================================
# Combining bootstrapped datasets 
# ========================================

# Author: Roxanne Beauclair

# Description: This script takes all of the datasets
# produced by the bootstrapping and models done in
# the cluster, and combines them

library(tidyverse)

# ==========
# File paths
# ==========
wd <- "/Users/roxannebeauclair/Documents/Analytical Projects/PhD/CTSbS Age Mixing/Analysis"
cdata <- paste0(wd, "/Data/Cleaned")
bdata <- paste0(cdata, "/Bootstrapped/")

bootdata <- paste0(cdata, "/ctsbs_boot_test_data.rda")

# ==================================
# Load datasets and combine datasets
# ==================================
ampdf <- tibble(.imp = factor(),
                            sex = factor(),
                            hiv = factor(),
                            .id = integer(),
                            model = list(),
                            modelsum = list(),
                            bvar = list(),
                            wvar = list()) ##initiate the big fat master dataset

for(i in 1:50){
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
