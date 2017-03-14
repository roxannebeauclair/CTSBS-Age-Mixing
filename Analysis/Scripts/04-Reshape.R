# =================
# Reshaping datasets
# =================

# Author: Roxanne Beauclair

# Description: This script creates a relationship-level dataset 
# from episode level data

# ===================
# Relative file paths
# ===================
wd <- getwd()
cdata <- paste0(wd, "/Data/Cleaned")

newvarsdata <- paste0(cdata, "/ctsbs_newvars_data.rda")
reshapedata <- paste0(cdata, "/ctsbs_reshape_data.rda")

fxn <- paste0(wd, "/Scripts/00-Functions.R")
# ====================
# Loading dependencies
# ====================
source(fxn)

# magrittr: for use of %>% operator
# tidyverse: for data management functions

InstallLoad("magrittr", "tidyverse", "lubridate")

# =============
# Load datasets
# =============
load(newvarsdata)
  
# ===============
# Reshape to relationships
# ===============
df1 <- df %>%
  select(-period:-perend, -sf, -cf) %>%
  distinct(id, partner, .keep_all = T) %>%
  mutate(start = as_date(start)) %>%
  select(id:startmp, start, relstart, relend, everything())

df <- df1
  
# ==================
# Save relationship-level data
# ==================

save(df, file = reshapedata)

# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
Vectorize(detach)(name = paste0("package:", c("tidyverse")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())
