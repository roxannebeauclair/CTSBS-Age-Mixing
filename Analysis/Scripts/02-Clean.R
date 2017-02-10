# =================
# Cleaning datasets
# =================

# Author: Roxanne Beauclair

# Description: This script cleans the raw dataset,
# removes excess variables that won't be useful for the analysis,
# and saves cleaned datasets for subsequent analyses

# ===================
# Relative file paths
# ===================
wd <- getwd()
rdata <- paste0(wd, "/Data/Raw") 
cdata <- paste0(wd, "/Data/Cleaned")

rdf <- paste0(rdata, "")
cdf <- paste0(cdata, "")

fxn <- paste0(wd, "Scripts/00-Functions.R")

# ====================
# Loading dependencies
# ====================
source(fxn)

# magrittr: for use of %>% operator
# tidyverse: for data management functions
# gdata for unknownToNA()

InstallLoad("magrittr", "tidyverse", "gdata")

# =============
# Load datasets
# =============

# ====================================
# Select variables needed for analysis
# Rename them
# ====================================

# ========================
# Recode values to missing
# ========================


# ===========================================================
# Dropping unused levels from factor variables in data frames
# ===========================================================


# ==========
# Label vars
# ==========



# =====================
# Other transformations
# =====================

# =============
# Save datasets
# =============

# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
RemoveLibraries("magrittr", "tidyverse", "gdata")
rm(list=ls())
