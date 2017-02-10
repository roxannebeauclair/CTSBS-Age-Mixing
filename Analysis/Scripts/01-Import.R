# =================
# Importing dataset
# =================

# Author: Roxanne Beauclair

# Description: This script imports the raw dataset from the  
# CTSBS study and the HIV results from ZAMSTAR,
# merges them together and saves it as a dataframe that can be loaded 
# in other scripts

# ===================
# Relative file paths
# ===================

wd <- getwd()
rdata <- paste0(wd, "/Data/Raw") 
cdata <- paste0(wd, "/Data/Cleaned")

fxn <- paste0(wd, "Scripts/00-Functions.R")

# ====================
# Loading dependencies
# ====================
source(fxn)

# magrittr: for use of %>% operator
# tidyverse: for data management functions
# haven: used to import stata formatted datasets

InstallLoad("magrittr", "tidyverse", "haven")

# ===========
# Import data
# ===========

# ============================
# Prepare datasets for merging
# ============================

# First, add leading zeroes to pad other id vars
# ID created from EA, HH id, and core respondent id
# Part 1 respondent ID created from EA, HH, and resp id
# Delete extraneous id vars
# Take out non-core resp from s1



# =======================
# Merge datasets together
# =======================
 

# =========
# Save data
# =========

# ================
# Remove libraries 
# ================
RemoveLibraries("magrittr", "tidyverse", "haven")
rm(list=ls())
