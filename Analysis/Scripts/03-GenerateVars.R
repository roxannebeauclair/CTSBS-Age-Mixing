# =================
# Variable generation
# =================

# Author: Roxanne Beauclair

# Description: Creates variables necessary for analyses

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


InstallLoad("magrittr", "tidyverse")

# =============
# Load datasets
# =============


# ===============
# Create new vars
# ===============

# =============
# Save datasets
# =============


# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
RemoveLibraries("magrittr", "tidyverse")
rm(list=ls())
