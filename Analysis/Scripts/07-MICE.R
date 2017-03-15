# ====================
# Multiple Imputations
# ====================

# Author: Roxanne Beauclair

# Description: This script creates multiple imputations

# ===================
# Relative file paths
# ===================
wd <- getwd()
cdata <- paste0(wd, "/Data/Cleaned")

excludedata <- paste0(cdata, "/ctsbs_excluded_data.rda")

fxn <- paste0(wd, "/Scripts/00-Functions.R")

# ====================
# Loading dependencies
# ====================
source(fxn)

# magrittr: for use of %>% operator
# tidyverse: for data management functions
# mice: for MI


InstallLoad("magrittr", "tidyverse", "mice")

# =============
# Load datasets
# =============
load(excludedata)


