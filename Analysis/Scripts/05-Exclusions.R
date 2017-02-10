# =================
# Exclusions
# =================

# Author: Roxanne Beauclair

# Description: This script excludes participants who will not be included in the analysis

# ===================
# Relative file paths
# ===================

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

# ==============================================
# Exclude 
# ==============================================

# ==================
# Save datasets
# ==================


# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
RemoveLibraries("magrittr", "tidyverse")
rm(list=ls())
