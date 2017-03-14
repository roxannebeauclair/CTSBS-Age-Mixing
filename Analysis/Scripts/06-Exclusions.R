# =================
# Exclusions
# =================

# Author: Roxanne Beauclair

# Description: This script excludes participants who will not be included in the analysis

# ===================
# Relative file paths
# ===================
wd <- getwd()
cdata <- paste0(wd, "/Data/Cleaned")

concurdata <- paste0(cdata, "/ctsbs_concur_added_data.rda")
excludedata <- paste0(cdata, "/ctsbs_excluded_data.rda")

fxn <- paste0(wd, "/Scripts/00-Functions.R")

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
load(concurdata)

# =====================
# Exclude participants
# =====================

# Age: missing, less than 15, older than 70
# Gender: missing or other
# Sexorientation: homosexual or other

# Race: not black or coloured
df1 <- df %>%
  filter(race == "Coloured" | race == "Black")

# Partner: no reported partner in previous year 
df <- df1 %>%
  filter(partner != 0)

# Sex: no sex with partner in the past year
# Episodes in the previous 12 months

# ============================
# Drop non-essential variables
# =============================


# ==================
# Save datasets
# ==================
save(df, file = excludedata)

# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
Vectorize(detach)(name = paste0("package:", c("tidyverse")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())
