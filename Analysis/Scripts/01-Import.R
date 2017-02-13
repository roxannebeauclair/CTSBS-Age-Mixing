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

sexdata <- paste0(rdata, "/February (Week 5) Survey_Data.csv")
hivdata <- paste0(rdata, "/RESULTS_20120316 2.dta")
data <- paste0(rdata, "/ctsbs_raw_data.rda")

fxn <- paste0(wd, "/Scripts/00-Functions.R")

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

sexdf <- read_delim(sexdata, delim = ";")
hivdf <- read_dta(hivdata)

# ============================
# Prepare datasets for merging
# ============================

# Make sure id's are same type
# Drop duplicates
hivdf <- hivdf %>%
  mutate(IndBarcode = as.numeric(IndBarcode)) %>%
  distinct(.keep_all = TRUE)


# =======================
# Merge datasets together
# =======================
df <- left_join(sexdf, hivdf, by = c("BARCODE" = "IndBarcode"))

# =========
# Save data
# =========

save(df, file = data)

# ================
# Remove libraries 
# ================
# RemoveLibraries("magrittr", "tidyverse", "haven")
Vectorize(detach)(name = paste0("package:", c("magrittr", "tidyverse", "haven")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())
