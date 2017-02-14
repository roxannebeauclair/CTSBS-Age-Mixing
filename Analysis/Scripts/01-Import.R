# =================
# Importing dataset
# =================

# Author: Roxanne Beauclair

# Description: This script imports the raw dataset from the  
# CTSBS study and the HIV results from ZAMSTAR,
# merges them together and saves it as a dataframe that can be loaded 
# in other scripts. It also runs queries on the df at the end
# to make sure there are no duplicates or clerical errors

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
# Drop completely duplicated rows
hivdf <- hivdf %>%
  mutate(IndBarcode = as.numeric(IndBarcode)) %>%
  distinct(.keep_all = TRUE)


# =======================
# Merge datasets together
# =======================
df <- left_join(sexdf, hivdf, by = c("BARCODE" = "IndBarcode"))

# =======
# QUERIES
# =======

# Get rid of all completely duplicated rows
df <- df %>%
  distinct(.keep_all = TRUE)

# There shouldn't be rows with the same barcode, partner number,
# partner type and period number

df <- df %>%
  distinct(Partner_Number, Partner_Type, BARCODE, Period_Number,
           .keep_all = TRUE)

# =========
# Save data
# =========

save(df, file = data)

# ================
# Remove libraries 
# ================
# RemoveLibraries("magrittr", "tidyverse", "haven")
Vectorize(detach)(name = paste0("package:", c("tidyverse", "haven", "magrittr")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())
