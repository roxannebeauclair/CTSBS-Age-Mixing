# =================
# Create Concurrency Metrics
# =================

# Author: Roxanne Beauclair

# Description: This script creates a variety of concurrency 
# indicators

# ===================
# Relative file paths
# ===================
wd <- getwd()
cdata <- paste0(wd, "/Data/Cleaned")

reshapedata <- paste0(cdata, "/ctsbs_reshape_data.rda")
concurdata <- paste0(cdata, "/ctsbs_concur_added_data.rda")

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
load(reshapedata)

# =========================================
# Create relationship concurrency indicator
# =========================================

# This is a conservative estimate. The date we use
# for the end of the relationship is assumed to be the last date of 
# the last period, even if they said the relationship was ongoing at
# the time of the survey

df1 <- df %>%
  arrange(id, relstart, relend) %>%
  group_by(id) %>%
  mutate(relseq = row_number(),
         relcount = n(),
         relconcur = "No") %>%
  ungroup()

for(i in 1:nrow(df1)) {
  
  if(df1$relend[i] >= df1$relstart[i + 1] &
     df1$id[i] == df1$id[i + 1] &
     !is.na(df1$relend[i]) &
     !is.na(df1$relstart[i + 1])) {
    
     df1$relconcur[i] <- "Yes"
    
  }
  
  if(i != 1 & df1$relseq[i] != 1) {
    
    for(j in 1:(df1$relseq[i] - 1)) {
      
       if(df1$relstart[i] <= df1$relend[i - j] &
         df1$id[i] == df1$id[i - j] &
         !is.na(df1$relend[i - j]) &
         !is.na(df1$relstart[i])) {
        
         df1$relconcur[i] <- "Yes"
         break
       }
    }
    
  }
  
}

# =============================
# Mean, Max duration of overlap
# =============================


# ================================
# Concurrency at partnership level
# ================================

df <- df1 %>%
  group_by(id) %>%
  mutate(relconcur = as.factor(relconcur),
         partconcur = as.factor(ifelse(any(relconcur == "Yes"), 
                                       "Yes", "No")),
         concurn = sum(relconcur == "Yes"))


# ==================
# Save relationship-level data
# ==================

save(df, file = concurdata)

# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
Vectorize(detach)(name = paste0("package:", c("tidyverse")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())
