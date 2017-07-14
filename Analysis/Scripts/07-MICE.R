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
imputemids <- paste0(cdata, "/ctsbs_impute_mids.rda")
imputedata <- paste0(cdata, "/ctsbs_impute_data.rda")
imputedatapart <- paste0(cdata, "/ctsbs_impute_data_part.rda")

fxn <- paste0(wd, "/Scripts/00-Functions.R")

# ====================
# Loading dependencies
# ====================
source(fxn)

# magrittr: for use of %>% operator
# tidyverse: for data management functions
# mice: for MI


InstallLoad("magrittr", "tidyverse", "mice",
            "CALIBERrfimpute", "lattice")

# =============
# Load datasets
# =============
load(excludedata)

# =======================================
# Select out only variables used for mice
# =======================================

dfmice <- df %>%
  mutate(start = as.factor(ifelse(!is.na(start), "Last year", "Before last year"))) %>%
  select(id, age, sex,  
         job, grade, hiv, relid, 
         relseq, relcount, pt, slept, 
         agep, live, otherpart, ongoing, 
         relconcur, relsf, relcf, reldurweeks,
         easy, private, truth, start) %>%
  mutate(missage = factor(ifelse(is.na(age), "Imputed", "Observed")),
         missagep = factor(ifelse(is.na(agep), "Imputed", "Observed")),
         misshiv = factor(ifelse(is.na(hiv), "Imputed", "Observed")),
         missrelcf = factor(ifelse(is.na(relcf), "Imputed", "Observed")),
         missrelconcur = factor(ifelse(is.na(relconcur), "Imputed", "Observed")),
         missrelsf = factor(ifelse(is.na(relsf), "Imputed", "Observed")),
         missreldur = factor(ifelse(is.na(reldurweeks), "Imputed", "Observed")))

# ===========================================
# Specify imputation method for each variable
# ===========================================

meth <- c("", "rfcont", "rfcat", 
          "rfcat", "rfcat", "rfcat", "",
          "", "", "", "rfcat",
          "rfcont", "rfcat", "rfcat", "rfcat",
          "rfcat", "rfcont", "polr", "rfcont",
          "polr", "polr", "polr", "",
          "", "", "", "",
          "", "", "")

# ====================
# Get predictor matrix
# ====================

imp <- mice(dfmice, maxit = 0)

pred <- quickpred(dfmice,
                  exclude = c("id", "relid", "relseq",
                              "missage", "missagep", "misshiv",
                              "missrelcf", "missrelconcur", "missrelsf",
                              "missreldur"),
                  include = c("age", "sex", "hiv",
                              "pt", "slept", "truth"),
                  mincor = 0.1,
                  minpuc = 0.1)

# ======
# Impute
# ======
start.time <- Sys.time()

imp <- mice(dfmice, 
            m = 100,
            method = meth,
            predictorMatrix = pred,
            seed = 40657,
            maxit = 10)

end.time <- Sys.time()


# ====================
# Save raw imputations
# ====================
save(imp, file = imputemids)

# =================
# Check imputations
# =================

# Check for convergence
# No SD's for easy or truth because there is only 1 missing value
plot(imp)
plot(imp, c("agep", "age", "hiv"))

# Check to see if observed values have similar distribution
# as imputed values
densityplot(imp)
stripplot(imp)

# ================================
# Convert mids object to dataframe
# ================================
dfimp <- complete(imp, action = "long", include = T)

# =================
# Variable creation
# =================

# Ensure all of participant-level variables are the same by id
# Will do this by using the first value by id for each imputed df
dfimp1cc <- dfimp %>%
  arrange(.imp, id) %>%
  filter(.imp == 0) # Keep complete cases aside

dfimp1mis <- dfimp %>%
  arrange(.imp, id) %>%
  group_by(.imp, id) %>%
  filter(.imp != 0) %>%
  mutate(age = first(age),
         sex = first(sex),
         job = first(job),
         grade = first(grade),
         hiv = first(hiv),
         easy = first(easy),
         private = first(private),
         truth = first(truth)) %>%
  ungroup()

dfimp1 <- bind_rows(dfimp1cc, dfimp1mis) %>%
  as.data.frame()
         
         
# Ensure all cont vars have plausible values 
dfimp2 <- dfimp1 %>%
  mutate(age = ifelse(age < 15, 15, 
                  ifelse(age > 69, 69, age)) %>%
           round(),
         agep = ifelse(agep < 11, 11, 
                       ifelse(agep > 95, 95, agep)) %>%
           round(),
         relsf = ifelse(relsf < 0, 0, relsf) %>%
           round(),
         reldurweeks = ifelse(reldurweeks < 1, 1,
                              ifelse(reldurweeks > 51, 51, reldurweeks)) %>%
           round())
           
# New measures for subsequent analyses
dfimp3 <- dfimp2 %>%
  mutate(agedif = ifelse(sex == "Male", age - agep, agep - age)) %>%
  group_by(.imp, id) %>%
  mutate(admean = round(mean(agedif, na.rm = T)),
         admax = max(agedif, na.rm = T),
         admin = min(agedif, na.rm = T),
         bridgewidth = admax - admin,
         partconcur = ifelse(any(relconcur == "Yes"), "Yes", "No"),
         partcf = ifelse(all(relcf == "Always"), "Always", 
                                ifelse(all(relcf == "Never"), "Never",
                                       "Inconsistent")),
         partsf = round(mean(relsf, na.rm = T)),
         partdurweeks = round(mean(reldurweeks, na.rm = T))) %>%
  ungroup() 

dfimp4 <- dfimp3 %>%
  group_by(.imp, sex) %>% #Want the mean and sd of ages by gender
  mutate(aveage = round(mean(age, na.rm = T)),
         sdage = round(sd(age, na.rm = T), 2)) %>%
  ungroup()

dfimp <- dfimp4 %>%
  mutate(partconcur = as.factor(partconcur),
         partcf = as.factor(partcf),
         age0 = age - 15, # Centered on youngest age
         agemean = age - aveage, # Centered age on mean
         age2sd = (age - aveage)/ (2 * sdage), # Centered age on mean, then scaled by 2SD
         agegroup = cut(age,
                        breaks = c(14, 24, 34, 44, 54, 70),
                        labels = c("15-24", "25-34", "35-44", "45-54", "55-70")),
         agepgroup = cut(agep, 
                         breaks = c(10, 24, 34, 44, 54, 100),
                         labels = c("<=24", "25-34", "35-44", "45-54", ">=55"))) %>%
  as.data.frame()

# Create separate dataset that only has one row per participant and imputation
partdfimp <- dfimp %>%
  distinct(.imp, id, .keep_all = T)

# =======================
# Save imputed dataframes
# =======================
save(dfimp, file = imputedata)
save(partdfimp, file = imputedatapart)

# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
Vectorize(detach)(name = paste0("package:", c("tidyverse",
                                              "CALIBERrfimpute",
                                              "mice",
                                              "lattice")), 
                  unload = TRUE, 
                  character.only = TRUE)

rm(list=ls())

