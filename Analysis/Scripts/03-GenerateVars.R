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

cleandata <- paste0(cdata, "/ctsbs_clean_data.rda")
newvarsdata <- paste0(cdata, "/ctsbs_newvars_data.rda")

fxn <- paste0(wd, "/Scripts/00-Functions.R")

# ====================
# Loading dependencies
# ====================
source(fxn)

# magrittr: for use of %>% operator
# tidyverse: for data management functions
# forcats: for categorical and string vars


InstallLoad("magrittr", "tidyverse", "forcats",
            "lubridate")

# =============
# Load datasets
# =============

load(cleandata)

# ===============
# Create new vars
# ===============

# New id and community vars
df1 <- df %>%
  mutate(id = id - 52230000000,
         community = cut(id, breaks = c(0, 5600000, 6000000, 7000000),
                         labels = c("Delft", "Wallacedene", "Khayelitsha"),
                         right = FALSE))


# Combine vars that are separate for each partner type
df2 <- df1 %>%
  rename(cfmp = cf) %>%
  mutate(sf = ifelse(pt == "MP", sfmp,
                     ifelse(pt == "OMP", sfomp, sfmp)),
         cf = factor(ifelse(pt == "MP", cfmp,
                     ifelse(pt == "OMP", cfomp, cfcp)),
                     levels = c(1, 2, 3, 4),
                     labels = c("Always", "Mostly", "Rarely", "Never"),
                     ordered = TRUE),
         agep = ifelse(pt == "MP", agemp,
                       ifelse(pt == "OMP", ageomp, agecp)),
         bornp = ifelse(pt == "MP", bornmp,
                        ifelse(pt == "OMP", bornomp, borncp)),
         live = factor(ifelse(pt == "OMP", liveomp,
                              ifelse(pt == "CP", livecp, NA)), 
                       levels = c(1, 2, 3, 4),
                       labels = c("Same household", "Dif household, same township",
                                 "Dif township, same province", "Dif province, but SA")),
         condlast = factor(ifelse(pt == "OMP", condlastomp,
                                  ifelse(pt == "CP", condlastcp, NA)),
                           levels = c(1, 2),
                           labels = c("Yes", "No")),
         drugs = factor(ifelse(pt == "OMP", drugsomp,
                               ifelse(pt == "CP", drugscp, NA)), 
                        levels = c(1, 2),
                        labels = c("Yes", "No")),
         alc = factor(ifelse(pt == "OMP", alcomp,
                             ifelse(pt == "CP", alccp, NA)),
                      levels = c(1, 2),
                      labels = c("Yes", "No")),
         youdrugs = factor(ifelse(pt == "OMP", youdrugsomp,
                                  ifelse(pt == "CP", youdrugscp, NA)),
                           levels = c(1, 2),
                           labels = c("Yes", "No")),
         youalc = factor(ifelse(pt == "OMP", youalcomp,
                                ifelse(pt == "CP", youalccp, NA)),
                         levels = c(1, 2),
                         labels = c("Yes", "No")),
         otherpart = factor(ifelse(pt == "OMP", otherpartomp,
                                   ifelse(pt == "CP", otherpartcp, NA)),
                            levels = c(1, 2),
                            labels = c("Yes", "No")),
         travel = factor(ifelse(pt == "OMP", travelomp,
                                ifelse(pt == "CP", travelomp, NA)),
                         levels = c(1, 2, 3, 4),
                         labels = c("morefourhours", "onehourorless", 
                                    "onetofourhours","lesshalfhour" )),
         ongoing = factor(ifelse(pt == "MP", ongoingmp,
                                 ifelse(pt == "OMP", ongoingomp, ongoingcp)),
                          levels = c(1, 2),
                          labels = c("Yes", "No")),
         start = ifelse(pt == "CP", startdatecp, 
                        ifelse(pt == "OMP", startdateomp, NA))) %>%
  select(-agemp, -ageomp, -agecp, -cfmp, -cfomp, -cfcp, -sfmp, -sfomp, -sfcp,
         -bornmp, - bornomp, -borncp, -condlastcp, -condlastomp, -drugscp, 
         -drugsomp, -alccp, -alcomp, -youdrugscp, -youdrugsomp, -youalccp,
         -youalcomp, -otherpartcp, -otherpartomp, -ongoingmp, -ongoingcp,
         -ongoingomp, -travelcp, -travelomp, -livecp, -liveomp, -startdatecp,
         -startdateomp)


# Where partner is missing, add zero
df3 <- df2 %>%
  mutate(partner = ifelse(is.na(partner), 0, partner))

# Create variables for point prevalence dates
df4 <- df3 %>%
  mutate(acasidate12mb = acasidate - 365,
         acasidate9mb = acasidate - 274,
         acasidate6mb = acasidate - 183,
         acasidate3mb = acasidate - 91)

# Unique relid, start and end dates for relationship
df5 <- df4 %>%
  mutate(relid = as.numeric(paste0(id, partner))) %>%
  group_by(relid) %>%
  mutate(relstart = min(perstart),
         relend = max(perend),
         reldur = (relend - relstart) + 1) %>%
  ungroup() %>%
  mutate(reldurweeks = ifelse(reldur < 7, 1, round(reldur / 7)))

# Rel characteristics
df6 <- df5 %>%
  group_by(relid) %>%
  mutate(relsf = mean(sf),
         relcf = as.factor(ifelse(all(cf == "Always"), "Always", 
                                  ifelse(all(cf == "Never"), "Never",
                                         "Inconsistent")))) %>%
  ungroup()

# Create age and partner age variables
df7 <- df6 %>%
  mutate(acasiyear = year(acasidate),
         age = ifelse(is.na(age), acasiyear - born, age),
         age = ifelse(age > 70 | age < 15, NA, age),
         agep = ifelse(is.na(agep), acasiyear - bornp, agep),
         agecat = cut(age, breaks = c(0, 19, 24, 29, 34, 39, 44, 49, 54, 59,
                                      64, 70)),
         agedif = ifelse(sex == "Male", age - agep, agep - age),
         agedifcat = cut(agedif, breaks = c(-71, -6, -1, 5, 10, 15, 65)))
                     
# Create part level agedif vars
df8 <- df7 %>%
  group_by(id) %>%
  mutate(minagedif = min(agedif, na.rm = T),
         maxagedif = max(agedif, na.rm = T)) %>%
  ungroup() %>%
  mutate(bw = maxagedif - minagedif)

# Combine other categories 
# Even if the person identifies as "other" gender, they would be percieved by others
# as male or female, or at least have the associated biological risk of one or the other
# So I feel ok imputing what that gender would be
df <- df8 %>%
  mutate(hiv = factor(ifelse(hiv == "Not done", NA, hiv),
                      levels = c(2, 3),
                      labels = c("Negative", "Positive")),
         sex = factor(ifelse(sex == "Other", NA, sex),
                      levels = c(1, 2),
                      labels = c("Male", "Female")),
         grade = fct_collapse(grade,
                              Primary = c("grade0", "grade1", "grade2",
                                          "grade3", "grade4", "grade5",
                                          "grade6", "grade7"),
                              Secondary = c("grade8", "grade9", "grade10",
                                            "grade11", "grade12"),
                              Teriary = "tertiary"))

# =============
# Save datasets
# =============

save(df, file = newvarsdata)

# ====================================================
# Detach libraries and remove objects from environment
# ====================================================

Vectorize(detach)(name = paste0("package:", c("tidyverse", "forcats", "lubridate")), 
                  unload = TRUE, 
                  character.only = TRUE)
rm(list=ls())
