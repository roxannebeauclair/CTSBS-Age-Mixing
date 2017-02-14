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


InstallLoad("magrittr", "tidyverse", "forcats")

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
                          labels = c("Yes", "No"))) %>%
  select(-agemp, -ageomp, -agecp, -cfmp, -cfomp, -cfcp, -sfmp, -sfomp, -sfcp,
         -bornmp, - bornomp, -borncp, -condlastcp, -condlastomp, -drugscp, 
         -drugsomp, -alccp, -alcomp, -youdrugscp, -youdrugsomp, -youalccp,
         -youalcomp, -otherpartcp, -otherpartomp, -ongoingmp, -ongoingcp,
         -ongoingomp, -travelcp, -travelomp, -livecp, -liveomp)


# Where partner is missing, add zero

# =============
# Save datasets
# =============


# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
RemoveLibraries("magrittr", "tidyverse")
rm(list=ls())
