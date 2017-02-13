# =================
# Cleaning datasets
# =================

# Author: Roxanne Beauclair

# Description: This script cleans the raw dataset,
# removes excess variables that won't be useful for the analysis,
# and saves cleaned datasets for subsequent analyses

# ===================
# Relative file paths
# ===================
wd <- getwd()
rdata <- paste0(wd, "/Data/Raw") 
cdata <- paste0(wd, "/Data/Cleaned")

data <- paste0(rdata, "/ctsbs_raw_data.rda")
cleandata <- paste0(cdata, "/ctsbs_clean_data.rda")


fxn <- paste0(wd, "/Scripts/00-Functions.R")

# ====================
# Loading dependencies
# ====================
source(fxn)

# magrittr: for use of %>% operator
# tidyverse: for data management functions
# gdata: for unknownToNA()
# forcats: fct_ variables
# lubridate: formatting dates

InstallLoad("magrittr", "tidyverse", "gdata", "forcats",
            "lubridate")

# =============
# Load datasets
# =============

load(data)

# ====================================
# Select variables needed for analysis
# Rename them
# ====================================

df1 <- df %>%
  select(partner = Partner_Number, pt = Partner_Type, period = Period_Number,
         startend = StartEndDate, age = AGE, agemp = AGEPARTNER,
         id = BARCODE, cplastyear = CASUALPARTNERLASTYEAR, cf = CONDOMSFREQUENCE,
         earn = EARN, race = ETHNIC, sex = GENDER, interview = INTERVIEWAGAIN,
         job = JOB, lang = LANG, partmp = MAINPARTNER, ongoingmp = MAINPARTNERSTILL,
         agecp = MAKWAPHENIAGE, condlastcp = MAKWAPHENICONDOMLASTTIME,
         cfcp = MAKWAPHENICONDOMS, countcp = MAKWAPHENICOUNT, livecp = MAKWAPHENILIVE,
         otherpartcp = MAKWAPHENIOTHERPARTNERS, drugscp = MAKWAPHENISHEDRUGS,
         alccp = MAKWAPHENISHEDRUNK, ongoingcp = MAKWAPHENISTILLONGOING,
         sfcp = MAKWAPHENITIMESSEX, travelcp = MAKWAPHENITIMETRAVEL,
         borncp = MAKWAPHENIYEARBORN, youalccp = MAKWAPHENIYOUDRUNK,
         youdrugscp = MAKWAPHENIYOUDRUGS, partomp = OTHERMAINPARTNER,
         partsinceomp = OtherPartner, ageomp = OTHERPARTNERAGE,
         condlastomp = OTHERPARTNERCONDOMLASTTIME, cfomp = OTHERPARTNERCONDOMS,
         countomp = OTHERPARTNERCOUNT, liveomp = OTHERPARTNERLIVE,
         otherpartomp = OTHERPARTNEROTHERPARTNERS, drugsomp = OTHERPARTNERSHEDRUGS,
         alcomp = OTHERPARTNERSHEDRUNK, ongoingomp = OTHERPARTNERSTILLONGOING,
         travelomp = OTHERPARTNERTIMETRAVEL, bornomp = OTHERPARTNERYEARBORN,
         youdrugsomp = OTHERPARTNERYOUDRUGS, youalcomp = OTHERPARTNERYOUDRUNK,
         sfomp = OTHERSEXFREQUENCE, totalpartners = PARTNERSLIVETIME,
         periodmp = MAINPARTNERPERIOD, periodomp = OTHERPARTNERPERIOD,
         periodcp = MAKWAPHENIPERIOD, relig = RELIGION,
         school = SCHOOL, grade = SCHOOLGRADE, sfmp = SEXFREQUENCE,
         sexorientation = SEXORIENTATION, slept = SLEPT, 
         start = STARTTIME, easy = TOUCHSCREENEASY, private = TOUCHSCREENPRIVATE,
         truth = TRUTHFUL, born = YEARBORN, bornmp = YEARBORNPARTNER,
         hiv = Q66_HIV_DET, confirm = Q67_HIV_UNI, wantknow = Q68_KHS,
         resultgiven = Q69_GHB)
         

# ========================
# Recode values to missing
# ========================

df2 <- df1 %>%
  unknownToNA(unknown = "NULL") %>%
  unknownToNA(unknown = -999)

# ==========
# Label vars
# ==========
yn = c("Yes", "No")

df3 <- df2 %>%
  mutate(pt = factor(pt),
         cplastyear = factor(cplastyear, levels = c(0, 1),
                             labels = yn),
         cf = factor(cf, levels = c(0, 1, 2, 3),
                     labels = c("Always", "Mostly", "Rarely", "Never"),
                     ordered = TRUE),
         earn = factor(earn, levels = c(0, 1, 2, 3, 4),
                       labels = c("<= R500", "R500 - R2000",
                                  "R2001 - R4000", "R4001 - R6000",
                                  ">R6000"),
                       ordered = TRUE),
         race = factor(race, levels = c(0, 1, 2, 3, 4),
                       labels = c("White", "Coloured", "Black",
                                  "Indian", "No disclosure")),
         sex = factor(sex, levels = c(0, 1, 2),
                      labels = c("Male", "Female", "Other")),
         interview = factor(interview, levels = c(0, 1, 2, 3),
                            labels = c("ACASI", "Pen and Paper", "Reseearcher",
                                       "Phone")),
         job = factor(job, levels = c(0, 1),
                      labels = yn),
         lang = factor(lang, levels = c(0, 1, 2),
                       labels = c("Xhosa", "English", "Afrikaans")),
         partmp = factor(partmp, levels = c(0, 1),
                         labels = yn),
         ongoingmp = factor(ongoingmp, levels = c(0, 1),
                            labels = yn),
         condlastcp = factor(condlastcp, levels = c(0,1),
                             labels = yn),
         cfcp = factor(cfcp, levels = c(0, 1, 2, 3),
                       labels = c("Always", "Mostly", "Rarely", "Never"),
                       ordered = TRUE),
         livecp = factor(livecp, levels = c(0, 1, 2, 3),
                         labels = c("Same household", "Dif household, same township",
                                    "Dif township, same province", "Dif province, but SA")),
         otherpartcp = factor(otherpartcp, levels = c(0, 1),
                              labels = yn),
         drugscp = factor(drugscp, levels = c(0, 1),
                          labels = yn),
         alccp = factor(alccp, levels = c(0, 1),
                        labels = yn),
         ongoingcp = factor(ongoingcp, levels = c(0, 1),
                            labels = yn),
         travelcp = factor(travelcp),
         youalccp = factor(youalccp, levels = c(0, 1),
                           labels = yn),
         youdrugscp = factor(youdrugscp, levels = c(0, 1),
                             labels = yn),
         partomp = factor(partomp, levels = c(0, 1),
                          labels = yn),
         partsinceomp = factor(partsinceomp, levels = c(0, 1),
                               labels = yn),
         condlastomp = factor(condlastomp, level = c(0, 1),
                              labels = yn), 
         cfomp = factor(cfomp, level = c(0, 1, 2, 3),
                        labels = c("Always", "Mostly", "Rarely", "Never"),
                        ordered = TRUE),
         liveomp = factor(liveomp, levels = c(0, 1, 2, 3),
                          labels = c("Same household", "Dif household, same township",
                                     "Dif township, same province", "Dif province, but SA")),
         otherpartomp = factor(otherpartomp, levels = c(0, 1),
                               labels = yn),
         drugsomp = factor(drugsomp, levels = c(0, 1),
                           labels = yn),
         alcomp = factor(alcomp, levels = c(0, 1),
                         labels = yn),
         ongoingomp = factor(ongoingomp, levels = c(0, 1),
                             labels = yn),
         travelomp = factor(travelomp),
         youdrugsomp = factor(youdrugsomp, levels = c(0, 1),
                              labels = yn),
         youalcomp = factor(youalcomp, levels = c(0, 1),
                            labels = yn),
         relig = factor(relig, levels = c(0, 1, 2, 3),
                        labels = c("Christian", "Muslim",
                                   "Not religious", "Other")),
         school = factor(school, levels = c(0, 1),
                         labels = yn),
         grade = factor(grade),
         sexorientation = factor(sexorientation, levels = c(0, 1, 2),
                                 labels = c("Men", "Women", "Both")),
         slept = factor(slept, levels = c(0, 1),
                        labels = yn),
         easy = factor(easy, levels = c(0, 1, 2, 3),
                       labels = c("Very easy", "Somewhat easy", 
                                  "Somewhat difficult", "Very difficult"), 
                       ordered = TRUE),
         private = factor(private, levels = c(0, 1, 2),
                          labels = c("Very private", "Somewhat private",
                                     "No privacy"),
                          ordered = TRUE),
         truth = factor(truth, levels = c(0, 1, 2, 3, 4),
                        labels = c("All truthfully", "Most truthfully", 
                                   "Some truthfully", "Most dishonestly",
                                   "All dishonestly"),
                        ordered = TRUE),
         hiv = factor(hiv, levels = c(-5, 0, 1),
                      labels = c("Not done", "Negative", "Positive")),
         confirm = factor(confirm, levels = c(-5, 0, 1),
                          labels = c("Not done", "Negative", "Positive")),
         wantknow = factor(wantknow, levels = c(0, 1),
                           labels = c("No", "Yes")),
         resultgiven = factor(resultgiven, levels = c(0, 1),
                               labels = c("No", "Yes")),
         totalpartners = factor(totalpartners))


# =======================================
# Combining categories for some variables
# =======================================


df4 <- df3 %>%
  mutate(travelcp = fct_collapse(travelcp,
                                 lesshalfhour = c("15 Minutes", "15 Minute", "15 Imizuzu",
                                              "30 Minutes", "30 Minute", "30 Imizuzu",
                                              "15 I", "15 M", "30 I", "30 M"),
                                 onehourorless = c("45 Minutes", "45 Minute", "45 Imizuzu",
                                                   "45 I", "45 M",
                                                   "1 Hour", "1 Uur", "1 Iyure", "1 Iy", "1 Uu"),
                                 onetofourhours = c("1:15", "1:15 Hour","1:15 Iyure",
                                              "1:30", "1:30 Hour", "1:30 Uur", "1:30 Iyure",
                                              "1:45", "1:45 Iyure",
                                              "2 Hours", "2 Uurs", "2 Iyures",
                                              "2:30", "2:30 Uurs", "2:30 Iyures",
                                              "2 Iy", "2 Uu", "3 Iyures", "3 Iy",
                                              "3 Uu", "4 Iyures", "4 Ho"),
                                 morefourhours = c("5 Iy", "5 Uu", "5 Hours", "5 Uurs", "5 Iyures",
                                             "6 Iyures", "7 Iyures", "8 Hours", "8 Iyures",
                                             "9 Iyures", "10 Hours","10 Iyures",
                                             "11 Iyures", "12 Iyures", "+")),
         travelomp = fct_collapse(travelomp,
                                  lesshalfhour = c("15 Minutes", "15 Minute", "15 Imizuzu",
                                                   "30 Minutes", "30 Minute", "30 Imizuzu",
                                                   "15 I", "15 M", "30 I"),
                                  onehourorless = c("45 Minutes", "45 Imizuzu",
                                                    "1 Hour", "1 Iyure", "1 Iy", "1 Uu"),
                                  onetofourhours = c("1:15", "1:15 Iyure",
                                                     "1:30", "1:30 Hour",  "1:30 Iyure",
                                                     "1:45 Iyure", 
                                                     "2 Hours", "2 Uurs", "2 Iyures",
                                                     "2 Ho", "2:30", "2:30 Iyures",
                                                     "2 Iy", "2 Uu", "3 Iyures",
                                                     "3 Uu", "3 Hours", "4 Iy", "4 Uu"),
                                  morefourhours = c("5 Iy", "5 Uu","5 Iyures",
                                                    "6 Iyures", "7 Iyures", "7 Iy", "8 Iyures",
                                                    "8 Iy", "9 Iy", "9 Uu", "9 Uurs", "9 Iyures",
                                                    "10 Iyures", "11 H", "11 Hours",
                                                    "11 Iyures", "12 Iyures", "12 Hours", "12 I",
                                                    "+")),
         grade = fct_collapse(grade,
                              grade0 = c("Grade 0", "Graad 0"),
                              grade1 = c("Grade 1", "Graad 1"),
                              grade2 = c("Grade 2", "Graad 2"),
                              grade3 = c("Grade 3", "Graad 3"),
                              grade4 = c("Grade 4", "Graad 4"),
                              grade5 = c("Grade 5", "Graad 5"),
                              grade6 = c("Grade 6", "Graad 6"),
                              grade7 = c("Grade 7", "Graad 7"),
                              grade8 = c("Grade 8", "Graad 8"),
                              grade9 = c("Grade 9", "Graad 9"),
                              grade10 = c("Grade 10", "Graad 10"),
                              grade11 = c("Grade 11", "Graad 11"),
                              grade12 = c("Grade 12", "Graad 12"),
                              tertiary = c("Tertiary Education", "Tersiere Onderwys")))

# =======================
# Make characters numeric
# =======================
df5 <- df4 %>%
  mutate(agemp = as.numeric(agemp),
         agecp = as.numeric(agecp),
         ageomp = as.numeric(ageomp),
         sfmp = as.numeric(gsub("\\+", "16", sfmp)),
         sfomp = as.numeric(gsub("\\+", "16", sfomp)),
         sfcp = as.numeric(gsub("\\+", "16", sfcp)),
         countcp = as.numeric(gsub("\\+", "16", countcp)),
         countomp = as.numeric(countomp),
         born = as.numeric(born),
         borncp = as.numeric(borncp),
         bornmp = as.numeric(bornmp),
         bornomp = as.numeric(bornomp))
    
# ========================
# Re-format date variables
# ========================
df6 <- df5 %>%
  mutate()

# =============
# Save datasets
# =============

# ====================================================
# Detach libraries and remove objects from environment
# ====================================================
Vectorize(detach)(name = paste0("package:", c("magrittr", "lubridate", "forcats",
                                              "tidyverse", "gdata")), 
                  unload = TRUE, 
                  character.only = TRUE)
rm(list=ls())
