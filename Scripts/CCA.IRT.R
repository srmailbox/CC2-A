################################################################################
# Plan is to build a new CC2 for adolescents that evaluates Irreg, Non, and
# multimorphemic words
#
# this script tries to identify existing items that might be suitable.

include(readxl);include(dplyr);include(ggplot2);include(gridExtra)
include(psych)

# 1.0 - read the CC2-A norming data in ####

ccA = read_xlsx("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx", sheet = "CC2-A_FOR.R"
                 , na = c("NA", "n/a")) %>% 
  rename(Age = `Age (months)`, sID = `SUBJECT CODE`)
ccA.items = read_xlsx("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx"
                       , sheet="CC2-A_WordList")

### Check for item type issues among the scores
if(FALSE){
  all(unlist(lapply(ccA %>% select(contains("Item.")), is.numeric))) # TRUE, so at least the scores
  unlist(lapply(ccA %>% select(-contains("Item.")), typeof)) # Dates are integers
}

# Other than dates, which are integers, everything seems fine. Dates are not of interest
# so no other clean up.

# replace columns with actual items
ccA.items$ItemLbl = paste0("Item.", ccA.items$ItemID)
ccA = ccA %>% select(1:12, all_of(ccA.items$ItemLbl))
colnames(ccA)[13:ncol(ccA)]=ccA.items$Item

## 1.1 split by item type ####

ccA.irr = ccA %>% 
  select(Grade, Age, sID
         , all_of(ccA.items$Item[ccA.items$ItemType=="Irregular"]))
ccA.nw = ccA %>% 
  select(Grade, Age, sID, all_of(ccA.items$Item[ccA.items$ItemType=="Nonword"]))

# 2.0 IRT ####
# 
# Alright let's look at the two item types and see what our IRT gives us

## 2.1 Irregs ####
ccA.irr.irt = irt.fa(ccA.irr %>% select(-c(1:3)) %>% 
                     drop_na() %>% data.frame, 10)

## 2.2 Nonwords ####

## 2.1 Irregs ####
ccA.nw.irt = irt.fa(ccA.nw %>% select(-c(1:3)) %>% 
                       drop_na() %>% data.frame, 1)
