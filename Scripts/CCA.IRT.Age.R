################################################################################
# Plan is to build a new CC2 for adolescents that evaluates Irreg, Non, and
# multimorphemic words
#
# This is similar to the initial analysis, but tries to consider age in the
# modelling.
# 

include(psych);include(mirt);include(ggmirt)
include(readxl);include(dplyr);include(ggplot2);include(gridExtra)

# 1.0 - read the CC2-A norming data in ####

ccA.raw = read_xlsx("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx", sheet = "CC2-A_FOR.R"
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
# Need to order the items by Item num rather than grouped by type
ccA.items$ItemLbl = paste0("Item.", ccA.items$ItemID)
ccA = ccA.raw %>% select(sID, Grade, Age, all_of(ccA.items$ItemLbl)) %>% data.frame
colnames(ccA)[4:(ncol(ccA))]=ccA.items$Item
ccA = ccA %>% select(1:12, all_of(ccA.items$Item))

## 1.1 split by item type ####

ccA.irr = ccA %>% 
  select(Grade, Age, sID
         , all_of(ccA.items$Item[ccA.items$ItemType=="Irregular"]))
ccA.nw = ccA %>% 
  select(Grade, Age, sID, all_of(ccA.items$Item[ccA.items$ItemType=="Nonword"]))

# 2.0 Age effects ####
# Just look at correlations/scatterplots by Age

ccA.irr %>% 
  rowwise() %>% 
  mutate(CCA.irr = rowSums(pick(-c(1:3)), na.rm=T)) %>% 
  select(Age, Grade, CCA.irr) %>% cor(use="pairwise.complete")

ccA.nw %>% 
  rowwise() %>% 
  mutate(CCA.nw = rowSums(pick(-c(1:3)), na.rm=T)) %>% 
  select(Age, Grade, CCA.nw) %>% cor(use="pairwise.complete")

grid.arrange(
  ccA.irr %>% 
  rowwise() %>% 
  mutate(CCA.irr = rowSums(pick(-c(1:3)), na.rm=T)) %>% 
  ggplot(aes(x=Age, y=CCA.irr))+geom_point(aes(col=factor(Grade)))+geom_smooth()

,ccA.nw %>% 
  rowwise() %>% 
  mutate(CCA.nw = rowSums(pick(-c(1:3)), na.rm=T)) %>% 
  ggplot(aes(x=Age, y=CCA.nw, col=Grade))+geom_point(aes(col=factor(Grade)))+
  geom_smooth()
) 

## So there is a clear relationship with age/Grade
## Particularly for Irr which correlaes .39/.41
## NW: .27/.29
## 

## 2.1 by Grade ####
## There aren't enough kids in each grade to comfortably fit them separately
## So I'll do this by combining grades 6/7, 8/9, and 10/11
