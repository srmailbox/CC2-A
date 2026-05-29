################################################################################
# Plan is to build a new CC2 for adolescents that evaluates Irreg, Non, and
# multimorphemic words
#
# this script tries to identify existing items that might be suitable.

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

# 2.0 IRT ####
# 
# Alright let's look at the two item types and see what our IRT gives us

## 2.1 Irregs ####
# give has perfect accuracy, and so can't be used here.

ccA.irr.irt = mirt(ccA.irr %>% select(-c(1:3), -give) %>% drop_na
                   , model = 1
                   , itemtype="2PL")


ccA.irr.irtfa = irt.fa(ccA.irr %>% 
                         select(ccA.items$Item[ccA.items$ItemType=="Irregular"]
                                , -give)
                       , 1)
ccA.irr.irtInfo = plot(ccA.irr.irtfa, cut=0)
cbind(ccA.irr.irtfa$irt$difficulty[[1]], ccA.irr.irtfa$irt$discrimination)

## 2.2 Nonwords ####

ccA.nw.irt = mirt(ccA.nw %>% select(-c(1:3))
                  , model = 1, itemtype="2PL")

ccA.nw.irtfa = irt.fa(ccA.nw %>% select(-c(1:3))
                  , 1)

ccA.nw.irtfa$irt$discrimination

ccA.nw.irtInfo = plot(ccA.nw.irtfa)
# 3.0 Factor Analysis ####
# The IRT seems to suggest that different factors are involved here. Let's
# take a closer look at that.

## 3.1 Irregs ####

ccA.irr.fa = factanal(ccA.irr %>% select(-c(1:3)) %>% drop_na,14)

# 4.0 Drop low discrimination items - IRR ####
# Many irregular items have extremely poor discrimination, often due to very 
# high (or low) accuracy.
# These items have discriminations less than .3

lowDisc = read_lines('eye
couple
good
friend
blood
come
soul
work
shoe
island
sure
cough
break
ceiling
iron
routine
bowl
tomb
wolf
deaf
choir
shove
yacht
lose
bouquet
crêpe
meringue
cello
genre
depot
brooch')

## 4.1 refit IRT ####
ccA.irr.restricted = irt.fa(ccA.irr %>% select(-c(1:3),-all_of(lowDisc))
                      , 1)
