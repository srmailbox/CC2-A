##################################################################################################
# Attempt to understand why CC2-A (Adolescent) doesn't seem to provide a good diagnostic test of
# reading ability. (Relative to the CC2)
# The main difference between the CC2 and the CC2-A is the addition of a bunch of more difficult
# items.
#

# 1.0 - read the data in ####
include(readxl)
cc2A = read_excel("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx", sheet = "CC2-A_FOR.R"
                 , na=c("NA", "n/a"))
cc2A.items = read_excel("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx"
                       , sheet="CC2-A_WordList")

DiSTi = read_excel("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx"
                        , sheet="Disti_FOR.R")

MoST = read_excel("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx"
                   , sheet="MoST_FOR.R")

# 2.0 - item characteristics ####

cc2A.alpha = cc2A %>% select(starts_with("Item")) %>% data.frame %>% psych::alpha()

cc2A.alpha$item.stats = cc2A.alpha$item.stats %>% 
  mutate(ItemID = gsub("Item\\.", "", rownames(.))) %>% 
  merge(cc2A.items, by="ItemID")

# 3.0 DISTi, and MoST ####
## 3.1 Merge into cc2A ####

cc2A =
  cc2A %>% 
  merge(DiSTi %>% select(ID, DiSTi), by="ID", all.x=T) %>% 
  merge(MoST %>% select(ID, MoST.Adj, Rule.Adj, Suffix.Adj), by="ID", all.x=T) %>% 
  mutate(cc2Total = Reg.all+Irregs.all+NW.all)

# 4.0 - Scale correlations ####
# Starting with just the two items that correlate most with the overall scale
# look at how well a test based on only those items would correlate with the
# overall total.

cc2A.bestItems = cc2A.alpha$item.stats %>% arrange(desc(raw.r)) %>% 
  select(ItemID, Item, ItemType) %>% 
  mutate(
    cc2TotCor = NA
    , DiSTiCor = NA
    , MoSTTotCor = NA
    , MoSTRuleCor = NA
    , MoSTSuffixCor = NA
  )

for (i in 1:nrow(cc2A.bestItems)) {
  cc2A.bestItems[i, c("cc2TotCor", "DiSTiCor", "MoSTTotCor", "MoSTRuleCor"
                     , "MoSTSuffixCor")]=
    cor(
      rowSums(
        cc2A %>% select(all_of(paste0("Item.", cc2A.bestItems$ItemID[1:i]))))
      , cc2A %>% select(cc2Total, DiSTi, MoST.Adj, Rule.Adj, Suffix.Adj)
      , use="pairwise.complete"
      )
}

write.csv(cc2A.alpha$item.stats %>% arrange(desc(raw.r))
          , "itemsRankedByTotalCor.csv", row.names=F)
write.csv(cc2A.bestItems, "performanceByTestLength.csv", row.names=F)

## 4.1 Nonword Screener ####

cc2A.nonWords = cc2A.bestItems %>% filter(ItemType=="Nonword")

for (i in 1:nrow(cc2A.nonWords)) {
  cc2A.nonWords[i, c("cc2TotCor", "DiSTiCor", "MoSTTotCor", "MoSTRuleCor"
                      , "MoSTSuffixCor")]=
    cor(
      rowSums(
        cc2A %>% select(all_of(paste0("Item.", cc2A.nonWords$ItemID[1:i])))
        )
      , cc2A %>% select(cc2Total, DiSTi, MoST.Adj, Rule.Adj, Suffix.Adj)
      , use="pairwise.complete"
    )
}
write.csv(cc2A.nonWords, "nwPerformanceByTestLength.csv", row.names=F)
