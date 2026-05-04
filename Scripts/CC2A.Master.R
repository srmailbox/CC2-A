##################################################################################################
# Attempt to understand why CC2-A (Adolescent) doesn't seem to provide a good diagnostic test of
# reading ability. (Relative to the CC2)
# The main difference between the CC2 and the CC2-A is the addition of a bunch of more difficult
# items.
#

include(openxlsx);include(dplyr);include(ggplot2);include(gridExtra)

#### Step 1 - read the CC2-A norming data in ####

cc2A = read.xlsx("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx", sheet = "CC2-A_FOR.R"
                 , na.strings=c("NA", "n/a"))
cc2A.items = read.xlsx("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx"
                       , sheet="CC2-A_WordList")

### Check for item type issues among the scores
if(FALSE){
  all(unlist(lapply(cc2A %>% select(contains("Item.")), is.numeric))) # TRUE, so at least the scores
  unlist(lapply(cc2A %>% select(-contains("Item.")), typeof)) # Dates are integers
  
}

# Other than dates, which are integers, everything seems fine. Dates are not of interest
# so no other clean up.

#### Step 2 - Quick look at the distributions of raw scores ####

### First, let's calculate the "Old" scores vs. the "New"
cc2A = cc2A %>% mutate(Reg.old = Reg.all-Reg.new
                       , Irreg.old = Irregs.all-Irreg.new
                       , NW.old = NW.all - NW.new)

grid.arrange(
  ggplot(cc2A, aes(x=Reg.all, y=..density..))+geom_histogram()+
    geom_density(col="red", fill="red", alpha=.3)+theme_bw()+ggtitle("Reg All")
  , ggplot(cc2A, aes(x=Irregs.all, y=..density..))+geom_histogram()+
    geom_density(col="red", fill="red", alpha=.3)+theme_bw()+ggtitle("Irreg All")
  , ggplot(cc2A, aes(x=NW.all, y=..density..))+geom_histogram()+
    geom_density(col="red", fill="red", alpha=.3)+theme_bw()+ggtitle("Nonw All")
  , ggplot(cc2A, aes(x=Reg.old, y=..density..))+geom_histogram()+
    geom_density(col="red", fill="red", alpha=.3)+theme_bw()+ggtitle("Reg Old")
  , ggplot(cc2A, aes(x=Irreg.old, y=..density..))+geom_histogram()+
    geom_density(col="red", fill="red", alpha=.3)+theme_bw()+ggtitle("Irreg Old")
  , ggplot(cc2A, aes(x=NW.old, y=..density..))+geom_histogram()+
    geom_density(col="red", fill="red", alpha=.3)+theme_bw()+ggtitle("NW Old")
  , ggplot(cc2A, aes(x=Reg.new, y=..density..))+geom_histogram()+
    geom_density(col="red", fill="red", alpha=.3)+theme_bw()+ggtitle("Reg New")
  , ggplot(cc2A, aes(x=Irreg.new, y=..density..))+geom_histogram()+
    geom_density(col="red", fill="red", alpha=.3)+theme_bw()+ggtitle("Irreg New")
  , ggplot(cc2A, aes(x=NW.new, y=..density..))+geom_histogram()+
    geom_density(col="red", fill="red", alpha=.3)+theme_bw()+ggtitle("NW New")
  , nrow=3
)

# COLLAPSING ALL AGES
# The new Regular words help reduce a ceiling effect.
# The new Irregs seem to really only identify particularly strong readers. For the most part, I'd 
# guess the original set was sufficient.
# The Nonwords seem to have soemthing of a bimodal distribution. Here the new items really reduce
# the ceiling effect, which will increase the SD, but also the mean. It might be worth looking at
# thresholds implied by the "-1SD cutoff" rule changes with the new items included.

(cc2A.thrshlds=cc2A %>% select(contains("all"), contains("old")) %>% 
  lapply(function(x) data.frame(mean=mean(x, na.rm=T), sd=sd(x, na.rm=T)
                                , threshold=mean(x, na.rm=T)-sd(x, na.rm=T))) %>% 
  bind_rows(.id="itemset") %>% arrange(itemset))

# So, the -1SD threshold is not altered all that much by the new items, except for the regular words
# Where the threshold goes from 33.8 from the original set of items, to 39 from all of the items.

# How does this affect the proportions of kids that fall "below" the threshold.
# For Irregs, there will be no difference (scores are whole numbers so both thresholds are 22.)
# For Regs, we go from 11.1% to 14.0% (49 to 62)
# For NWs, a small change from 12.6% to 14.2% (56 to 63 kids)

# OK, but all of that ignores the range of ages in the data set.

