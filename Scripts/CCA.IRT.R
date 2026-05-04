################################################################################
# Plan is to build a new CC2 for adolescents that evaluates Irreg, Non, and
# multimorphemic words
#
# this script tries to identify existing items that might be suitable.

include(openxlsx);include(dplyr);include(ggplot2);include(gridExtra)

#### Step 1 - read the CC2-A norming data in ####

cc2A = read.xlsx("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx", sheet = "CC2-A_FOR.R"
                 , na.strings=c("NA", "n/a"))
cc2A.items = read.xlsx("MASTER_FINAL_DECEMBER 2017_Data_entry_spreadsheets.SR.xlsx"
                       , sheet="CC2-A_WordList")