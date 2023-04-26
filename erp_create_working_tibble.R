################ NOTES ################ 
# Data came from Dan, with existing 6 averaged conditions for each component.
# Decision to use this set of data was made on April 14th 2023.
# Other option includes individual trial data extracted from a newer EEG software that is not the same source Dan was used to for other ERP projects.
# While the exact differences are still to be identified, some spot checking suggest that they are different.
################################################ 



# Set up -----
library("tidyverse")
library("readxl")

rm(list=ls())
setwd(dir="../data/ERP/raw_data/averaged6conds041423")

# read in raw data (provided by Dan) in preparation of constructing LME models -----
data_N2_P2p_375_475 <- read_csv("RAT_medAD_midlate041423.csv", col_names = F) %>%
  rename(subj_num = X1, # rename header according to readme_RATIO_medAD_P2indtrialdata.txt
         KL.cont = X2,
         time_point = X3,
         erp.p2p_cond1 = X4,
         erp.p2p_cond2 = X5,
         erp.p2p_cond3 = X6,
         erp.p2p_cond4 = X7,
         erp.p2p_cond5 = X8,
         erp.p2p_cond6 = X9,
         erp.n2_cond1 = X10,
         erp.n2_cond2 = X11,
         erp.n2_cond3 = X12,
         erp.n2_cond4 = X13,
         erp.n2_cond5 = X14,
         erp.n2_cond6 = X15) 

data_N1_P2a <- read_csv("RAT_medAD_early41423.csv", col_names = F) %>%
  rename(subj_num = X1, # rename header according to readme_RATIO_medAD_P2indtrialdata.txt
         KL.cont = X2,
         time_point = X3,
         erp.n1_cond1 = X4,
         erp.n1_cond2 = X5,
         erp.n1_cond3 = X6,
         erp.n1_cond4 = X7,
         erp.n1_cond5 = X8,
         erp.n1_cond6 = X9,
         erp.p2a_cond1 = X10,
         erp.p2a_cond2 = X11,
         erp.p2a_cond3 = X12,
         erp.p2a_cond4 = X13,
         erp.p2a_cond5 = X14,
         erp.p2a_cond6 = X15) 


# Combine data frames and tidy
data_erp_all <- full_join(data_N2_P2p_375_475, data_N1_P2a, by = c("subj_num", "time_point", "KL.cont")) %>%
  pivot_longer(cols = starts_with("erp"), names_to = "component", names_prefix = "erp.", values_to = "amp") %>%
  separate(col = component, into = c("component", "cond"), sep = "_") %>%
  mutate(cond = factor(str_extract(cond, "[1-9]$")),
         ratio =
           case_when(
             cond %in% c(1, 2) ~ "close",
             cond %in% c(3, 4) ~ "med",
             cond %in% c(5, 6) ~ "far"),
         distance =
           case_when(
             cond %in% c(1, 2, 3, 4) ~ "1",
             cond %in% c(5, 6) ~ "2"),
         cardinal =
           case_when(
             cond %in% c(4, 6) ~ 1,
             cond %in% c(2, 3) ~ 2,
             cond %in% c(1, 5) ~ 3))

# Save and export ----

save(data_erp_all, file="../../tidied/erp_tidied.RData")

# Archive ----
# data_N2_P2p_375_475 <- read_csv("RATIO_medAD_MIDLATEindtrialdata051323.csv", col_names = F) %>%
#   rename(subj_num = X1, # rename header according to readme_RATIO_medAD_P2indtrialdata.txt
#          KL.cont = X2,
#          cond = X3,
#          erp.p2p = X4,
#          erp.n2 = X5,
#          time_point = X6) 
# 
# data_N1_P2a <- read_csv("RATIO_medAD_EARLYindtrialdata051323.csv", col_names = F) %>%
#   rename(subj_num = X1, # rename header according to readme_RATIO_medAD_N1indtrialdata.txt
#          KL.cont = X2,
#          cond = X3,
#          erp.n1 = X4,
#          erp.p2a = X5,
#          time_point = X6) 
# 
# data_erp_all <- full_join(data_N2_P2p_375_475, data_N1_P2a, by = c("subj_num", "time_point", "cond", "KL.cont")) %>%
#   pivot_longer(cols = starts_with("erp"), names_to = "component", names_prefix = "erp.", values_to = "amp") %>%
#   distinct(subj_num, time_point, KL.cont, cond, component, amp) %>%
#   mutate(ratio = 
#            case_when(
#              cond %in% c(1, 2) ~ "close",
#              cond %in% c(3, 4) ~ "med",
#              cond %in% c(5, 6) ~ "far"),
#          distance = 
#            case_when(
#              cond %in% c(1, 2, 3, 4) ~ "1",
#              cond %in% c(5, 6) ~ "2"),
#          cardinal = 
#            case_when(
#              cond %in% c(4, 6) ~ 1,
#              cond %in% c(2, 3) ~ 2,
#              cond %in% c(1, 5) ~ 3))
_