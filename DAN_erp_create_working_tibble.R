# Set up -----
library("tidyverse")
library("readxl")

rm(list=ls())
setwd(dir="../averaged6conds041423")

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

  mutate(ratio = 
           case_when(
             cond %in% c("cond1", "cond2") ~ "close",
             cond %in% c("cond3", "cond4") ~ "med",
             cond %in% c("cond5", "cond6") ~ "far"),
         distance = 
           case_when(
             cond %in% c("cond1", "cond2", "cond3", "cond4") ~ "1",
             cond %in% c("cond5", "cond6") ~ "2"),
         cardinal = 
           case_when(
             cond %in% c("cond4", "cond6") ~ 1,
             cond %in% c("cond2", "cond3") ~ 2,
             cond %in% c("cond1", "cond5") ~ 3))

# Save and export ----

save(data_erp_all, file="../../tidied/DAN_erp_tidied.RData")

_