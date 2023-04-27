############ NOTES ############ 
# processing of individual trials and some data stats
#################################### 

# Set up -----
library("tidyverse")
library("readxl")

rm(list=ls())
setwd(dir="../data/ERP/raw_data/reprocessed040823")

# read in raw data (provided by Dan) in preparation of constructing LME models -----
data_N2_P2p_375_475 <- read_csv("RATIO_medAD_P2indtrialdata040823.csv", col_names = F) %>%
  rename(subj_num = X1, # rename header according to readme_RATIO_medAD_P2indtrialdata.txt
         KL.cont = X2,
         cond = X3,
         erp.p2p = X4,
         erp.n2 = X5,
         time_point = X6) %>%
  select(subj_num, time_point, KL.cont, cond, erp.p2p, erp.n2) %>%
  group_by(subj_num, time_point, cond) %>%
  mutate(trial_num = row_number()) %>%
  ungroup()

data_N1_P2a <- read_csv("RATIO_medAD_N1indtrialdata040823.csv", col_names = F) %>%
  rename(subj_num = X1, # rename header according to readme_RATIO_medAD_N1indtrialdata.txt
         KL.cont = X2,
         cond = X3,
         erp.n1 = X4,
         erp.p2a = X5,
         time_point = X6) %>%
  select(subj_num, time_point, KL.cont, cond, erp.n1, erp.p2a) %>%
  group_by(subj_num, time_point, cond) %>%
  mutate(trial_num = row_number()) %>%
  ungroup()


# Combine data frames and tidy
data_erp_all_indi_trial <- full_join(data_N2_P2p_375_475, data_N1_P2a,
                           relationship = "many-to-many") %>%
  pivot_longer(cols = starts_with("erp"), names_to = "component", names_prefix = "erp.", values_to = "amp") %>%
  distinct(subj_num, time_point, KL.cont, cond, component, amp, trial_num) %>%
  mutate(ratio = 
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

save(data_erp_all_indi_trial, file="../../tidied/erp_individual_trial.RData")

