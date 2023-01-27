# Set up -----
library("tidyverse")
library("readxl")

rm(list=ls())
setwd(dir="../data/ERP/raw_data")

# read in raw data (provided by Dan) in preparation of constructing LME models -----
data_N2_P2p_375_475 <- read_csv("RATIO_medAD_P2indtrialdata.csv", col_names = F) %>%
  rename(subj_num = X1, # rename header according to readme_RATIO_medAD_P2indtrialdata.txt
         KL.cont = X2,
         cond = X3,
         erp.p2p = X4,
         erp.n2 = X5,
         time_point = X6) %>%
  mutate(ratio = 
           case_when(
             cond %in% c(1, 2) ~ "close",
             cond %in% c(3, 4) ~ "med",
             cond %in% c(5, 6) ~ "far"),
         distance = 
           case_when(
             cond %in% c(1, 2, 3, 4) ~ "1",
             cond %in% c(5, 6) ~ "2"))

data_N1_P2a <- read_csv("RATIO_medAD_N1indtrialdata.csv", col_names = F) %>%
  rename(subj_num = X1, # rename header according to readme_RATIO_medAD_N1indtrialdata.txt
         KL.cont = X2,
         cond = X3,
         erp.n1 = X4,
         erp.p2a = X5,
         time_point = X6) %>%
  mutate(cardinal = 
           case_when(
             cond %in% c(4, 6) ~ 1,
             cond %in% c(2, 3) ~ 2,
             cond %in% c(1, 5) ~ 3))

# Save and export ----

save(data_N2_P2p_375_475, data_N1_P2a, file="../tidied/erp_tidied.RData")

_