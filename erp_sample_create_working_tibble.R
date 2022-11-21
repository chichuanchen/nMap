# Set up -----
library("tidyverse")
library("readxl")

rm(list=ls())
setwd(dir="../data/ERP/sample_data")

# read in sample data in preparation of constructing LME models -----
data_sample <- read_csv("RATIO_medAD_P2indtrialdata.csv", col_names = F) %>%
  rename(subj_num = X1, # rename header according to readme.txt
         KL.cont = X2,
         cond = X3,
         erp.p2p = X4,
         erp.n2 = X5,
         time_point = X6) %>%
  mutate(ratio = 
           case_when(
             cond %in% c(1, 2) ~ "close",
             cond %in% c(3, 4) ~ "med",
             cond %in% c(5, 6) ~ "far"
           ))

# Save and export ----

save(data_sample, file="../tidied/erp_sample_tidied.RData")

