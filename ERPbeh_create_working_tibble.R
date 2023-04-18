#######################
# This script reads in behavioral response data during ERP collection 
# (answering whether the quantity of visually presented items is more or less than the heard number words)
# and outputs data frames with individual performance (accuracy across time points and conditions) in both long and wide format
#######################

# Set up
library("tidyverse")
library("readxl")

rm(list=ls())
setwd(dir="../data/ERPbeh")

# # filter data using this list of subjects
# data.subjlist <- read_csv("BEHAVIORAL_ERPGOOD_SUBLIST.csv", col_names = T) %>%
#   select(SUBNUMB, PREPOST)
# 
# table(data.subjlist$PREPOST)

# variables of interest
my_vars <- c("Subject", "Session", "Trial", "image[SubTrial]", "sound1[SubTrial]", "tag", 
             "animalpic.RESP", "respdisp.RESP")

# prep pre and post data sets
data.pre <- read_csv("nmap_merged_PRE.csv", col_names = T) %>%
  select(all_of(my_vars)) %>%
  rename(stim.image = `image[SubTrial]`,
         stim.sound = `sound1[SubTrial]`) %>%
  filter(Session == 1) %>% # make sure all observations are from session 1
         # Subject %in% data.subjlist$SUBNUMB[data.subjlist$PREPOST==1])  %>% # filter subjects whose ERP data are usable
  drop_na(stim.image)

data.post <- read_csv("nmap_merged_POST.csv", col_names = T) %>%
  select(my_vars) %>%
  rename(stim.image = `image[SubTrial]`,
         stim.sound = `sound1[SubTrial]`) %>%
  filter(Session==2) %>% # make sure all observations are from session 2
         # Subject %in% data.subjlist$SUBNUMB[data.subjlist$PREPOST==2]) %>% # filter subjects whose ERP data are usable
  drop_na(stim.image)

# combine datasets and tidy
# long format
data.long <- bind_rows(data.pre, data.post) %>%
  separate(tag, into = c("cue", "probe"), sep = "0", convert = T) %>%
  mutate(
    inconsist.resp = 
      case_when(
        animalpic.RESP == respdisp.RESP ~ 0,
        is.na(animalpic.RESP) | is.na(respdisp.RESP) ~ 0,
        TRUE ~ 1),
    cresp = 
      case_when(
        cue < probe ~ 4,
        cue > probe ~ 1),
    correct_or_not = 
      case_when(
        animalpic.RESP == cresp | respdisp.RESP == cresp ~ 1, # if response during either pic or after pic is correct, then is correct
        TRUE ~ 0), # the rest is incorrect (including inconsistent responses)
    cond = 
      case_when(
        cue == 1 & probe == 2 ~ "med",
        cue == 2 & probe == 1 ~ "med",
        cue == 2 & probe == 3 ~ "close",
        cue == 3 & probe == 2 ~ "close",
        cue == 1 & probe == 3 ~ "far",
        cue == 3 & probe == 1 ~ "far"),
    order = if_else(cue < probe, "asc", "desc")
        ) %>%
  select(Subject, Session, Trial, cue, probe, order, cond, cresp, inconsist.resp, correct_or_not, stim.image, stim.sound)

# wide format
data.wide <- data.long %>%
  unite("condition", c(cue, probe), sep = "_") %>%
  group_by(Subject, Session, condition) %>%
  summarise(individual_acc = mean(correct_or_not)) %>%
  pivot_wider(names_from = condition,
              values_from = individual_acc)

# Save and export ----
## rename
nmap_data.long <- data.long
nmap_acc.wide <- data.wide

save(nmap_data.long, nmap_acc.wide, file="./tidied/nmap_data_tidied.RData")
