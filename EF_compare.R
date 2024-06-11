rm(list=ls())
load("./beh/beh_data_tidied.RData")

beh_data <- beh_data.raw %>%
  drop_na(KL) %>%
  mutate(KL.cat =
           case_when(
             KL %in% c(0:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA)))


CCshare <- read_xlsx("./beh/TRN4.20.21.USonlyCCshare.xlsx") # this one
# plosone <- read_xlsx("TRN_DATASET_PLOS_SHARE10.29.21.xlsx") 

rejected_subs <- read_xlsx("ERPrejectedSUBS.xlsx") %>%
  rename(subj_num = SUBNUMB,
         time_point = SESSION) %>%
  select(subj_num, time_point)

my_vars <- c("subjnumber", "sex", "agemonths", 
             "HAVEPOST", "vocabPREss",
             "WMpreALL", "ConflictPRE", "InhibitPRE", 
             "WMpostALL", "ConflictPOST", "InhibitPOST")

EF_df <- CCshare %>% 
  select(all_of(my_vars))

EF_df_tidied <- EF_df %>% 
  rename(subj_num = subjnumber,
         WM_pre = WMpreALL,
         conflict_pre = ConflictPRE,
         inhibit_pre = InhibitPRE,
         WM_post = WMpostALL,
         conflict_post = ConflictPOST,
         inhibit_post = InhibitPOST,
         vocab = vocabPREss) %>%
  pivot_longer(cols = -c(subj_num, -sex, agemonths, -HAVEPOST, vocab),
               names_to = c(".value", "time_point"),
               names_pattern = "(.+)_(.+)") %>%
  drop_na() %>%
  mutate(time_point = as.numeric(factor(time_point, levels = c("pre", "post"))))
  
glimpse(EF_df_tidied)


unmatched_rows <- anti_join(rejected_subs, EF_df_tidied)

EF_rejected <- merge(rejected_subs, EF_df_tidied)
EF_accepted <- beh_data %>% select(subj_num, time_point, WM, CONFLICT, INHIBIT, age.days, VOCAB.SS) %>%
  mutate(agemonths = age.days/30)

t.test(EF_rejected$agemonths, EF_accepted$agemonths, var.equal = T)

t.test(EF_rejected$WM, EF_accepted$WM, var.equal = T)
t.test(EF_rejected$conflict, EF_accepted$CONFLICT, var.equal = T)
t.test(EF_rejected$inhibit, EF_accepted$INHIBIT, var.equal = T)

t.test(EF_rejected$vocab, EF_accepted$VOCAB.SS, var.equal = T)

# new analyses after R2 -----
# A. Excluded group: 42 subjects (for the 4 that contributed 2 trials, get first 
# time point). Included group: 128 subjects minus whoever has an excluded session = 102 subjects

ex_group <- EF_rejected %>% 
  distinct(subj_num, .keep_all = T)

in_group <- EF_accepted %>%
  arrange(time_point) %>% 
  distinct(subj_num, .keep_all = TRUE) %>%
  filter(!subj_num %in% ex_group$subj_num)

t.test(ex_group$agemonths, in_group$agemonths, var.equal = T) #*

t.test(ex_group$WM, in_group$WM, var.equal = T)
t.test(ex_group$conflict, in_group$CONFLICT, var.equal = T)
t.test(ex_group$inhibit, in_group$INHIBIT, var.equal = T)

t.test(ex_group$vocab, in_group$VOCAB.SS, var.equal = T)

# B. Group comparison within time 1 and time 2

time1_in <- EF_accepted %>% filter(time_point == 1)
time2_in <- EF_accepted %>% filter(time_point == 2)

time1_ex <- EF_rejected %>% filter(time_point == 1)
time2_ex <- EF_rejected %>% filter(time_point == 2)

t.test(time1_in$agemonths, time1_ex$agemonths, var.equal = T)
t.test(time2_in$agemonths, time2_ex$agemonths, var.equal = T) #*

t.test(time1_in$WM, time1_ex$WM, var.equal = T)
t.test(time2_in$WM, time2_ex$WM, var.equal = T) #*

t.test(time1_in$CONFLICT, time1_ex$conflict, var.equal = T) 
t.test(time2_in$CONFLICT, time2_ex$conflict, var.equal = T) 

t.test(time1_in$INHIBIT, time1_ex$inhibit, var.equal = T) 
t.test(time2_in$INHIBIT, time2_ex$inhibit, var.equal = T) 

t.test(time1_in$VOCAB.SS, time1_ex$vocab, var.equal = T) #*
