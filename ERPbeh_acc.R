# SETUP
library("lme4")
library("car")


rm(list = ls())
# import all data -----
load("../data/ERPbeh/tidied/nmap_data_tidied.RData") 
load("../data/beh/beh_data_tidied.RData")
load("../data/ERP/tidied/erp_tidied.RData")

# wide format acc already calculated in a tibble, now output
# write.csv(nmap_acc.wide, file = "acc_subj_6cond_wide.csv", row.names = FALSE)

# ERPbeh #######################################################################
## Accuracy by condition (6) -----

acc.by.subj.cond <- nmap_data.long%>%
  group_by(Subject, Session, cue, probe) %>%
  summarise(individual_acc = mean(correct_or_not, na.rm=T)) %>%
  ungroup()

write.csv(acc.by.subj.cond, file = "../data/ERPbeh/ERPbeh_acc_subj_6cond.csv")

## Accuracy by subject (across all 6 conditions) -----
acc.by.subj <- nmap_data.long%>%
  group_by(Subject, Session) %>%
  summarise(individual_acc = mean(correct_or_not, na.rm=T)) %>%
  ungroup()

write.csv(acc.by.subj, file = "../data/ERPbeh/ERPbeh_acc_subj.csv")

## Accuracy by ratio effect (close minus far) -----
acc.ratio.effect <- nmap_data.long %>%
  rename(subj_num = Subject,
         time_point = Session) %>%
  group_by(subj_num, time_point, cue, probe) %>%
  mutate(cond_acc = mean(correct_or_not, na.rm=T),
         cond = factor(cond)) %>%
  distinct(subj_num, time_point, cue, probe, cond, cond_acc) %>%
  group_by(subj_num, time_point, cond) %>%
  summarise(avg.acc.ratio = mean(cond_acc)) %>%
  group_by(subj_num, time_point) %>%
  summarise(ratio.acc = avg.acc.ratio[cond == "close"] - avg.acc.ratio[cond == "far"]) %>%
  ungroup()

write.csv(acc.ratio.effect, file = "../data/ERPbeh/ERPbeh_acc_ratioeffect.csv")

# ERPbeh x Beh #################################################################
# combine ERPbeh acc with subj info (sex, KL, EF, age, vocab)

## By condition (6) -----
ERPbeh_info_by.cond <- acc.by.subj.cond %>%
  rename(subj_num = Subject,
         time_point = Session) %>%
  right_join(beh_data.raw, by = c("subj_num", "time_point")) %>%
  mutate(KL.cat = 
           factor(case_when(
             KL %in% c(1:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA))),
         distance = factor(abs(cue - probe))) 


## By distance (2) -----
ERPbeh_info_by.distance <- ERPbeh_info_by.cond %>%
  group_by(subj_num, time_point, KL.cat, distance) %>%
  summarise(avg.acc.dist = mean(individual_acc))

## By subject -----
ERPbeh_info_by.subj <- acc.by.subj %>%
  rename(subj_num = Subject,
         time_point = Session) %>%
  right_join(beh_data.raw, by = c("subj_num", "time_point")) %>%
  mutate(KL.cat = 
           factor(case_when(
             KL %in% c(1:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA)))) %>%
  ungroup()

### Descriptive -----
#### Overall task accuracy by CP-status -----
ERPbeh_info_by.subj %>%
  group_by(KL.cat) %>%
  summarise(mean.acc.KL = mean(individual_acc, na.rm=T),
            sd.acc.KL = sd(individual_acc, na.rm=T),
            n = n())

### Stats -----
#### Overall performance > chance? ----
ERPbeh_info_by.subj.SS <- ERPbeh_info_by.subj %>%
  filter(KL.cat == "SS") %>%
  ungroup() 

t.test(ERPbeh_info_by.subj.SS$individual_acc, mu=.5)

ERPbeh_info_by.subj.CP <- ERPbeh_info_by.subj %>%
  filter(KL.cat == "CP") %>%
  ungroup() 

t.test(ERPbeh_info_by.subj.CP$individual_acc, mu=.5)

#### Overall performance group difference? ----
ERPbeh_info_by.subj %>%
  t.test(individual_acc ~ KL.cat, data = .)
  

#### Overall performance distance effect? ----
# by group
model.dis.SS <- lm(avg.acc.dist ~ distance,
     data = subset(ERPbeh_info_by.cond, KL.cat == "SS"))

anova(model.dis.SS)

model.dis.CP <- lm(avg.acc.dist ~ distance,
                   data = subset(ERPbeh_info_by.cond, KL.cat == "CP"))

anova(model.dis.CP)


#### Correlations -----
##### ERPbeh acc and EF -----
corr.acc_WM <- ERPbeh_info_by.subj %>%
  drop_na(individual_acc, WM) 
 
corr.acc_CONFLICT <- ERPbeh_info_by.subj %>%
  drop_na(individual_acc, CONFLICT) 

corr.acc_INHIBIT <- ERPbeh_info_by.subj %>%
  drop_na(individual_acc, INHIBIT) 

cor.test(corr.acc_WM$individual_acc, corr.acc_WM$WM)
cor.test(corr.acc_CONFLICT$individual_acc, corr.acc_CONFLICT$CONFLICT)
cor.test(corr.acc_INHIBIT$individual_acc, corr.acc_INHIBIT$INHIBIT)

##### beh ratio effect and EF -----
corr.ratio_WM <- ERPbeh_info_by.subj %>%
  left_join(acc.ratio.effect, by = c("subj_num", "time_point")) %>%
  drop_na(ratio.acc, WM) 

corr.ratio_CONFLICT <- ERPbeh_info_by.subj %>%
  left_join(acc.ratio.effect, by = c("subj_num", "time_point")) %>%
  drop_na(ratio.acc, CONFLICT) 

corr.ratio_INHIBIT <- ERPbeh_info_by.subj %>%
  left_join(acc.ratio.effect, by = c("subj_num", "time_point")) %>%
  drop_na(ratio.acc, INHIBIT) 

pvals <- cor.test(corr.ratio_WM$ratio.acc, corr.ratio_WM$WM)$p.value
pvals <- c(pvals, cor.test(corr.ratio_CONFLICT$ratio.acc, corr.ratio_CONFLICT$CONFLICT)$p.value)
pvals <- c(pvals, cor.test(corr.ratio_INHIBIT$ratio.acc, corr.ratio_INHIBIT$INHIBIT)$p.value)

# Perform FDR correction
p.adjust(pvals, method = "fdr")
