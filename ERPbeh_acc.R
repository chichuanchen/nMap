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
## Accuracy by condition (6) BASE-----

acc.by.subj.cond <- nmap_data.long %>%
  rename(subj_num = Subject,
         time_point = Session) %>%
  group_by(subj_num, time_point, cue, probe) %>%
  mutate(cond_acc = mean(correct_or_not, na.rm=T)) %>%
  distinct(subj_num, time_point, cue, probe, cond_acc) %>%
  ungroup() %>%
  mutate(approx.dist = case_when(
           cue > probe ~ cue/probe,
           cue < probe ~ probe/cue),
         exact.dist = abs(cue - probe))

# write.csv(acc.by.subj.cond, file = "../data/ERPbeh/ERPbeh_acc_subj_6cond.csv")

## Accuracy by subject (across all 6 conditions) -----

acc.by.subj <-  acc.by.subj.cond %>%
  group_by(subj_num, time_point) %>%
  summarise(subj_acc = mean(cond_acc, na.rm=T),
            subj_acc_sd = sd(cond_acc, na.rm=T),
            n=n(),
            subj_acc_se = subj_acc_sd/sqrt(n)) %>%
  ungroup()

write.csv(acc.by.subj, file = "../data/ERPbeh/ERPbeh_acc_subj.csv")

## Accuracy by approximate distance effect (close minus far) -----
acc.approx.dist.effect <- acc.by.subj.cond %>%
  group_by(subj_num, time_point, approx.dist) %>%
  summarise(avg.acc.approx.dist = mean(cond_acc)) %>%
  group_by(subj_num, time_point) %>%
  summarise(approx.dist.acc = avg.acc.approx.dist[approx.dist == 1.5] - avg.acc.approx.dist[approx.dist == 3]) %>%
  ungroup()

write.csv(acc.approx.dist.effect, file = "../data/ERPbeh/ERPbeh_acc_approx_dist_effect.csv")
save(acc.approx.dist.effect, file="../data/ERPbeh/acc.approx.dist.effect.RData")
## Accuracy by exact distance effect (2 - 1) -----
acc.exact.dist.effect <- acc.by.subj.cond %>%
  group_by(subj_num, time_point, exact.dist) %>%
  summarise(avg.acc.exact.dist = mean(cond_acc)) %>%
  group_by(subj_num, time_point) %>%
  summarise(exact.dist.acc = avg.acc.exact.dist[exact.dist == "2"] - avg.acc.exact.dist[exact.dist == "1"]) %>%
  ungroup()

write.csv(acc.exact.dist.effect, file = "../data/ERPbeh/ERPbeh_acc_exact_dist_effect.csv")
save(acc.exact.dist.effect, file="../data/ERPbeh/acc.exact.dist.effect.RData")

#
dis.effects <- acc.approx.dist.effect %>%
  full_join(acc.exact.dist.effect, by = c("subj_num","time_point")) %>%
  drop_na(approx.dist.acc, exact.dist.acc)
cor.test(dis.effects$approx.dist.acc, dis.effects$exact.dist.acc, method = "pearson")

# ERPbeh x Beh #################################################################
# combine ERPbeh acc with subj info (sex, KL, EF, age, vocab)

## By condition (6) -----
ERPbeh_info_by.cond <- acc.by.subj.cond %>%
  right_join(beh_data.raw, by = c("subj_num", "time_point")) %>%
  mutate(KL.cat = 
           factor(case_when(
             KL %in% c(1:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA))))

write.csv(ERPbeh_info_by.cond, file = "../data/ERPbeh/ERPbeh_acc_subj_6cond.csv")
### Acc by distance -----
# approximate distance
ERPbeh_info_by.cond %>%
  group_by(subj_num, time_point, KL.cat, approx.dist) %>%
  summarise(approx.dist.acc = mean(cond_acc)) %>%
  group_by(KL.cat, approx.dist) %>%
  summarise(group.approx.dist.acc = mean(approx.dist.acc),
            group.approx.dist.acc.sd = sd(approx.dist.acc),
            n = n(),
            group.approx.dist.acc.se = group.approx.dist.acc.sd/sqrt(n)) %>%
  drop_na()

# exact distance
ERPbeh_info_by.cond %>%
  group_by(subj_num, time_point, KL.cat, exact.dist) %>%
  summarise(exact.dist.acc = mean(cond_acc)) %>%
  group_by(KL.cat, exact.dist) %>%
  summarise(group.exact.dist.acc = mean(exact.dist.acc),
            group.exact.dist.acc.sd = sd(exact.dist.acc),
            n = n(),
            group.exact.dist.acc.se = group.exact.dist.acc.sd/sqrt(n)) %>%
  drop_na()

## By exact distance (2) -----
ERPbeh_info_by.exact.dist <- acc.exact.dist.effect %>%
  right_join(beh_data.raw, by = c("subj_num", "time_point")) %>%
  mutate(KL.cat = 
           factor(case_when(
             KL %in% c(1:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA))))

## By approximate distance (3) -----
ERPbeh_info_by.approx.dist <- acc.approx.dist.effect %>%
  right_join(beh_data.raw, by = c("subj_num", "time_point")) %>%
  mutate(KL.cat = 
           factor(case_when(
             KL %in% c(1:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA))))

## By subject -----
ERPbeh_info_by.subj <- acc.by.subj %>%
  right_join(beh_data.raw, by = c("subj_num", "time_point")) %>%
  mutate(KL.cat = 
           factor(case_when(
             KL %in% c(1:4) ~ "SS",
             KL %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA)))) %>%
  ungroup()



## Descriptive -----
table(duplicated(ERPbeh_info_by.subj$subj_num))

#### Overall task accuracy and age -----
ERPbeh_info_by.subj %>%
  group_by(KL.cat) %>%
  summarise(mean.acc.KL = mean(subj_acc, na.rm=T),
            sd.acc.KL = sd(subj_acc, na.rm=T),
            n = n(),
            se.acc.KL = sd.acc.KL/sqrt(n),
            mean.age = mean(age.days)/365,
            sd.age = sd(age.days)/365) %>%
  as.data.frame()

ERPbeh_info_by.subj %>%
  # group_by(KL.cat) %>%
  summarise(mean.acc = mean(subj_acc, na.rm=T),
            sd.acc = sd(subj_acc, na.rm=T),
            n = n(),
            se.acc = sd.acc/sqrt(n),
            mean.age = mean(age.days)/365,
            sd.age = sd(age.days)/365) %>%
  as.data.frame()

### Stats -----
ERPbeh_info_by.subj.SS <- ERPbeh_info_by.subj %>%
  filter(KL.cat == "SS") %>%
  ungroup() 

ERPbeh_info_by.subj.CP <- ERPbeh_info_by.subj %>%
  filter(KL.cat == "CP") %>%
  ungroup() 
#### Age and EF and vocab difference between groups? -----

t.test(ERPbeh_info_by.subj.CP$age.days, ERPbeh_info_by.subj.SS$age.days)
t.test(ERPbeh_info_by.subj.CP$WM, ERPbeh_info_by.subj.SS$WM)
t.test(ERPbeh_info_by.subj.CP$CONFLICT, ERPbeh_info_by.subj.SS$CONFLICT)
t.test(ERPbeh_info_by.subj.CP$INHIBIT, ERPbeh_info_by.subj.SS$INHIBIT)
t.test(ERPbeh_info_by.subj.CP$VOCAB, ERPbeh_info_by.subj.SS$VOCAB)

#### Overall performance > chance? ----

t.test(ERPbeh_info_by.subj.SS$subj_acc, mu=.5)
t.test(ERPbeh_info_by.subj.CP$subj_acc, mu=.5)

#### Overall performance group difference? ----
ERPbeh_info_by.subj %>%
  t.test(subj_acc ~ KL.cat, data = .)
  

#### Overall performance distance effect? ----
# by group

model.exact.dis.CP <- aov(cond_acc~factor(exact.dist)+Error(factor(subj_num)), 
                data = subset(ERPbeh_info_by.cond, KL.cat == "CP"))
summary(model.exact.dis.CP)

model.approx.dis.CP <- aov(cond_acc~factor(approx.dist)+Error(factor(subj_num)), 
                          data = subset(ERPbeh_info_by.cond, KL.cat == "CP"))
summary(model.approx.dis.CP)

#

model.exact.dis.SS <- aov(cond_acc~factor(exact.dist)+Error(factor(subj_num)), 
                          data = subset(ERPbeh_info_by.cond, KL.cat == "SS"))
summary(model.exact.dis.SS)

model.approx.dis.SS <- aov(cond_acc~factor(approx.dist)+Error(factor(subj_num)), 
                           data = subset(ERPbeh_info_by.cond, KL.cat == "SS"))
summary(model.approx.dis.SS)



#### Correlations -----
##### ERPbeh acc and EF -----
corr.acc_WM <- ERPbeh_info_by.subj %>%
  drop_na(subj_acc, WM) 
 
corr.acc_CONFLICT <- ERPbeh_info_by.subj %>%
  drop_na(subj_acc, CONFLICT) 

corr.acc_INHIBIT <- ERPbeh_info_by.subj %>%
  drop_na(subj_acc, INHIBIT) 

cor.test(corr.acc_INHIBIT$subj_acc, corr.acc_INHIBIT$INHIBIT)
cor.test(corr.acc_WM$subj_acc, corr.acc_WM$WM)
cor.test(corr.acc_CONFLICT$subj_acc, corr.acc_CONFLICT$CONFLICT)


pvals <- cor.test(corr.acc_INHIBIT$subj_acc, corr.acc_INHIBIT$INHIBIT)$p.value
pvals <- c(pvals, cor.test(corr.acc_WM$subj_acc, corr.acc_WM$WM)$p.value)
pvals <- c(pvals, cor.test(corr.acc_CONFLICT$subj_acc, corr.acc_CONFLICT$CONFLICT)$p.value)

# Perform FDR correction
p.adjust(pvals, method = "fdr")

##### beh distance effect and EF -----
###### approximate distance -----
corr.ratio_WM <- ERPbeh_info_by.subj %>%
  left_join(acc.approx.dist.effect, by = c("subj_num", "time_point")) %>%
  drop_na(approx.dist.acc, WM) 

corr.ratio_CONFLICT <- ERPbeh_info_by.subj %>%
  left_join(acc.approx.dist.effect, by = c("subj_num", "time_point")) %>%
  drop_na(approx.dist.acc, CONFLICT) 

corr.ratio_INHIBIT <- ERPbeh_info_by.subj %>%
  left_join(acc.approx.dist.effect, by = c("subj_num", "time_point")) %>%
  drop_na(approx.dist.acc, INHIBIT) 

pvals <- cor.test(corr.ratio_WM$approx.dist.acc, corr.ratio_WM$WM)$p.value
pvals <- c(pvals, cor.test(corr.ratio_CONFLICT$approx.dist.acc, corr.ratio_CONFLICT$CONFLICT)$p.value)
pvals <- c(pvals, cor.test(corr.ratio_INHIBIT$approx.dist.acc, corr.ratio_INHIBIT$INHIBIT)$p.value)

# Perform FDR correction
p.adjust(pvals, method = "fdr")

###### exact distance -----
corr.ratio_WM <- ERPbeh_info_by.subj %>%
  left_join(acc.exact.dist.effect, by = c("subj_num", "time_point")) %>%
  drop_na(exact.dist.acc, WM) 

corr.ratio_CONFLICT <- ERPbeh_info_by.subj %>%
  left_join(acc.exact.dist.effect, by = c("subj_num", "time_point")) %>%
  drop_na(exact.dist.acc, CONFLICT) 

corr.ratio_INHIBIT <- ERPbeh_info_by.subj %>%
  left_join(acc.exact.dist.effect, by = c("subj_num", "time_point")) %>%
  drop_na(exact.dist.acc, INHIBIT) 

pvals <- cor.test(corr.ratio_WM$exact.dist.acc, corr.ratio_WM$WM)$p.value
pvals <- c(pvals, cor.test(corr.ratio_CONFLICT$exact.dist.acc, corr.ratio_CONFLICT$CONFLICT)$p.value)
pvals <- c(pvals, cor.test(corr.ratio_INHIBIT$exact.dist.acc, corr.ratio_INHIBIT$INHIBIT)$p.value)

# Perform FDR correction
p.adjust(pvals, method = "fdr")


##
exact.wm <- ERPbeh_info_by.exact.dist %>%
  drop_na(exact.dist.acc, WM)
cor.test(exact.wm$exact.dist.acc, exact.wm$WM, method = "pearson")

exact.inhibit <- ERPbeh_info_by.exact.dist %>%
  drop_na(exact.dist.acc, INHIBIT)
cor.test(exact.inhibit$exact.dist.acc, exact.inhibit$INHIBIT, method = "pearson")

exact.CONFLICT <- ERPbeh_info_by.exact.dist %>%
  drop_na(exact.dist.acc, CONFLICT)
cor.test(exact.CONFLICT$exact.dist.acc, exact.CONFLICT$CONFLICT, method = "pearson")

exact.VOCAB <- ERPbeh_info_by.exact.dist %>%
  drop_na(exact.dist.acc, VOCAB)
cor.test(exact.VOCAB$exact.dist.acc, exact.VOCAB$VOCAB, method = "pearson")

exact.age <- ERPbeh_info_by.exact.dist %>%
  drop_na(exact.dist.acc, age.days)
cor.test(exact.age$exact.dist.acc, exact.age$age.days, method = "pearson")

#


approx.wm <- ERPbeh_info_by.approx.dist %>%
  drop_na(approx.dist.acc, WM)
cor.test(approx.wm$approx.dist.acc, approx.wm$WM, method = "pearson")

approx.inhibit <- ERPbeh_info_by.approx.dist %>%
  drop_na(approx.dist.acc, INHIBIT)
cor.test(approx.inhibit$approx.dist.acc, approx.inhibit$INHIBIT, method = "pearson")

approx.CONFLICT <- ERPbeh_info_by.approx.dist %>%
  drop_na(approx.dist.acc, CONFLICT)
cor.test(approx.CONFLICT$approx.dist.acc, approx.CONFLICT$CONFLICT, method = "pearson")

approx.VOCAB <- ERPbeh_info_by.approx.dist %>%
  drop_na(approx.dist.acc, VOCAB)
cor.test(approx.VOCAB$approx.dist.acc, approx.VOCAB$VOCAB, method = "pearson")

approx.age <- ERPbeh_info_by.approx.dist %>%
  drop_na(approx.dist.acc, age.days)
cor.test(approx.age$approx.dist.acc, approx.age$age.days, method = "pearson")