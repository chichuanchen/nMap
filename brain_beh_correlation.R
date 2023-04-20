# Set up -----
library("tidyverse")
library("readxl")
library("lme4")
library("lmerTest")
library("lattice") # used for visual inspection of normal distribution of residuals
library("car") # leveneTest
library("emmeans") # extract estimated marginal means
# library("MuMIn") # for calculating r squared from LMEs

rm(list = ls())
load("../data/ERP/tidied/erp_tidied.RData")
load("../data/beh/beh_data_tidied.RData")

#
data_erp_all <- data_erp_all %>%
  mutate(KL.cat = 
           case_when(
             KL.cont %in% c(1:4) ~ "SS",
             KL.cont %in% c(5:8) ~ "CP",
             TRUE ~ as.character(NA)))

# calculate individual (subj_num and time_point) mean ERP amplitude for different components
# N1 sensitivity to visual quantity 3 - 1 (= far - close) ----
indi_n1_sensitivity <- data_erp_all %>%
  filter(component == "n1") %>%
  group_by(subj_num, time_point, KL.cat, cardinal) %>%
  summarise(cardinal.mean.amp = mean(amp)) %>%
  summarise(n1.mean.sensitivity.3_1 = (cardinal.mean.amp[cardinal==3]) - (cardinal.mean.amp[cardinal==1]))

## combine erp and beh info ----
bbcor_n1_beh <- full_join(indi_n1_sensitivity, beh_data.raw) %>%
  mutate(subj_num = factor(subj_num),
         time_point = factor(time_point),
         KL.cat = factor(KL.cat)) %>%
  ungroup() %>%
  mutate(WM.c = as.vector(scale(WM, center = TRUE, scale=TRUE)),
         CONFLICT.c = as.vector(scale(CONFLICT, center = TRUE, scale=TRUE)),
         INHIBIT.c = as.vector(scale(INHIBIT, center = TRUE, scale=TRUE)),
         VOCAB.c = as.vector(scale(VOCAB, center = TRUE, scale=TRUE))) 

corr.n1sen.inhib <- bbcor_n1_beh %>%
  drop_na(n1.mean.sensitivity.3_1, INHIBIT)

corr.n1sen.wm <- bbcor_n1_beh %>%
  drop_na(n1.mean.sensitivity.3_1, WM)

corr.n1sen.conflict <- bbcor_n1_beh %>%
  drop_na(n1.mean.sensitivity.3_1, CONFLICT)

## N1 & EF correlation ----

cor.test(corr.n1sen.inhib$n1.mean.sensitivity.3_1, corr.n1sen.inhib$INHIBIT, method = "pearson")
cor.test(corr.n1sen.wm$n1.mean.sensitivity.3_1, corr.n1sen.wm$WM, method = "pearson")
cor.test(corr.n1sen.conflict$n1.mean.sensitivity.3_1, corr.n1sen.conflict$CONFLICT, method = "pearson")

pvals <- cor.test(corr.n1sen.inhib$n1.mean.sensitivity.3_1, corr.n1sen.inhib$INHIBIT, method = "pearson")$p.value
pvals <- c(pvals, cor.test(corr.n1sen.wm$n1.mean.sensitivity.3_1, corr.n1sen.wm$WM, method = "pearson")$p.value)
pvals <- c(pvals, cor.test(corr.n1sen.conflict$n1.mean.sensitivity.3_1, corr.n1sen.conflict$CONFLICT, method = "pearson")$p.value)

# Perform FDR correction
p.adjust(pvals, method = "fdr")
# write.csv(corr.n1sen.inhib, "./../corr.n1sen.inhib_0413.csv")



## N1 & EF regression ----

n1_model <- lm(n1.mean.sensitivity.3_1~WM.c+CONFLICT.c+INHIBIT.c, data = bbcor_n1_beh)
Anova(n1_model, type="III")
summary(n1_model)


ggplot(data = bbcor_n1_beh, aes(x=INHIBIT, y=n1.mean.sensitivity.3_1)) +
  geom_point() +
  geom_smooth(method="lm",se = F) +
  labs(x="Inhibition score", y="N1 sensitivity to visual number array") +
  xlim(0.5, 1)
  

# N2 distance effect ----
## N2 exact distance effect ----
indi_n2_exact.dist <- data_erp_all %>%
  filter(component == "n2") %>%
  group_by(subj_num, time_point, KL.cat, distance) %>%
  summarise(exact.dist.mean.amp = mean(amp)) %>%
  summarise(n2.exact.dist.2_1 = (exact.dist.mean.amp[distance==2]) - (exact.dist.mean.amp[distance==1]))

## N2 approximate distance effect ----
indi_n2_approx.dist <- data_erp_all %>%
  filter(component == "n2") %>%
  group_by(subj_num, time_point, KL.cat, ratio) %>%
  summarise(approx.dist.mean.amp = mean(amp)) %>%
  summarise(n2.approx.dist.2_1 = (approx.dist.mean.amp[ratio=="far"]) - (approx.dist.mean.amp[ratio=="close"]))

# combine erp and beh info ----
## N2 exact distance effect ----
bbcor_n2.exact.dist_beh <- full_join(indi_n2_exact.dist, beh_data.raw) %>%
  mutate(subj_num = factor(subj_num),
         time_point = factor(time_point),
         KL.cat = factor(KL.cat)) %>%
  ungroup() %>%
  mutate(WM.c = as.vector(scale(WM, center = TRUE, scale=TRUE)),
         CONFLICT.c = as.vector(scale(CONFLICT, center = TRUE, scale=TRUE)),
         INHIBIT.c = as.vector(scale(INHIBIT, center = TRUE, scale=TRUE)),
         VOCAB.c = as.vector(scale(VOCAB, center = TRUE, scale=TRUE))) 


### N2 exact & EF correlation ----
corr.n2exact.inhib <- bbcor_n2.exact.dist_beh %>%
  drop_na(n2.exact.dist.2_1, INHIBIT)

corr.n2exact.wm <- bbcor_n2.exact.dist_beh %>%
  drop_na(n2.exact.dist.2_1, WM)

corr.n2exact.conflict <- bbcor_n2.exact.dist_beh %>%
  drop_na(n2.exact.dist.2_1, CONFLICT)

cor.test(corr.n2exact.inhib$n2.exact.dist.2_1, corr.n2exact.inhib$INHIBIT, method = "pearson")
cor.test(corr.n2exact.wm$n2.exact.dist.2_1, corr.n2exact.wm$WM, method = "pearson")
cor.test(corr.n2exact.conflict$n2.exact.dist.2_1, corr.n2exact.conflict$CONFLICT, method = "pearson")

pvals <- cor.test(corr.n2exact.inhib$n2.exact.dist.2_1, corr.n2exact.inhib$INHIBIT, method = "pearson")$p.value
pvals <- c(pvals, cor.test(corr.n2exact.wm$n2.exact.dist.2_1, corr.n2exact.wm$WM, method = "pearson")$p.value)
pvals <- c(pvals, cor.test(corr.n2exact.conflict$n2.exact.dist.2_1, corr.n2exact.conflict$CONFLICT, method = "pearson")$p.value)

# Perform FDR correction
p.adjust(pvals, method = "fdr")

### N2 exact & EF regression ----

n2exact_model <- lm(n2.exact.dist.2_1~WM.c+CONFLICT.c+INHIBIT.c, data = bbcor_n2.exact.dist_beh)
Anova(n2exact_model, type="III")
summary(n2exact_model)



## N2 approximate distance effect ----
bbcor_n2.approx.dist_beh <- full_join(indi_n2_approx.dist, beh_data.raw) %>%
  mutate(subj_num = factor(subj_num),
         time_point = factor(time_point),
         KL.cat = factor(KL.cat)) %>%
  ungroup() %>%
  mutate(WM.c = as.vector(scale(WM, center = TRUE, scale=TRUE)),
         CONFLICT.c = as.vector(scale(CONFLICT, center = TRUE, scale=TRUE)),
         INHIBIT.c = as.vector(scale(INHIBIT, center = TRUE, scale=TRUE)),
         VOCAB.c = as.vector(scale(VOCAB, center = TRUE, scale=TRUE))) 


### N2 approximate & EF correlation ----
corr.n2approx.inhib <- bbcor_n2.approx.dist_beh %>%
  drop_na(n2.approx.dist.2_1, INHIBIT)

corr.n2approx.wm <- bbcor_n2.approx.dist_beh %>%
  drop_na(n2.approx.dist.2_1, WM)

corr.n2approx.conflict <- bbcor_n2.approx.dist_beh %>%
  drop_na(n2.approx.dist.2_1, CONFLICT)

cor.test(corr.n2approx.inhib$n2.approx.dist.2_1, corr.n2approx.inhib$INHIBIT, method = "pearson")
cor.test(corr.n2approx.wm$n2.approx.dist.2_1, corr.n2approx.wm$WM, method = "pearson")
cor.test(corr.n2approx.conflict$n2.approx.dist.2_1, corr.n2approx.conflict$CONFLICT, method = "pearson")

pvals <- cor.test(corr.n2approx.inhib$n2.approx.dist.2_1, corr.n2approx.inhib$INHIBIT, method = "pearson")$p.value
pvals <- c(pvals, cor.test(corr.n2approx.wm$n2.approx.dist.2_1, corr.n2approx.wm$WM, method = "pearson")$p.value)
pvals <- c(pvals, cor.test(corr.n2approx.conflict$n2.approx.dist.2_1, corr.n2approx.conflict$CONFLICT, method = "pearson")$p.value)

# Perform FDR correction
p.adjust(pvals, method = "fdr")

### N2 approximate & EF regression ----

n2approx_model <- lm(n2.approx.dist.2_1~WM.c+CONFLICT.c+INHIBIT.c, data = bbcor_n2.approx.dist_beh)
Anova(n2approx_model, type="III")
summary(n2approx_model)







# --------
# combine erp and beh info
bbcor_n2_beh <- full_join(indi_n2_sensitivity, beh_data.raw) %>%
  mutate(subj_num = factor(subj_num),
         time_point = factor(time_point),
         KL.cat = factor(KL.cat)) %>%
  ungroup() %>%
  mutate(WM.c = as.vector(scale(WM, center = TRUE, scale=TRUE)),
         CONFLICT.c = as.vector(scale(CONFLICT, center = TRUE, scale=TRUE)),
         INHIBIT.c = as.vector(scale(INHIBIT, center = TRUE, scale=TRUE)),
         VOCAB.c = as.vector(scale(VOCAB, center = TRUE, scale=TRUE))) %>%
  drop_na(mean.sensitivity)

cor.test(bbcor_n2_beh$mean.sensitivity, bbcor_n2_beh$INHIBIT, method = "pearson")


mymodel <- lm(mean.sensitivity ~ INHIBIT.c + WM.c + CONFLICT.c, 
                data=bbcor_n2_beh)
summary(mymodel)

coes <- summary(mymodel)$coefficients

ggplot(data=bbcor_n2_beh, aes(x=INHIBIT.c, y=mean.sensitivity)) + 
  geom_point() +
  geom_abline(intercept = coes[1], slope = coes[2]) +
  labs(x="Inhibition Score", y="N1 Sensitivity to Visual Quantities")



# N2 sensitivity to exact numerical distance (distance 1 minus distance 2)
indi_n2_sensitivity <- data_erp_all %>%
  filter(component == "n2") %>%
  mutate(KL.cat = if_else(KL.cont < 5, "SS", "CP")) %>%
  group_by(subj_num, time_point, KL.cat, distance) %>%
  summarise(distance.mean.amp = mean(amp, na.rm = T)) %>%
  group_by(subj_num, time_point, KL.cat) %>%
  summarise(mean.sensitivity = 
              (distance.mean.amp[distance=="1"]) - 
              (distance.mean.amp[distance=="2"]))


# N2 sensitivity to EF load (visual quantity 2 (cond 2 and 3) - visual quantity of 1 and 3 (cond 1 4 5 6))
indi_n2_sensitivity2 <- data_erp_all %>%
  filter(component == "n2") %>%
  mutate(KL.cat = if_else(KL.cont < 5, "SS", "CP"),
         EFload = if_else(cond %in% c(2, 3), "high", "low")) %>%
  group_by(subj_num, time_point, KL.cat, EFload) %>%
  summarise(EFload.mean.amp = mean(amp, na.rm = T)) %>%
  group_by(subj_num, time_point, KL.cat) %>%
  summarise(mean.sensitivity = 
              (EFload.mean.amp[EFload == "high"]) - 
              (EFload.mean.amp[EFload == "low"]))


bbcor_n2_beh2 <- full_join(indi_n2_sensitivity2, beh_data.raw) %>%
  mutate(subj_num = factor(subj_num),
         time_point = factor(time_point),
         KL.cat = factor(KL.cat)) %>%
  ungroup() %>%
  mutate(WM.c = as.vector(scale(WM, center = TRUE, scale=TRUE)),
         CONFLICT.c = as.vector(scale(CONFLICT, center = TRUE, scale=TRUE)),
         INHIBIT.c = as.vector(scale(INHIBIT, center = TRUE, scale=TRUE)),
         VOCAB.c = as.vector(scale(VOCAB, center = TRUE, scale=TRUE))) %>%
  drop_na(mean.sensitivity)


mymodel <- lm(mean.sensitivity ~ INHIBIT.c + WM.c + CONFLICT.c, 
              data=bbcor_n2_beh2)
summary(mymodel)
coes <- summary(mymodel)$coefficients

ggplot(data=bbcor_n2_beh2, aes(x=INHIBIT.c, y=mean.sensitivity)) + 
  geom_point() +
  geom_abline(intercept = coes[1], slope = coes[2]) +
  labs(x="Inhibition Score", y="N1 Sensitivity to Visual Quantities")