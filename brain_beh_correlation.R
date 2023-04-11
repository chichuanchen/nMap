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


# calculate individual (subj_num and time_point) mean ERP amplitude for different components
# N1 sensitivity to visual quantity 3 - 1 (= far - close)
indi_n1_sensitivity <- data_erp_all %>%
  filter(component == "n1") %>%
  mutate(KL.cat = if_else(KL.cont < 5, "SS", "CP")) %>%
  group_by(subj_num, time_point, KL.cat, cardinal) %>%
  summarise(cardinal.mean.amp = mean(amp, na.rm = T)) %>%
  group_by(subj_num, time_point, KL.cat) %>%
  summarise(n1.mean.sensitivity.3_1 = (cardinal.mean.amp[cardinal==3]) - (cardinal.mean.amp[cardinal==1]))

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

# combine erp and beh info
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

write.csv(corr.n1sen.inhib, "./../data/corr.n1sen.inhib.csv")

corr.n1sen.WM <- bbcor_n1_beh %>%
  drop_na(n1.mean.sensitivity.3_1, WM)

cor.test(corr.n1sen.inhib$n1.mean.sensitivity.3_1, corr.n1sen.inhib$INHIBIT, method = "pearson")
cor.test(corr.n1sen.WM$n1.mean.sensitivity.3_1, corr.n1sen.WM$WM, method = "pearson")


glimpse(bbcor_n1_beh)

bbcor_n1_beh.CP <- bbcor_n1_beh %>% filter(KL.cat == "CP")
bbcor_n1_beh.SS <- bbcor_n1_beh %>% filter(KL.cat == "SS")


ggplot(data = bbcor_n1_beh, aes(x=INHIBIT, y=n1.mean.sensitivity.3_1)) +
  geom_point() +
  geom_smooth(method="lm")
  

ggplot(data = bbcor_n1_beh.SS, aes(x=INHIBIT, y=n1.mean.sensitivity.3_1)) +
  geom_point() +
  geom_smooth(method="lm")

mymodel <- lmer(n1.mean.sensitivity.3_1 ~ INHIBIT.c + WM.c + CONFLICT.c + (1|time_point), 
                data=bbcor_n1_beh) # singular fit

mymodel <- lm(n1.mean.sensitivity.3_1 ~ INHIBIT.c, data=bbcor_n1_beh) 

mymodel <- lm(n1.mean.sensitivity.3_1 ~ INHIBIT.c + time_point , 
                data=bbcor_n1_beh)

summary(mymodel)

confint(mymodel, oldNames = FALSE)

mymodel.SS <- lm(mean.sensitivity ~ INHIBIT.c + WM.c + CONFLICT.c, 
              data=subset(bbcor_n1_beh, KL.cat == "SS"))
summary(mymodel.SS)

mymodel.CP <- lm(mean.sensitivity ~ INHIBIT.c + WM.c + CONFLICT.c, 
                 data=subset(bbcor_n1_beh, KL.cat == "CP"))
summary(mymodel.CP)

coef(mymodel)["(Intercept)"]
# fixef(mymodel)["(Intercept)"]

ggplot(data=bbcor_n1_beh, aes(x=INHIBIT.c, y=mean.sensitivity,color=KL.cat)) + 
  geom_point() +
  geom_smooth(method = "lm") +
  # geom_abline(intercept = coef(mymodel)["(Intercept)"], slope = coef(mymodel)["INHIBIT.c"]) +
  labs(x="Inhibition Score", y="N1 Sensitivity to Visual Quantities") +
  facet_grid(~time_point)



# 
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