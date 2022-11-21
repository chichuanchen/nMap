# Set up -----
library("tidyverse")
library("readxl")
library("lme4")
library("lmerTest")
library("lattice") # used for visual inspection of normal distribution of residuals
library("car") # leveneTest

rm(list = ls())
load("../tidied/erp_sample_tidied.RData")

# Specify data type for subsequent analysis -----
data_sample$subj_num <- as.factor(data_sample$subj_num)
data_sample$ratio <- as.factor(data_sample$ratio)
data_sample$time_point <- as.factor(data_sample$time_point)


# FIT LME MODEL -----

## P2p -----
model.p2p <- lmerTest::lmer(erp.p2p ~ ratio * time_point + (1|subj_num),
                            data = data_sample, REML = T)

model.p2p.KL <- lmerTest::lmer(erp.p2p ~ ratio + time_point + KL.cont + (1|subj_num),
                               data = data_sample, REML = T)
### test model assumption -----
plot(resid(model.p2p), data_sample$erp.p2p) # Linearity (visual inspection)
qqmath(model.p2p) # Normal distribution of residuals (visual inspection for sample N > 5000)
leveneTest(residuals(model.p2p) ~ data_sample$ratio) # Homoscedasticity

### read model output -----
anova(model.p2p)
anova(model.p2p.KL)

## N2 -----
model.n2 <- lmerTest::lmer(erp.n2 ~ ratio + time_point + (1|subj_num),
                           data = data_sample, REML = T)

model.n2.KL <- lmerTest::lmer(erp.n2 ~ ratio + time_point + KL.cont + (1|subj_num),
                              data = data_sample, REML = T)
### test model assumption -----
plot(resid(model.n2), data_sample$erp.n2) # Linearity (visual inspection)
qqmath(model.n2) # Normal distribution of residuals (visual inspection for sample N > 5000)
leveneTest(residuals(model.n2) ~ data_sample$ratio) # Homoscedasticity

### read model output -----
anova(model.n2)
anova(model.n2.KL)
