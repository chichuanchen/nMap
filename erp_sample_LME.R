# Set up -----
library("tidyverse")
library("readxl")
library("lme4")
library("lmerTest")
library("lattice") # used for visual inspection of normal distribution of residuals
library("car") # leveneTest

rm(list = ls())
load("../data/ERP/tidied/erp_sample_tidied.RData")

# Define catogeorical KL
data_sample <- data_sample %>%
  mutate(KL.cat = if_else(KL.cont %in% c(1:4), "SS", "CP"))

# Specify data type for subsequent analysis -----
data_sample$subj_num <- as.factor(data_sample$subj_num)
data_sample$ratio <- factor(data_sample$ratio, levels = c("close", "med", "far"))
data_sample$time_point <- as.factor(data_sample$time_point)
data_sample$KL.cat <- as.factor(data_sample$KL.cat)


# FIT LME MODEL -----

## P2p -----
model.p2p <- lmerTest::lmer(erp.p2p ~ ratio * time_point + (1|subj_num/time_point),
                            data = data_sample, REML = T)

model.p2p.KL <- lmerTest::lmer(erp.p2p ~ ratio * KL.cat + time_point + (1|subj_num/time_point),
                               data = data_sample, REML = T)
### test model assumption -----
plot(resid(model.p2p.KL), data_sample$erp.p2p) # Linearity (visual inspection)
qqmath(model.p2p.KL) # Normal distribution of residuals (visual inspection for sample N > 5000)
leveneTest(residuals(model.p2p.KL) ~ data_sample$ratio) # Homoscedasticity

### read model output -----
anova(model.p2p)
anova(model.p2p.KL)

### plots-----
data_plot <- data_sample %>%
  group_by(KL.cat, ratio, subj_num, time_point) %>%
  summarise(ind.mean_erp.p2p = mean(erp.p2p, na.rm=T),
            ind.mean_erp.n2 = mean(erp.n2, na.rm=T)) %>%
  group_by(KL.cat, ratio, time_point) %>%
  summarise(mean_erp.p2p = mean(ind.mean_erp.p2p, na.rm=T),
            mean_erp.n2 = mean(ind.mean_erp.n2, na.rm=T)) 
  
ggplot(data_plot, aes(x=ratio, y=mean_erp.p2p, color = KL.cat, group=interaction(time_point, KL.cat))) +
  geom_point(aes(shape=time_point)) +
  geom_line()
  

## N2 -----
model.n2 <- lmerTest::lmer(erp.n2 ~ ratio + time_point + (1|subj_num/time_point),
                           data = data_sample, REML = T)

model.n2.KL <- lmerTest::lmer(erp.n2 ~ ratio * KL.cat + time_point+ (1|subj_num/time_point),
                              data = data_sample, REML = T)
### test model assumption -----
plot(resid(model.n2.KL), data_sample$erp.n2) # Linearity (visual inspection)
qqmath(model.n2.KL) # Normal distribution of residuals (visual inspection for sample N > 5000)
leveneTest(residuals(model.n2.KL) ~ data_sample$ratio) # Homoscedasticity

### read model output -----
anova(model.n2)
anova(model.n2.KL)

### plots -----
ggplot(data_plot, aes(x=ratio, y=mean_erp.n2, color = KL.cat, group=interaction(time_point, KL.cat))) +
  geom_point(aes(shape=time_point)) +
  geom_line()