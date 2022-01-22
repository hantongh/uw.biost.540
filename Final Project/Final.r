setwd("C:/Users/second/Desktop/BIOST 540/Final")

library(reshape2)
library(dplyr)
library(VIM)
library(ggplot2)
library(wgeesel)
library(geepack)
library(multcomp)
library(doBy)
library(tidyverse)

### Read data
acl <- read.csv("acl_subset.csv")

### Turn into long format
dat <- acl[,-1]
dat.long <- melt(dat, id=c("id", "sex", "race", "ses", "age"))
dat.long$year <- 1986
dat.long$year[dat.long$variable=="w2"] <- 1989
dat.long$year[dat.long$variable=="w3"] <- 1994
dat.long$year[dat.long$variable=="w4"] <- 2002
dat.long$year[dat.long$variable=="w5"] <- 2011

dat.long <- dat.long %>% arrange(id,year)

### Exploratory analysis
# Demographic features
library(table1)
label(dat$age) <- "Baseline age"
label(dat$sex) <- "Sex"
label(dat$ses) <- "Socioeconomic status"
label(dat$w1) <- "Wave 1 (1986)"
label(dat$w2) <- "Wave 2 (1989)"
label(dat$w3) <- "Wave 3 (1994)"
label(dat$w4) <- "Wave 4 (2002)"
label(dat$w5) <- "Wave 5 (2011)"
dat$race <- factor(dat$race, levels = c("AA","W"), 
                         labels = c("African American", 
                                    "White American"))

table1(~ age + sex + ses + w1 + w2 + w3 + w4 + w5| race, data=dat,
       render.continuous=c(.="Mean (SD)"))


# Missing pattern
ids.miss <- unique(dat.long$id[is.na(dat.long$value)])
dat.long$missing <- "Complete"
dat.long$missing[dat.long$id %in% ids.miss] <- "Missing"

ggplot(dat.long %>% filter(!is.na(value)),
       aes(x = as.factor(value), fill = missing)) + 
  geom_bar(alpha =0.25) + 
  facet_grid(race ~ variable) +
  xlab("Functional Impairment (0=No, 1=Yes)") +
  theme(legend.position="bottom")

### Q2
dat.long$lag1y <- ylag(dat.long$id,dat.long$value,1)

# Remove ids with no ylag value
dat.long.avail <- dat.long %>%
  filter(!is.na(lag1y))

# Model
mod_avail <- geeglm(value ~ race*year + age + sex + ses + lag1y, id = id, 
                       data = dat.long.avail,
                       family=binomial(link="logit"))
summary(mod_avail)
inf <- glht(mod_avail)
mod_avail_ci <- confint(inf)[[9]] # 95% CI for all covariates
print(exp(mod_avail_ci))

# 95% CI for white american
lambda2 <- c(0, 0, 1, 0, 0, 0, 0, 0, 1)
exp(lambda2 %*% mod_avail$coefficients + 
      qnorm(c(0.025, 0.5, 0.975)) * c(summary(mod_avail)$coefficients$Std.err %*% lambda2))

### Q3
# Primary
mod_time <- geeglm(value ~ race*variable + age + sex + ses, id = id, 
                    data = dat.long,
                    family=binomial(link="logit"))
summary(mod_time)
inf2 <- glht(mod_time)
mod_time_ci <- confint(inf2)[[9]]
print(exp(mod_time_ci))


l1 <- c(0,0,0,0,0,0,0,0,0,0,1,0,0,0)
l2 <- c(0,0,0,0,0,0,0,0,0,0,0,1,0,0)
l3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,1,0)
l4 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1)

esticon(mod_time, rbind(l1,l2,l3,l4), joint.test = T)

# Sensitivity
library(mice)
library(lme4)
dat.sens <- dat.long[,c(1:7)]

pred_long <- make.predictorMatrix(dat.sens)
pred_long["value","id"] <- -2
imp_long <- mice(dat.sens, method = "2l.bin",
                 pred = pred_long, seed = 540,
                 maxit = 1, m = 20, print=F)
densityplot(imp_long)

fit.imp_long <- with(imp_long,
                     geeglm(value ~ race*variable + age + sex + ses, id = id, 
                            family=binomial(link="logit")))

imp_mod <- summary(pool(fit.imp_long))

l_race <- c(0,1,0,0,0,0,0,0,0,0,0,0,0,0)
exp(l_race %*% imp_mod$estimate + 
      qnorm(c(0.025, 0.5, 0.975)) * c(imp_mod$std.err %*% l_race))

exp(l1 %*% imp_mod$estimate + 
      qnorm(c(0.025, 0.5, 0.975)) * c(imp_mod$std.err %*% l1))
exp(l2 %*% imp_mod$estimate + 
      qnorm(c(0.025, 0.5, 0.975)) * c(imp_mod$std.err %*% l2))
exp(l3 %*% imp_mod$estimate + 
      qnorm(c(0.025, 0.5, 0.975)) * c(imp_mod$std.err %*% l3))
exp(l4 %*% imp_mod$estimate + 
      qnorm(c(0.025, 0.5, 0.975)) * c(imp_mod$std.err %*% l4))





