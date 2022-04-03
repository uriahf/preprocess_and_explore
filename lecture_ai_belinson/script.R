library(tidyverse)
library(regclass)
library(broom)
library(ggplot2)
library(naniar)

dat <- read.csv("biostats.csv") %>%
  mutate(overweight = bmi > 25) %>%
  mutate(sector = factor(sector, levels = c("general", "haredi", "arab")))
names(dat)

make_stars <- function(pval) {
  stars = ""
  if(pval <= 0.001)
    stars = "***"
  if(pval > 0.001 & pval <= 0.01)
    stars = "**"
  if(pval > 0.01 & pval <= 0.05)
    stars = "*"
  if(pval > 0.05 & pval <= 0.1)
    stars = "."
  stars
}


summary(dat)


model1 <- glm(covid ~ age, data = dat, family = "binomial")
tidy(model1, conf.int = T) %>%
  mutate(signif = sapply(p.value, function(x) make_stars(x))) %>%
  select(-std.error, -statistic)

model2 <- glm(covid ~ age + sector, data = dat, family = "binomial")
summary(model2)
tidy(model2)
VIF(model2)

model3 <- glm(covid ~ age + sector + height + weight, data = dat, family = "binomial")
VIF(model3)
tidy(model3)

model4 <- glm(covid ~ age + sector + height + weight, data = dat, family = "binomial")
VIF(model4)
tidy(model4) %>%
  mutate(signif = sapply(p.value, function(x) make_stars(x))) %>%
  select(-std.error, -statistic)


model5 <- glm(covid ~ age + sector + overweight, data = dat, family = "binomial")
VIF(model5)
tidy(model5, conf.int = T) %>%
  mutate(signif = sapply(p.value, function(x) make_stars(x))) %>%
  select(-std.error, -statistic)


model6 <- glm(covid ~ age + sector + overweight + is_immigrant, data = dat, family = "binomial")
VIF(model6)
tidy(model6, conf.int = T) %>%
  mutate(signif = sapply(p.value, function(x) make_stars(x))) %>%
  select(-std.error, -statistic)



