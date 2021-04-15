
rm(list=ls())
setwd("~/Desktop")

library(haven)
library(ggplot2)
library(tidyverse)
library(rdrobust)
library("lfe")

# resource from here: https://evalf20.classes.andrewheiss.com/example/rdd-fuzzy/

medicare <- read.csv("medicare.csv")
medicare$age <- as.integer(medicare$age)
medicare$hasmedicare <- ifelse(medicare$covtype == "Medicare", 1, 0)
medicare$conservative <- ifelse(medicare$ideology == "Conservative", 1, 0)
medicare$gop <- ifelse(medicare$party == "Republican", 1, 0)
medicare$white <- ifelse(medicare$race == "White", 1, 0)

ggplot(medicare, aes(x = age, y = hasmedicare, color = hasmedicare)) +  
  geom_point(size = 0.5, alpha = 0.5,
             position = position_jitter(width = 0, height = .25, seed = 1234)) +
  geom_vline(xintercept = 65) + 
  scale_x_continuous(breaks = c(18,35,50,65,80))

# Trying to see compliance by group
medicare %>%
  group_by(hasmedicare, recage2 == "65+") %>%
  summarise(count =  n()) %>%
  group_by(hasmedicare) %>%
  mutate(prop = count / sum(count))

# Age was screwed up so making it normal
medicare<- medicare %>%
  mutate(correctage =  age - 1 + 18) %>%
  filter(correctage != 95) #because this was an error code
# Checking to make sure this is correct, looks good
table(medicare$correctage, medicare$recage2)


# Need to make an instrument for whether there was tutoring or not
medicare_centered <- medicare %>%
  mutate(age_centered = correctage - 65,
         eligible = correctage >= 65)

# The variable that I want to use for outcome so far is whether they support Obamacare -> theory being that they support other people getting government healthcare as well
medicare_centered$supportaca <- 
  case_when(
    medicare_centered$aca == "Very favorable" |
      medicare_centered$aca == "Somewhat favorable" ~ 1,
      medicare_centered$aca == "Very unfavorable" |
      medicare_centered$aca == "Somewhat unfavorable" |
      medicare_centered$aca ==  "Don't know" |
      medicare_centered$aca == "Refused" ~ 0)
table(medicare_centered$supportaca)

# Need to run an IV using medicare as the instrument
library("estimatr")

model_fuzzy  <- iv_robust(
  supportaca ~ age_centered + hasmedicare + gop  + conservative| age_centered + eligible + gop + conservative,
  data = filter(medicare_centered, age_centered >= -5 & age_centered <= 5))
tidy(model_fuzzy) #looks like there's no effect of having medicare on supporting aca

?iv_robust
# Q6a: Do you favor/disfavor: Expanding government financial help for people who buy their own health insurance
medicare_centered$favorexpansion <- ifelse(medicare_centered$q6a == "Favor", 1, 0)

model_fuzzy2  <- iv_robust(
  favorexpansion ~ age_centered + hasmedicare | age_centered + eligible,
  data = filter(medicare_centered, age_centered >= -5 & age_centered <= 5))
tidy(model_fuzzy2)

# Q6b: Providing more federal funding to states that have not yet expanded Medicaid if they expand their Medicaid program to cover more low-income adults
medicare_centered$fedfunding <- ifelse(medicare_centered$q6b == "Favor", 1, 0)

model_fuzzy3  <- iv_robust(
  fedfunding ~ age_centered + hasmedicare + gop + conservative + white| age_centered + eligible + gop + conservative  + white,
  data = filter(medicare_centered, age_centered >= -5 & age_centered <= 5))
tidy(model_fuzzy3)

#  Q7: Generally speaking, how often do you think our health care system treats people unfairly based on their race or ethnic background?

medicare_centered$discrim <- case_when(
  medicare_centered$q7 == "Very often" |
    medicare_centered$q7 == "Somewhat often" ~ 1,
  medicare_centered$q7 == "Not too often" |
    medicare_centered$q7 == "Never" ~ 0)

model_fuzzy4  <- iv_robust(
  discrim ~ age_centered + hasmedicare + gop + conservative + white| age_centered + eligible + gop + conservative + white,
  data = filter(medicare_centered, age_centered >= -5 & age_centered <= 5))
tidy(model_fuzzy4)
