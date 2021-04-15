rm(list=ls())
setwd("~/Desktop")

library(haven)
library(ggplot2)
library(tidyverse)
library(rdrobust)
library("lfe")

gss <- read_sav("GSS.sav")

# Using this page: http://congressdata.joshuamccrain.com/regression_discontinuity.html

gss$SSI <- ifelse(gss$AGE >= 65, "Eligible", "Ineligible")

gss$white <- ifelse(gss$RACE == 1, 1, 0)
gss$black <- ifelse(gss$RACE == 2, 1, 0)
gss$gop <- ifelse(gss$PARTYID == 4 | gss$PARTYID == 5 | gss$PARTYID == 6, 1, 0)
gss$male <- ifelse(gss$SEX == 1, 1, 0)
gss$ideo <- ifelse(gss$POLVIEWS == 8 | gss$POLVIEWS == 9 | gss$POLVIEWS == 0, NA, gss$POLVIEWS)

## VISUALIZATIONS 

# A. Makes people work less than they would if there wasn't a welfare system.
gss %>%
  filter(AGE >= 60 & AGE <= 70) %>%
  ggplot(aes(x = AGE, y = WELFARE1, group = SSI, color = SSI)) +
  geom_point() +
  geom_smooth(method = "lm")

# B. Helps people get on their feet when facing difficult situations such as unemployment, a divorce or a death in the family.
gss %>%
  filter(AGE >= 60 & AGE <= 70) %>%
  ggplot(aes(x = AGE, y = WELFARE2, group = SSI, color = SSI)) +
  geom_point() +
  geom_smooth(method = "lm")

# D. Helps keep people's marriage together in times of financial problems?
gss %>%
  filter(AGE >= 60 & AGE <= 70) %>%
  ggplot(aes(x = AGE, y = WELFARE4, group = SSI, color = SSI)) +
  geom_point() +
  geom_smooth(method = "lm")

# E. Helps to prevent hunger and starvation.
gss %>%
  filter(AGE >= 60 & AGE <= 70) %>%
  ggplot(aes(x = AGE, y = WELFARE5, group = SSI, color = SSI)) +
  geom_point() +
  geom_smooth(method = "lm")

# NATFARE
gss %>%
  filter(AGE >= 60 & AGE <= 70) %>%
  ggplot(aes(x = AGE, y = NATFARE, group = SSI, color = SSI)) +
  geom_point() +
  geom_smooth(method = "lm")

#NATSOC
gss %>%
  filter(AGE >= 60 & AGE <= 70) %>%
  ggplot(aes(x = AGE, y = NATSOC, group = SSI, color = SSI)) +
  geom_point() +
  geom_smooth(method = "lm")

# Confidence in federal government
gss %>%
  filter(AGE >= 60 & AGE <= 70) %>%
  ggplot(aes(x = AGE, y = CONFED, group = SSI, color = SSI)) +
  geom_point() +
  geom_smooth(method = "lm")

# Government pays enough attention to whites
gss %>%
  filter(AGE >= 60 & AGE <= 70) %>%
  ggplot(aes(x = AGE, y = WHTGOVT, group = SSI, color = SSI)) +
  geom_point() +
  geom_smooth(method = "lm")

# Government does enough to help the poor
gss %>%
  filter(AGE >= 60 & AGE <= 70) %>%
  ggplot(aes(x = AGE, y = HELPPOOR, group = SSI, color = SSI)) +
  geom_point() +
  geom_smooth(method = "lm")

## ANALYSIS

# A. Makes people work less than they would if there wasn't a welfare system.

### RDD- create variables for whether eligible, age margin, interaction between them
gss$SSIeligible <- ifelse(gss$SSI == "Eligible", 1, 0)
gss$agemargin <- gss$AGE - 65
gss$age_interact <- gss$SSIeligible * gss$agemargin

### Bandwidth selection 
gss_full <- gss %>%
  filter(!is.na(WELFARE1)) %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(SSIeligible))
gss_full$cutoff <- NULL
gss_full$bandwidth <- NULL
gss_full$kw <- NULL

bws <- rdbwselect(gss_full$WELFARE1, gss_full$agemargin, p = 1, c = 0,
                  kernel = "tri", bwselect = "mserd",
                  covs = gss_full$gop, gss_full$white)

gss_full$bandwidth <- bws$bws[1]
gss_full$cutoff <- 0
gss_full$kw <- 1 - (abs(gss_full$cutoff - gss_full$agemargin))/gss_full$bandwidth
gss_full$kw[gss_full$agemargin > (gss_full$cutoff  + gss_full$bandwidth) | 
              gss_full$agemargin < (gss_full$cutoff - gss_full$bandwidth)] <- 0

### Estimation
model1 <- felm(gss_full$WELFARE1[gss_full$kw > 0] ~ agemargin  + SSIeligible + age_interact + gop + white + male + as.factor(REGION)|YEAR|0|0, data = gss_full[gss_full$kw > 0,], weights = gss_full$kw[gss_full$kw > 0])

summary(model1)

# B. Helps people get on their feet when facing difficult situations such as unemployment, a divorce or a death in the family.

### Bandwidth selection 
gss_full <- gss %>%
  filter(!is.na(WELFARE2)) %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(SSIeligible))
gss_full$cutoff <- NULL
gss_full$bandwidth <- NULL
gss_full$kw <- NULL

bws <- rdbwselect(gss_full$WELFARE2, gss_full$agemargin, p = 1, c = 0,
                  kernel = "tri", bwselect = "mserd",
                  covs = gss_full$gop, gss_full$white)

gss_full$bandwidth <- bws$bws[1]
gss_full$cutoff <- 0
gss_full$kw <- 1 - (abs(gss_full$cutoff - gss_full$agemargin))/gss_full$bandwidth
gss_full$kw[gss_full$agemargin > (gss_full$cutoff  + gss_full$bandwidth) | 
              gss_full$agemargin < (gss_full$cutoff - gss_full$bandwidth)] <- 0

### Estimation
getonfeet <- felm(gss_full$WELFARE2[gss_full$kw > 0] ~ agemargin  + SSIeligible + age_interact + gop + white + male + as.factor(REGION)|YEAR|0|0, data = gss_full[gss_full$kw > 0,], weights = gss_full$kw[gss_full$kw > 0])

summary(getonfeet)

# D. Helps keep people's marriage together in times of financial problems?

### Bandwidth selection 
gss_full <- gss %>%
  filter(!is.na(WELFARE1)) %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(SSIeligible))
gss_full$cutoff <- NULL
gss_full$bandwidth <- NULL
gss_full$kw <- NULL

bws <- rdbwselect(gss_full$WELFARE4, gss_full$agemargin, p = 1, c = 0,
                  kernel = "tri", bwselect = "mserd",
                  covs = gss_full$gop, gss_full$white)

gss_full$bandwidth <- bws$bws[1]
gss_full$cutoff <- 0
gss_full$kw <- 1 - (abs(gss_full$cutoff - gss_full$agemargin))/gss_full$bandwidth
gss_full$kw[gss_full$agemargin > (gss_full$cutoff  + gss_full$bandwidth) | 
              gss_full$agemargin < (gss_full$cutoff - gss_full$bandwidth)] <- 0

### Estimation
marriagetogether <- felm(gss_full$WELFARE4[gss_full$kw > 0] ~ agemargin  + SSIeligible + age_interact + gop + white + male|YEAR|0|0, data = gss_full[gss_full$kw > 0,], weights = gss_full$kw[gss_full$kw > 0])

summary(marriagetogether)

# E. Helps to prevent hunger and starvation.
### Bandwidth selection 
gss_full <- gss %>%
  filter(!is.na(WELFARE5)) %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(SSIeligible))
gss_full$cutoff <- NULL
gss_full$bandwidth <- NULL
gss_full$kw <- NULL

bws <- rdbwselect(gss_full$WELFARE5, gss_full$agemargin, p = 1, c = 0,
                  kernel = "tri", bwselect = "mserd",
                  covs = gss_full$gop, gss_full$white)

gss_full$bandwidth <- bws$bws[1]
gss_full$cutoff <- 0
gss_full$kw <- 1 - (abs(gss_full$cutoff - gss_full$agemargin))/gss_full$bandwidth
gss_full$kw[gss_full$agemargin > (gss_full$cutoff  + gss_full$bandwidth) | 
              gss_full$agemargin < (gss_full$cutoff - gss_full$bandwidth)] <- 0

### Estimation
hunger <- felm(gss_full$WELFARE5[gss_full$kw > 0] ~ agemargin  + SSIeligible + age_interact + gop + white  + male|YEAR|0|0, data = gss_full[gss_full$kw > 0,], weights = gss_full$kw[gss_full$kw > 0])

summary(hunger)

# "We are faced with many problems in this country, none of which can be solved easily or inexpensively. I'm going to name some of these problems, and for each one I'd like you to tell me whether you think we're spending too much money on it, too little money, or about the right amount." "Assistance to the poor"

gss_full <- gss %>%
  filter(!is.na(NATFARE)) %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(SSIeligible))
gss_full$cutoff <- NULL
gss_full$bandwidth <- NULL
gss_full$kw <- NULL

bws <- rdbwselect(gss_full$NATFARE, gss_full$agemargin, p = 1, c = 0,
                  kernel = "tri", bwselect = "mserd",
                  covs = gss_full$gop, gss_full$white)

gss_full$bandwidth <- bws$bws[1]
gss_full$cutoff <- 0
gss_full$kw <- 1 - (abs(gss_full$cutoff - gss_full$agemargin))/gss_full$bandwidth
gss_full$kw[gss_full$agemargin > (gss_full$cutoff  + gss_full$bandwidth) | 
              gss_full$agemargin < (gss_full$cutoff - gss_full$bandwidth)] <- 0

### Estimation
assistpoor <- felm(gss_full$NATFARE[gss_full$kw > 0] ~ agemargin  + SSIeligible + age_interact + gop + white + male + ideo|YEAR|0|0, data = gss_full[gss_full$kw > 0,], weights = gss_full$kw[gss_full$kw > 0])

summary(assistpoor)

# Confidence in federal government
gss_full <- gss %>%
  filter(!is.na(CONFED)) %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(SSIeligible))
gss_full$cutoff <- NULL
gss_full$bandwidth <- NULL
gss_full$kw <- NULL

bws <- rdbwselect(gss_full$CONFED, gss_full$agemargin, p = 1, c = 0,
                  kernel = "tri", bwselect = "mserd",
                  covs = gss_full$gop, gss_full$white)

gss_full$bandwidth <- bws$bws[1]
gss_full$cutoff <- 0
gss_full$kw <- 1 - (abs(gss_full$cutoff - gss_full$agemargin))/gss_full$bandwidth
gss_full$kw[gss_full$agemargin > (gss_full$cutoff  + gss_full$bandwidth) | 
              gss_full$agemargin < (gss_full$cutoff - gss_full$bandwidth)] <- 0

### Estimation
confidence <- felm(gss_full$CONFED[gss_full$kw > 0] ~ agemargin  + SSIeligible + age_interact + gop +  white + male+ ideo |YEAR|0|0, data = gss_full[gss_full$kw > 0,], weights = gss_full$kw[gss_full$kw > 0])

summary(confidence)

# Government pays enough attention to whites
gss_full <- gss %>%
  filter(!is.na(WHTGOVT)) %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(SSIeligible))
gss_full$cutoff <- NULL
gss_full$bandwidth <- NULL
gss_full$kw <- NULL

bws <- rdbwselect(gss_full$WHTGOVT, gss_full$agemargin, p = 1, c = 0,
                  kernel = "tri", bwselect = "mserd",
                  covs = gss_full$gop, gss_full$white)

gss_full$bandwidth <- bws$bws[1]
gss_full$cutoff <- 0
gss_full$kw <- 1 - (abs(gss_full$cutoff - gss_full$agemargin))/gss_full$bandwidth
gss_full$kw[gss_full$agemargin > (gss_full$cutoff  + gss_full$bandwidth) | 
              gss_full$agemargin < (gss_full$cutoff - gss_full$bandwidth)] <- 0

### Estimation
whites <- felm(gss_full$WHTGOVT[gss_full$kw > 0] ~ agemargin  + SSIeligible + age_interact + gop + white + male + ideo|YEAR|0|0, data = gss_full[gss_full$kw > 0,], weights = gss_full$kw[gss_full$kw > 0])

summary(whites)

# Government does enough to help the poor
gss_full <- gss %>%
  filter(!is.na(HELPPOOR)) %>%
  filter(!is.na(AGE)) %>%
  filter(!is.na(SSIeligible))
gss_full$cutoff <- NULL
gss_full$bandwidth <- NULL
gss_full$kw <- NULL

bws <- rdbwselect(gss_full$HELPPOOR, gss_full$agemargin, p = 1, c = 0,
                  kernel = "tri", bwselect = "mserd",
                  covs = gss_full$gop, gss_full$white)

gss_full$bandwidth <- bws$bws[1]
gss_full$cutoff <- 0
gss_full$kw <- 1 - (abs(gss_full$cutoff - gss_full$agemargin))/gss_full$bandwidth
gss_full$kw[gss_full$agemargin > (gss_full$cutoff  + gss_full$bandwidth) | 
              gss_full$agemargin < (gss_full$cutoff - gss_full$bandwidth)] <- 0

### Estimation
helppoor <- felm(gss_full$HELPPOOR[gss_full$kw > 0] ~ agemargin  + SSIeligible + age_interact + gop + white + male + ideo|YEAR|0|0, data = gss_full[gss_full$kw > 0,], weights = gss_full$kw[gss_full$kw > 0])

summary(helppoor)

library(stargazer)
stargazer(assistpoor, confidence,whites, helppoor, type = "text", column.labels = c("assistpoor","confidence", "whites", "helppoor"))
