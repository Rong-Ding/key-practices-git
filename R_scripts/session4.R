library(tidyverse)
library(ggpubr)
library(rstatix)
library(lme4)
library(arm)

setwd('C:/Users/lenovo/Desktop/PhD/course regis/Comp Psycholing')
data <- read.csv(file = 'data_finalised_naming.csv')
head(data)

# non-multilevel ones - fixed effects?
result <-  lm(meanRT ~ isi + condition + isi:condition, data = data)
summary(result)

# intercept model
result2 <- glm(meanRT ~ isi + condition + isi:condition, data = data)
display(result2)

# with random intercepts
result3 <- lmer(meanRT ~ isi + condition + isi:condition + (1|target) 
              + (1|prime),
              data = data)
summary(result3)
display(result3)

# with random intercepts but also random slopes

data$isi=as.factor(data$isi) # mark isi as categorical

results_RT <- lmerTest::lmer(meanRT ~ isi*condition 
                + (1 + isi | target) + (1 | prime), # random structure: condition highly correlated w/ target, r = .99 ??
                data = data)
summary(results_RT)


# might need to choose one of the ISI (50/1050 have identical cosine vals)
results_cos <- lmerTest::lmer(cosine ~ condition # isi doesn't matter in the case of cosine
                          + (1 + condition | target) + (1 | prime), # prime/prime_un highly correlated
                          data = data)
summary(results_cos)

# correlate RT w/ cosine

