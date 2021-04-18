#----------------------Script Header----------------------------------####
# Date:           10/04/2020
# Author:         Rong Ding
# Filename:       session4.R
# Description:    Code to try out LMER models to analyse how inter-stimuli 
#                 intervals (ISIs) and semantic relationships between words
#                 influence response times (RTs) to target words in a
#                 semantic priming experiment. 
#---------------------------------------------------------------------###

#----------------------Change Log------------------------------------####
# Date:           18/04/2020
# Change by:      Rong Ding
# Filename:       session4.R
# Change:         Adding script headers to make the code more comprehensible
#---------------------------------------------------------------------###

#----------------------Library Declarations--------------------------####

library(tidyverse)
library(ggpubr)
library(rstatix)
library(lme4)
library(arm)

# Download a package if it's not installed yet
if (!require(package, character.only=T, quietly=T)) {
  install.packages(package)
  library(package, character.only = T)
}
#---------------------------------------------------------------------###

#----------------------Parameters------------------------------------####

# alter the directory and the datafile name here
directory = 'C:/Users/lenovo/Desktop/PhD/course regis/Comp Psycholing'
filename = 'data_finalised_naming.csv'

#---------------------------------------------------------------------###


#----------------------Data Prep-------------------------------------####

setwd(directory)
data <- read.csv(file = filename)

# double-check data format
head(data)

# mark categorical variables
data$isi=as.factor(data$isi)

#---------------------------------------------------------------------###

#----------------------Model Implementation--------------------------####

# basic regression model without random structure
result_basic <-  lm(meanRT ~ isi + condition + isi:condition, data = data)
summary(result1)

# LMER with random intercepts only, to control for variance of stimulus 
# word identity
result_intrcpt <- lmer(meanRT ~ isi + condition + isi:condition # predictor vars 
                       + (1|target) + (1|prime), # random structure
                      data = data)
summary(result_intrcpt)


# LMER model with random intercepts but also random slopes, plus stats testing
results_final <- lmerTest::lmer(meanRT ~ isi*condition 
                + (1 + isi | target) + (1 | prime),
                data = data)
summary(results_final)

#---------------------------------------------------------------------###