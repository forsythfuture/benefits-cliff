##################################################################################################################################

# this script performs an analysis on the 2019 unbanked NC population by race/ethnicity

##################################################################################################################################

library(srvyr)
library(tidyverse)
library(tidycensus)

# filter for total population - banked and unbanked
total_data <- hh %>%
  filter(hunbnk == 1 | hunbnk == 2)

# create survey object
fdic_srvdata_total <- as_survey_rep(total_data, weights = h, repweights = repwgt1:repwgt160, type = "JKn", scale = 0.025, 
                              rscales = rep(1,160), combined.weights = TRUE)

# totals by race/ethnicity for population
total_test <- fdic_srvdata_total %>%
  group_by(`Race Ethnicity`) %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96)

# filter out 'Other Race' to match outputs below for proportions
total_test <- total_test[c(1:2,4),]


##################################################################################################################################

# filter for unbanked
unbanked_data <- hh %>%
  filter(hunbnk == 1)

# create survey object
fdic_srvdata_unbanked <- as_survey_rep(unbanked_data, weights = h, repweights = repwgt1:repwgt160, type = "JKn", scale = 0.025, 
                              rscales = rep(1,160), combined.weights = TRUE)

# totals by race/ethnicity for unbanked population
unbanked_total_test <- fdic_srvdata_unbanked %>%
  group_by(`Race Ethnicity`) %>%
  summarise(total = survey_total()) %>%
  mutate(total_cv = total_se/total * 100) %>%
  mutate(total_moe = total_se * 1.96)

# output proportions by race/ethnicity
output_unbanked_total_test <- unbanked_total_test %>%
  select(`Race Ethnicity`, total, total_moe) %>%
  mutate(prop = paste0(format(round(total / total_test$total * 100, 1), nsmall = 1), "%"),
         `Proportion MOE` = paste0(format(round(moe_prop(total, total_test$total, total_moe, 
                                                         total_test$total_moe) * 100, 1), nsmall = 1), "%"),
         Proportion = paste0(prop, " +/- ", `Proportion MOE`)
  ) %>%
  select(-total, -total_moe, -`Proportion MOE`, -prop)
